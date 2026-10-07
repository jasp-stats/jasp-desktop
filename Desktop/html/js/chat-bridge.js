"use strict";

var aiBridge = null;
var currentSignals = null;
var streamHasContent = false;

// Message-log integrity:
// deep-chat rebuilds its entire chat view from the `history` property whenever
// a reactive style property is reassigned (theme / avatar changes), dropping
// live messages. We therefore keep _msgLog as the canonical conversation and
// route every property swap through _applyProps(), which restores the log
// first. _syncMsgLog() snapshots the live view after every state change.
var _chatEl = null;
var _msgLog = [];
// Property assignment deferred while a stream is active (rebuilding
// mid-stream would kill the in-flight streaming bubble); applied on close.
var _pendingApply = null;

function _syncMsgLog() {
  if (!_chatEl) return;
  try {
    var msgs = _chatEl.getMessages();
    if (msgs) _msgLog = msgs;
  } catch (e) {
    console.warn("chat-bridge: could not sync message log:", e);
  }
}

function _applyProps(newProps) {
  var apply = function () {
    if (!_chatEl) return;
    // Restore the canonical log before reassignment. The restore re-fires
    // onMessage with isHistory:true — we have no onMessage listener, so that
    // is harmless.
    try {
      _chatEl.history = _msgLog;
    } catch (e) {
      console.warn("chat-bridge: could not restore message log:", e);
    }
    for (var key in newProps) {
      try {
        if (key === "hostStyle") {
          // Inline styles on the host element — these beat deep-chat's own
          // one-shot :host rule (background-color:#fff / border) and don't
          // trigger a rebuild.
          for (var p in newProps.hostStyle) _chatEl.style[p] = newProps.hostStyle[p];
          continue;
        }
        _chatEl[key] = newProps[key];
      } catch (e) {
        console.warn("chat-bridge: could not apply " + key + ":", e);
      }
    }
    if (typeof _chatEl.scrollToBottom === "function") _chatEl.scrollToBottom();
    _syncMsgLog();
  };
  if (_isStreaming) {
    // Defer until the stream ends — see onStreamClose / onStreamError.
    _pendingApply = apply;
    return;
  }
  apply();
}

function _handleThemeUpdate(name) {
  if (typeof window.applyChatTheme !== "function") {
    console.warn("chat-bridge: chat-themes.js not loaded");
    return;
  }
  _applyProps(window.applyChatTheme(name || "lightTheme"));
}

// Property changes that arrived while a stream was active are deferred here
// and applied when the stream ends (a rebuild mid-stream would kill the
// in-flight streaming bubble).
function _flushPendingApply() {
  if (!_pendingApply) return;
  var apply = _pendingApply;
  _pendingApply = null;
  apply();
}

// Table enhancement is deferred until stream ends to avoid flickering
// and autoscroll disruption during partial table rendering.
var _isStreaming = false;
var _enhanceTablesFn = null;
var _enhanceDebounceModule = null;

function _scheduleEnhance() {
  if (_enhanceDebounceModule) clearTimeout(_enhanceDebounceModule);
  _enhanceDebounceModule = setTimeout(function () {
    if (_enhanceTablesFn && !_isStreaming) _enhanceTablesFn();
  }, 150);
}

document.addEventListener("DOMContentLoaded", function () {
  // IMPORTANT: use window.qt, not bare qt — deepChat.bundle.js declares
  // `const qt="Authorization header"` at top level, which creates a global
  // lexical binding that shadows the Qt-injected window.qt transport object.
  console.log(
    "chat-bridge: DOM ready, window.qt=" +
      typeof window.qt +
      " transport=" +
      (typeof window.qt !== "undefined"
        ? typeof window.qt.webChannelTransport
        : "N/A"),
  );

  if (typeof window.qt !== "undefined" && window.qt.webChannelTransport) {
    new QWebChannel(window.qt.webChannelTransport, function (channel) {
      console.log(
        "chat-bridge: channel objects:",
        Object.keys(channel.objects),
      );

      aiBridge = channel.objects.aiBridge;

      if (!aiBridge) {
        console.error("chat-bridge: aiBridge not found in channel.objects");
        return;
      }

      aiBridge.onStreamOpen.connect(function () {
        console.log("chat-bridge: onStreamOpen");
        _isStreaming = true;
        // Cancel any pending post-stream table enhancement — a new
        // stream is starting (possibly tool-call loop).
        if (_enhanceDebounceModule) {
          clearTimeout(_enhanceDebounceModule);
          _enhanceDebounceModule = null;
        }
        if (currentSignals) {
          currentSignals.onOpen();
          if (streamHasContent) currentSignals._needNewline = true;
        } else if (!window._introBuf) {
          window._introBuf = "";
        }
      });

      aiBridge.onStreamChunk.connect(function (text) {
        console.log("chat-bridge: onStreamChunk len=" + text.length);
        if (currentSignals) {
          if (currentSignals._needNewline) {
            currentSignals._needNewline = false;
            text = "\n" + text;
          }
          currentSignals.onResponse({ text: text });
        } else {
          // Auto-intro stream — accumulate chunks and add as message when done
          if (!window._introBuf) window._introBuf = "";
          window._introBuf += text;
        }
        streamHasContent = true;
      });

      aiBridge.onStreamClose.connect(function () {
        console.log("chat-bridge: onStreamClose");
        _isStreaming = false;
        if (currentSignals) {
          currentSignals.onClose();
          // Snapshot the completed exchange — the theme/avatar applyProps
          // relies on this being fresh.
          _syncMsgLog();
        } else {
          // Auto-intro stream finished — replace the primer only if we have
          // intro text; never wipe an existing conversation.
          if (_chatEl && window._introBuf) {
            _chatEl.clearMessages();
            _chatEl.addMessage({ text: window._introBuf, role: "ai" });
          }
          window._introBuf = "";
          _syncMsgLog();
        }
        // Keep currentSignals alive — tool-call loops may emit more onOpen/onClose.
        // Run table enhancement now that the stream is fully complete.
        _scheduleEnhance();
        _flushPendingApply();
      });

      aiBridge.onStreamError.connect(function (errorMsg) {
        console.log("chat-bridge: onStreamError: " + errorMsg);
        _isStreaming = false;
        if (currentSignals) {
          try {
            currentSignals.onOpen();
            if (streamHasContent) errorMsg = "\n" + errorMsg;
            currentSignals.onResponse({ text: errorMsg });
          } catch (e) {
            console.warn("chat-bridge: error in onOpen/onResponse:", e);
          }
          try {
            currentSignals.onClose();
          } catch (e) {
            console.warn("chat-bridge: error in onClose:", e);
          }
          _syncMsgLog();
        } else {
          // No active handler — append the error as a message; never wipe
          // the conversation.
          if (_chatEl) _chatEl.addMessage({ text: errorMsg, role: "ai" });
          window._introBuf = "";
          _syncMsgLog();
        }
        currentSignals = null;
        _scheduleEnhance();
        _flushPendingApply();
      });

      aiBridge.onClearChat.connect(function () {
        console.log("chat-bridge: onClearChat — clearing deep-chat UI");
        currentSignals = null;
        if (_chatEl) {
          _chatEl.clearMessages();
          _chatEl.addMessage({ text: "Starting up…", role: "ai" });
          _syncMsgLog();
        }
        window._introBuf = "";
      });

      console.log("chat-bridge: aiBridge connected, setting up deep-chat");
      setupDeepChat();
      // Theme: pull the initial value, then react to live switches from QML.
      // After setupDeepChat so the element exists when the async callback
      // with the initial theme name arrives.
      if (aiBridge.getChatTheme) {
        aiBridge.getChatTheme(function (name) {
          _handleThemeUpdate(name);
        });
      }
      if (aiBridge.chatThemeUpdated) {
        aiBridge.chatThemeUpdated.connect(function (name) {
          _handleThemeUpdate(name);
        });
      }
    });
  } else {
    console.error("chat-bridge: window.qt.webChannelTransport not available");
  }
});

function setupDeepChat() {
  var chat = document.querySelector("deep-chat");
  if (!chat) {
    console.warn("chat-bridge: <deep-chat> element not found");
    return;
  }
  _chatEl = chat;

  chat.connect = {
    stream: true,
    handler: function (body, signals) {
      console.log(
        "chat-bridge: handler called, messages=" +
          (body.messages ? body.messages.length : 0),
      );

      currentSignals = signals;

      // The user message was just rendered — snapshot immediately so a theme
      // switch that lands before/without a stream close still keeps it.
      _syncMsgLog();

      signals.stopClicked.listener = function () {
        console.log("chat-bridge: stop clicked");
        if (aiBridge) aiBridge.stopStream();
      };

      if (aiBridge) {
        var json = JSON.stringify(body.messages);
        console.log(
          "chat-bridge: calling aiBridge.startStream, json len=" + json.length,
        );
        aiBridge.startStream(json);
      } else {
        console.error("chat-bridge: aiBridge is null, cannot start stream");
        signals.onResponse({ error: "AI bridge not connected" });
      }
    },
  };

  // Theme-aware AI avatar (initial) — routed through _applyProps so the
  // message log survives the view rebuild.
  if (aiBridge.aiIconPath) {
    var avatars = chat.avatars || {};
    avatars.ai = {
      src: aiBridge.aiIconPath,
      styles: {
        avatar: { width: "32px", height: "32px", alignSelf: "center" },
      },
    };
    avatars.default = {
      styles: {
        position: "start",
        avatar: { width: "32px", height: "32px", alignSelf: "center" },
      },
    };
    avatars.user = {
      styles: {
        avatar: { width: "30px", height: "30px", alignSelf: "center" },
      },
    };
    _applyProps({ avatars: avatars });
  }

  // React to persona changes
  aiBridge.personaAvatarUpdated.connect(function (newPath) {
    console.log("chat-bridge: aiIconPath changed: " + newPath);
    var avatars = chat.avatars || {};
    avatars.ai = {
      src: newPath,
      styles: {
        avatar: { width: "32px", height: "32px", alignSelf: "center" },
      },
    };
    _applyProps({ avatars: avatars });
  });

  // React to user avatar changes
  if (aiBridge.userIconPath) {
    console.log("chat-bridge: userIconPath: " + aiBridge.userIconPath);
    var avatars = chat.avatars || {};
    avatars.user = {
      src: aiBridge.userIconPath,
      styles: {
        avatar: { width: "30px", height: "30px", alignSelf: "center" },
      },
    };
    _applyProps({ avatars: avatars });
  }

  aiBridge.userAvatarUpdated.connect(function (newPath) {
    console.log("chat-bridge: userIconPath changed: " + newPath);
    var avatars = chat.avatars || {};
    if (newPath) {
      avatars.user = {
        src: newPath,
        styles: {
          avatar: { width: "30px", height: "30px", alignSelf: "center" },
        },
      };
    } else {
      delete avatars.user; // let deep-chat use its default
    }
    _applyProps({ avatars: avatars });
  });

  console.log("chat-bridge: deep-chat handler configured");

  // Use MutationObserver instead of polling to avoid reflow-induced flickering
  // of graphs and other content during streaming.
  if (typeof MutationObserver !== "undefined") {
    var _enhanceDebounce = null;

    function _enhanceTables() {
      var sr = chat.shadowRoot;
      if (!sr || typeof enhanceMarkdownTables !== "function") return;
      var tables = sr.querySelectorAll(
        "table:not(.jasp-no-select):not(.jasp-table-enhanced)",
      );
      if (tables.length) {
        enhanceMarkdownTables(sr);
        // Table enhancement can shrink content height (e.g. combined cells
        // collapse from repeated text to a single &nbsp;). Scroll back to
        // bottom so the user still sees the latest message.
        if (typeof chat.scrollToBottom === "function") chat.scrollToBottom();
      }
    }

    _enhanceTablesFn = _enhanceTables;

    var _tableObserver = new MutationObserver(function (mutations) {
      // Defer table enhancement until the stream is fully complete.
      // Enhancing during streaming causes flickering (repeated class
      // toggles, cell merging) and disrupts autoscroll anchoring.
      if (_isStreaming) return;
      for (var i = 0; i < mutations.length; i++) {
        if (mutations[i].addedNodes.length) {
          if (_enhanceDebounce) clearTimeout(_enhanceDebounce);
          _enhanceDebounce = setTimeout(_enhanceTables, 200);
          return;
        }
      }
    });

    if (chat.shadowRoot) {
      _tableObserver.observe(chat.shadowRoot, {
        childList: true,
        subtree: true,
      });
      _enhanceTables();
    } else {
      // Shadow root not ready yet — poll briefly
      var _srPoll = setInterval(function () {
        if (chat.shadowRoot) {
          clearInterval(_srPoll);
          _tableObserver.observe(chat.shadowRoot, {
            childList: true,
            subtree: true,
          });
          _enhanceTables();
        }
      }, 50);
    }
  }

  // Intercept link clicks inside deep-chat and open in external browser.
  // Uses capture phase + composedPath() to see through shadow DOM.
  document.addEventListener(
    "click",
    function (e) {
      var path = e.composedPath();
      for (var i = 0; i < path.length; i++) {
        var el = path[i];
        if (
          el.tagName === "A" &&
          el.href &&
          el.href.indexOf("#") !== 0 &&
          !el.href.startsWith("qrc:")
        ) {
          e.preventDefault();
          e.stopPropagation();
          e.stopImmediatePropagation();
          if (aiBridge && aiBridge.openUrl) aiBridge.openUrl(el.href);
          return;
        }
      }
    },
    true,
  );

  // Prime the chat with an AI intro on first load.
  // Deferred 100 ms to let deep-chat's internal init finish; calling
  // clearMessages() / addMessage() before the component is ready would be a no-op.
  setTimeout(function () {
    if (aiBridge) aiBridge.clearChat();
  }, 100);
}
