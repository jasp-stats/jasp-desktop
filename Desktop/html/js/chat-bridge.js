"use strict";

var aiBridge = null;
var currentSignals = null;
var streamHasContent = false;

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
        if (currentSignals) {
          currentSignals.onClose();
        } else {
          var chat = document.querySelector("deep-chat");
          if (chat) {
            chat.clearMessages();
            if (window._introBuf) {
              chat.addMessage({ text: window._introBuf, role: "ai" });
            }
          }
          window._introBuf = "";
        }
        // Keep currentSignals alive — tool-call loops may emit more onOpen/onClose
      });

      aiBridge.onStreamError.connect(function (errorMsg) {
        console.log("chat-bridge: onStreamError: " + errorMsg);
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
        } else {
          var chat = document.querySelector("deep-chat");
          if (chat) {
            chat.clearMessages();
            chat.addMessage({ text: errorMsg, role: "ai" });
          }
          window._introBuf = "";
        }
        currentSignals = null;
      });

      aiBridge.onClearChat.connect(function () {
        console.log("chat-bridge: onClearChat — clearing deep-chat UI");
        var chat = document.querySelector("deep-chat");
        if (chat) {
          chat.clearMessages();
          chat.addMessage({ text: "Starting up…", role: "ai" });
        }
        window._introBuf = "";
      });

      console.log("chat-bridge: aiBridge connected, setting up deep-chat");
      setupDeepChat();
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

  chat.connect = {
    stream: true,
    handler: function (body, signals) {
      console.log(
        "chat-bridge: handler called, messages=" +
          (body.messages ? body.messages.length : 0),
      );

      currentSignals = signals;

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

  // Theme-aware AI avatar
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
    chat.avatars = avatars;
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
    chat.avatars = avatars;
  });

  console.log("chat-bridge: deep-chat handler configured");

  var enhanceInterval = setInterval(function () {
    if (typeof enhanceMarkdownTables !== "function") return;
    var sr = chat.shadowRoot;
    if (!sr) return;
    var tables = sr.querySelectorAll(
      "table:not(.jasp-no-select):not(.jasp-table-enhanced)",
    );
    if (tables.length) {
      enhanceMarkdownTables(sr);
    }
  }, 500);

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
