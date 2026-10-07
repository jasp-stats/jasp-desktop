"use strict";

// Four chat themes for the embedded deep-chat app, matching the JASP
// interface theme (light/dark) and edition (normal / enterprise-PRO):
//   lightTheme, darkTheme, lightProTheme, darkProTheme
// Switched live via window.applyChatTheme(name); chat-bridge.js routes the
// returned property payload through its history-preserving applyProps().
//
// deep-chat notes (verified against upstream source, v2.5.x):
// - There is no built-in theme system; styling is done via reactive style
//   properties which are rebuilt on reassignment.
// - `auxiliaryStyle` (set once in chat.html) contains only structural rules
//   that consume the CSS custom properties below — custom properties cascade
//   into the shadow DOM and live-update, no rebuild needed.

(function () {
  var SUBMIT_SVG_CONTENT =
    '<?xml version="1.0" ?> <svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"> <g> <path d="M21.66,12a2,2,0,0,1-1.14,1.81L5.87,20.75A2,2,0,0,1,5,21a2,2,0,0,1-1.82-2.82L5.46,13H11a1,1,0,0,0,0-2H5.46L3.18,5.87A2,2,0,0,1,5.86,3.25h0l14.65,6.94A2,2,0,0,1,21.66,12Z"> </path> </g> </svg>';
  var STOP_SVG_CONTENT =
    '<?xml version="1.0" encoding="utf-8"?> <svg viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg"> <rect width="24" height="24" rx="4" ry="4" /> </svg>';

  // White icon on the coloured accent button (works on any accent colour).
  var ICON_WHITE_FILTER =
    "brightness(0) saturate(100%) invert(100%) sepia(28%) saturate(2%) hue-rotate(69deg) brightness(107%) contrast(100%)";
  // Original reddish loading/stop icon for light containers.
  var ICON_LIGHT_FILTER =
    "brightness(0) saturate(100%) invert(72%) sepia(0%) saturate(3044%) hue-rotate(322deg) brightness(100%) contrast(96%)";
  // Lighter variant for dark containers.
  var ICON_DARK_FILTER =
    "brightness(0) saturate(100%) invert(85%) sepia(0%) saturate(0%) hue-rotate(322deg) brightness(110%) contrast(96%)";

  function makeTheme(opts) {
    var isDark = opts.isDark;
    var container = isDark ? "#2E2E2E" : "white";
    var stopHover = isDark ? "#4a4a4a" : "#dadada52";
    var iconFilter = isDark ? ICON_DARK_FILTER : ICON_LIGHT_FILTER;
    var text = isDark ? "#EEE" : "black";
    var placeholder = isDark ? "#747677" : "#9A9A9A";
    var aiBorder = isDark ? "rgba(255,255,255,.1)" : "rgba(0,0,0,.1)";
    // deep-chat v2.0.0 hardcodes :host { background-color:#fff;
    // border:1px solid #cacaca } and #text-input-container { background-color:#fff }.
    // Host inline styles beat the one-shot :host rule; the input container is
    // overridden via textInput.styles.container.
    var hostStyle = isDark
      ? { backgroundColor: "#212121", borderColor: "#333333" }
      : { backgroundColor: "#FFFFFF", borderColor: "#cacaca" };
    var inputContainer = { width: "97.5%" };
    if (isDark) {
      inputContainer.backgroundColor = "#2E2E2E";
      inputContainer.border = "1px solid rgba(255,255,255,.12)";
      inputContainer.boxShadow = "none";
    }
    return {
      vars: {
        "jasp-zebra": isDark ? "#2E2E2E" : "#EBEBEB",
        "jasp-link": opts.link,
        "jasp-scrollbar": isDark ? "#5c5c5c" : "#9A9A9A",
      },
      messageStyles: {
        default: {
          shared: {
            outerContainer: { paddingLeft: "0", paddingRight: "0" },
            bubble: {
              maxWidth: "100%",
              backgroundColor: "unset",
              marginTop: "10px",
              marginBottom: "10px",
            },
          },
          user: {
            bubble: { marginLeft: "0px", color: text },
          },
          ai: {
            outerContainer: {
              backgroundColor: isDark ? "#2A2A2A" : "rgba(247,247,248)",
              borderTop: "1px solid " + aiBorder,
              borderBottom: "1px solid " + aiBorder,
            },
          },
        },
      },
      submitButtonStyles: {
        submit: {
          container: {
            default: { backgroundColor: opts.accent },
            hover: { backgroundColor: opts.accentHover },
            click: { backgroundColor: opts.accentClick },
          },
          svg: {
            content: SUBMIT_SVG_CONTENT,
            styles: {
              default: {
                width: "1.3em",
                marginTop: "0.15em",
                filter: ICON_WHITE_FILTER,
              },
            },
          },
        },
        loading: {
          container: { default: { backgroundColor: container } },
          svg: {
            styles: { default: { filter: iconFilter } },
          },
        },
        stop: {
          container: {
            default: { backgroundColor: container },
            hover: { backgroundColor: stopHover },
          },
          svg: {
            content: STOP_SVG_CONTENT,
            styles: {
              default: {
                width: "0.95em",
                marginTop: "0.32em",
                filter: iconFilter,
              },
            },
          },
        },
      },
      textInput: {
        placeholder: { text: "Ask anything...", style: { color: placeholder } },
        styles: {
          container: inputContainer,
          text: { color: text },
        },
      },
      hostStyle: hostStyle,
    };
  }

  window.CHAT_THEMES = {
    // Normal edition — JASP blue accents.
    lightTheme: makeTheme({
      isDark: false,
      accent: "#14a1e3",
      accentHover: "#0d8ac4",
      accentClick: "#0b7eb2",
      link: "#14a1e3",
    }),
    darkTheme: makeTheme({
      isDark: true,
      accent: "#0481c3",
      accentHover: "#036ba3",
      accentClick: "#025e8f",
      link: "#5db6e8",
    }),
    // Enterprise/PRO edition — teal accents (matches WelcomePage branding).
    lightProTheme: makeTheme({
      isDark: false,
      accent: "#007f8f",
      accentHover: "#006a78",
      accentClick: "#005c68",
      link: "#007f8f",
    }),
    darkProTheme: makeTheme({
      isDark: true,
      accent: "#008999",
      accentHover: "#00a3b8",
      accentClick: "#008698",
      link: "#4fb3c4",
    }),
  };

  // Applies the CSS custom properties (instant — they cascade into the
  // shadow DOM and live-update the auxiliaryStyle rules) and returns the
  // deep-chat property payload for chat-bridge's applyProps().
  window.applyChatTheme = function (name) {
    var theme = window.CHAT_THEMES[name];
    if (!theme) {
      console.warn(
        "chat-themes: unknown theme '" + name + "', falling back to lightTheme",
      );
      theme = window.CHAT_THEMES.lightTheme;
    }
    var root = document.documentElement;
    for (var key in theme.vars) root.style.setProperty("--" + key, theme.vars[key]);
    return {
      messageStyles: theme.messageStyles,
      submitButtonStyles: theme.submitButtonStyles,
      textInput: theme.textInput,
      hostStyle: theme.hostStyle,
    };
  };
})();