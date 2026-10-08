"use strict";

// Four chat themes for the embedded deep-chat app, keyed by JASP interface
// theme (light/dark) x edition (normal / enterprise-PRO):
//   lightTheme, darkTheme, lightProTheme, darkProTheme
//
// Flow: ChatWindow.qml computes the key from preferencesModel.currentThemeName
// and the PRO flag, and pushes it over the WebChannel. chat-bridge.js pulls it
// once on connect (aiBridge.getChatTheme) and applies live switches
// (aiBridge.chatThemeUpdated) via window.applyChatTheme(name) below.
//
// How theming works (deep-chat v2.0.0 has no built-in theme system):
// - auxiliaryStyle in chat.html is applied ONCE and contains only structural
//   rules consuming the CSS custom properties from `vars` (--jasp-zebra,
//   --jasp-link, --jasp-scrollbar). Custom properties cascade into the shadow
//   DOM and live-update, so switching vars needs no rebuild.
// - messageStyles / submitButtonStyles / textInput are reactive properties;
//   reassigning them rebuilds the chat view, so chat-bridge.js does it through
//   its history-preserving applyProps().
// - hostStyle overrides the hardcoded one-shot :host rule
//   (:host { background-color:#fff; border:1px solid #cacaca }) with inline
//   styles, which always win.

// ----------------------------------------------------------------- palettes
// Colours shared by both editions of one interface theme. Keep in sync with
// Desktop/components/JASP/Theme/Theme.qml (light) / DarkTheme.qml (dark) —
// the JaspTheme counterpart is noted per value.

var LIGHT = {
  // deep-chat defaults for the host element and text input container
  pageBackground: "#FFFFFF",
  pageBorder: "#cacaca",
  inputBackground: "white",
  inputBorder: "1px solid #0000001a",
  inputBoxShadow: "#959da533 0 1px 12px",
  // jaspTheme.textEnabled / grayDarker
  text: "black",
  textDisabled: "#9A9A9A",
  // AI message rows
  aiRowBackground: "rgba(247,247,248)",
  aiRowBorder: "rgba(0,0,0,.1)",
  // zebra rows in markdown tables (--jasp-zebra in chat.html auxiliaryStyle)
  zebra: "#EBEBEB",
  // scrollbar thumb (--jasp-scrollbar)
  scrollbar: "#9A9A9A",
  // loading / stop button containers
  buttonContainer: "white",
  buttonContainerHover: "#dadada52",
  // icon tint for the loading/stop svgs on that container
  iconFilter: ICON_LIGHT_FILTER,
};

var DARK = {
  // jaspTheme.uiBackground
  pageBackground: "#212121",
  pageBorder: "#333333",
  inputBackground: "#2E2E2E",
  inputBorder: "1px solid rgba(255,255,255,.12)",
  inputBoxShadow: "none",
  // jaspTheme.black / grayDarker
  text: "#EEE",
  textDisabled: "#747677",
  aiRowBackground: "#2A2A2A",
  aiRowBorder: "rgba(255,255,255,.1)",
  zebra: "#2E2E2E",
  // jaspTheme.gray
  scrollbar: "#5c5c5c",
  buttonContainer: "#2E2E2E",
  buttonContainerHover: "#4a4a4a",
  iconFilter: ICON_DARK_FILTER,
};

// ------------------------------------------------------------------ accents
// Submit/send button and markdown-link colours. Normal edition: JASP blue
// (jaspBlue, light #14a1e3 / dark #0481c3 per DarkTheme.qml). Enterprise/PRO
// edition: teal, matching the WelcomePage branding (#007f8f); the dark-pro
// accent is slightly brighter for contrast on the dark background.

var JASP_BLUE_LIGHT = {
  accent: "#14a1e3",
  accentHover: "#0d8ac4",
  accentClick: "#0b7eb2",
  link: "#14a1e3",
};
var JASP_BLUE_DARK = {
  accent: "#0481c3",
  accentHover: "#036ba3",
  accentClick: "#025e8f",
  link: "#5db6e8",
};
var PRO_TEAL_LIGHT = {
  accent: "#007f8f",
  accentHover: "#006a78",
  accentClick: "#005c68",
  link: "#007f8f",
};
var PRO_TEAL_DARK = {
  accent: "#008999",
  accentHover: "#00a3b8",
  accentClick: "#008698",
  link: "#4fb3c4",
};

// ------------------------------------------------------------- svg assets
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

// ----------------------------------------------------------------- builder
function makeTheme(palette, accent) {
  return {
    vars: {
      "jasp-zebra": palette.zebra,
      "jasp-link": accent.link,
      "jasp-scrollbar": palette.scrollbar,
    },
    hostStyle: {
      backgroundColor: palette.pageBackground,
      borderColor: palette.pageBorder,
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
          bubble: { marginLeft: "0px", color: palette.text },
        },
        ai: {
          outerContainer: {
            backgroundColor: palette.aiRowBackground,
            borderTop: "1px solid " + palette.aiRowBorder,
            borderBottom: "1px solid " + palette.aiRowBorder,
          },
          // deep-chat hardcodes .ai-message-text { color:#000 } — without
          // this the AI text stays black and is unreadable in dark mode.
          bubble: { color: palette.text },
        },
      },
    },
    submitButtonStyles: {
      submit: {
        container: {
          default: { backgroundColor: accent.accent },
          hover: { backgroundColor: accent.accentHover },
          click: { backgroundColor: accent.accentClick },
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
        container: { default: { backgroundColor: palette.buttonContainer } },
        svg: {
          styles: { default: { filter: palette.iconFilter } },
        },
      },
      stop: {
        container: {
          default: { backgroundColor: palette.buttonContainer },
          hover: { backgroundColor: palette.buttonContainerHover },
        },
        svg: {
          content: STOP_SVG_CONTENT,
          styles: {
            default: {
              width: "0.95em",
              marginTop: "0.32em",
              filter: palette.iconFilter,
            },
          },
        },
      },
    },
    textInput: {
      placeholder: {
        text: "Ask anything...",
        style: { color: palette.textDisabled },
      },
      styles: {
        container: {
          width: "97.5%",
          backgroundColor: palette.inputBackground,
          border: palette.inputBorder,
          boxShadow: palette.inputBoxShadow,
        },
        text: { color: palette.text },
      },
    },
  };
}

// ------------------------------------------------------------------ themes
// To adjust a colour: edit the palettes/accents above. To add a theme: add an
// entry here and teach ChatWindow.qml to produce its key.
window.CHAT_THEMES = {
  lightTheme: makeTheme(LIGHT, JASP_BLUE_LIGHT),
  darkTheme: makeTheme(DARK, JASP_BLUE_DARK),
  lightProTheme: makeTheme(LIGHT, PRO_TEAL_LIGHT),
  darkProTheme: makeTheme(DARK, PRO_TEAL_DARK),
};

// Applies the CSS custom properties (instant — they cascade into the shadow
// DOM and live-update the auxiliaryStyle rules) and returns the deep-chat
// property payload for chat-bridge's applyProps().
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