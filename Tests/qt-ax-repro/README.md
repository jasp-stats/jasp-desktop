# Qt/macOS accessibility bridge repro tool

Minimal QML app + AX driver used to isolate the macOS bug where Qt's
accessibility bridge dies permanently (`kAXErrorNotImplemented` / -25204 on
every query) after an `AXPress` on a button that opens a popup.

Observed with JASP (custom Qt 6.12-dev snapshot 2026-06-02) on macOS 26.5:
pressing a ribbon button that opens a QQuickMenu sometimes kills the whole
AX server inside the app — window/menu/table queries all fail with -25204
until the app is restarted. VoiceOver also loses the app at that point.

## Layout

- `main.cpp` / `main.qml` / `web.html` / `main.qrc` / `axrepro.pro`
  Minimal app: an `Open menu` button that pops a `Menu`, a plain button,
  and a hidden `WebEngineView` (the JASP scenarios always had webengine
  loaded; kept so the repro matches).
- `hammer.py` — launches the app, engages the AX bridge the way an
  assistive client does (`AXManualAccessibility` + focused-element queries;
  Qt 6.12 activates its bridge lazily on the first AT queries that reach the
  QNSView), then N rounds of: find `Open menu` via AX, `AXPress` it, check
  the bridge is still alive, `Esc` via CGEvent, check again.

## Build & run

Against stock Qt (control):

```sh
mkdir -p build-hb && cd build-hb
/opt/homebrew/opt/qt/bin/qmake ../axrepro.pro && make -j8
python3 ../hammer.py axrepro.app/Contents/MacOS/axrepro 20
```

Against the custom Qt used by JASP:

```sh
mkdir -p build-custom && cd build-custom
/Users/virtuoos/Broncode/JASP/qt-install/bin/qmake ../axrepro.pro && make -j8
python3 ../hammer.py axrepro.app/Contents/MacOS/axrepro 20
```

Requires the calling process (terminal) to have *Accessibility* permission
(System Settings → Privacy & Security → Accessibility).

## Current results (2026-09-28, macOS 26.5, arm64)

- Homebrew Qt 6.11.1: survived 20 rounds.
- Custom Qt 6.12-dev (qt-install): survived 20 rounds.

So the vanilla repro does **not** reproduce the JASP death yet — the
trigger is something JASP-specific on top of this shape (candidates:
JASP's custom popup/menu QML, its `Application::notify` event path, or an
interaction with the webengine AX activation observer). The hammer is the
regression harness for narrowing that down; keep it with the eventual
upstream Qt bug report.

## Related JASP-side observations

- QML `AXMenuItem`s do not implement `AXPress` (-25204 even while the bridge
  is healthy) — menu items must be activated via keyboard or synthesized
  clicks on macOS.
- `AXPress` on plain buttons and even `AXStaticText` returns success.
- The bridge never revives after the wedge; `AXManualAccessibility` re-set
  does not help.
- qtbase's `QCocoaScreen::requestUpdate()` also crashes (null-deref in
  `CGEventTapEnable` ← `SLEventTapEnable`) when the process lacks the
  Accessibility entitlement / GUI session — separate robustness issue,
  hit when running the dev binary from a non-GUI account.
