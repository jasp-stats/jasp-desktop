# JASP Desktop — Agent Guide (Windows)

## Project overview

JASP desktop application with a custom-patched Qt WebEngine for
accessibility work (screen-reader support on Windows/macOS).

- **Repo**: `C:\Virtuoos\jasp-desktop` (origin: `JorisGoosen/jasp-desktop`, branch: `screenreader`)
- **Qt WebEngine repo**: `C:\Virtuoos\qt612\src\qtwebengine` (origin: `jasp-stats/qtwebengine`, branch: `accessibilityQuick`)
- **Qt install prefix**: `C:\Qt\6.12.0-dev-msvc2022` (built from dev snapshots, see "Qt stack" below)
- **Build directory**: `jasp-build-screenreader/` (Release, Ninja)
- **Old build** (reference only, different branch): `jasp-build/`

## Environment setup

```powershell
# All builds and launches need this environment:
. C:\Virtuoos\tools\qt612-env.ps1
# Adds: CMake (C:\Qt\Tools), Ninja, Python 3.12, node, perl, bison/flex/gperf
# Sets: PATH (strips Rtools), vcvars64 (MSVC 14.44)
# NOTE: Rtools is REMOVED from PATH by qt612-env.ps1 — it poisons MSVC builds
#       (MinGW headers leaking into MSVC compilations via ICU detection).
#       Add it back explicitly when building R-Interface (MinGW) or running R.
```

Helper scripts (all in `C:\Virtuoos\tools\`):

| Script | Purpose |
|---|---|
| `start-jasp.cmd` | Launch JASP detached (WMI) with correct PATH/R_HOME/QTWEBENGINE_AX_DEBUG=1 |
| `launch-jasp.ps1` | Foreground launch with output redirect |
| `jasp-build.ps1` | `configure` / `build [target]` / `install` for jasp-build-screenreader |
| `build-qtmodule.ps1` | Configure + build + install a single Qt module |
| `rebuild-webengine.ps1` | Incremental webengine rebuild + install (~2–10 min) |
| `freeze-sampler.ps1` | Detect CPU-spin freezes, capture cdb stacks |
| `uia-walker.ps1` | UIA tree walker (button listing, invocation, freeze detection) |

## Building

```powershell
# JASP (after any source change):
powershell -File C:\Virtuoos\tools\jasp-build.ps1 build JASP

# QtWebEngine (after patching src/qtwebengine):
powershell -File C:\Virtuoos\tools\rebuild-webengine.ps1
# → builds + installs into C:\Qt\6.12.0-dev-msvc2022
# Must kill JASP first (DLL is locked while running)

# JASPConfigure (R-Interface + tools libs + modules):
# Wipes and reconfigures; needed after R version changes or R 4.5.2 re-install
```

Configure command (for reference):
```
cmake -GNinja -S . -B jasp-build-screenreader -DCMAKE_BUILD_TYPE=Release
  -DCMAKE_PREFIX_PATH=C:/Qt/6.12.0-dev-msvc2022
  -DRTOOLS_PATH=C:/rtools45/ucrt64
  -DJASP_STATIC_IS_DOWN_AGAIN=ON
```

## Critical version pins

| Component | Version | Why |
|---|---|---|
| R | **4.5.2** | Windows module bundles (remote-bundles.json) are built against R 4.5.2. Loading them under 4.6.x fails: `LoadLibrary failure: The specified procedure could not be found` on rlang.dll etc. |
| Qt | **6.12.0-dev** (snapshots 2026-06-02) | The accessibilityQuick webengine fork pins `find_package(Qt6 6.12.0 REQUIRED)`. All base modules synced via each repo's `dependencies.yaml` |
| MSVC | 14.44 (VS 2022 17.14) | Qt 6.12-dev requires C++20 |
| CMake | 3.30.5 (C:\Qt\Tools) | Qt 6.12-dev minimum |
| Rtools | 45 (ucrt64) | MinGW for R-Interface + R package compilation |

**R version mismatch symptoms**: engine spawns, enters eventloop, then
`unable to load shared object .../rlang.dll: LoadLibrary failure` when
loading any analysis module. Fix: install the matching R version into
`jasp-build-screenreader\R`.

## Qt 6.12-dev base stack

Built from source (installed into `C:\Qt\6.12.0-dev-msvc2022`):
qtbase, qtshadertools, qtdeclarative, qtsvg, qtwebchannel, qthttpserver, qttools.

All pinned to the **same dev snapshot** (matching the webengine fork's
`dependencies.yaml` refs — dev branch, 2026-06-02). Sources in
`C:\Virtuoos\qt612\src\`, builds in `C:\Virtuoos\qt612\build\`.

**Do NOT mix Qt versions**: the webengine fork hard-requires
`find_package(Qt6 ${PROJECT_VERSION})` — your installed 6.8–6.11
Qt binaries won't work.

**CRITICAL**: after reinstalling/rebuilding webengine, verify the DLL
size matches the build copy. A truncated install (file locked while
JASP runs) causes instant-exit with `0xC000027B` and no error output.
```powershell
(Get-Item C:\Qt\6.12.0-dev-msvc2022\bin\Qt6WebEngineCore.dll).Length
# should be ~205 MB, not 1.5 MB
```

## Running JASP

```powershell
# Detached (survives agent session):
Invoke-CimMethod -ClassName Win32_Process -MethodName Create -Arguments @{
    CommandLine = "cmd /c C:\Virtuoos\tools\start-jasp.cmd"
}

# The engine spawns lazily when a dataset is loaded (New Data, file open).
```

Debug output goes to `C:\Virtuoos\tools\jasp-run.log` (stdout, including
Qt warnings via JASP's message handler) and `jasp-run-err.log` (stderr).
Sandbox logs: `C:\Users\<user>\Documents\JASP_Sandbox\Logs\`.

## Accessibility architecture

### WebEngine side (C:\Virtuoos\qt612\src\qtwebengine)

Branch `accessibilityQuick`. Key files:

| File | Role |
|---|---|
| `src/core/browser_accessibility_qt.cpp` | Chromium AX node ↔ QAccessible bridge (`BrowserAccessibilityInterface`) |
| `src/core/accessibility_activation_observer.cpp` | Enables AX mode on screen-reader activation |
| `src/core/web_contents_adapter.cpp` | `browserAccessible()` entry point |
| `src/core/browser_accessibility_manager_qt.cpp` | AX event dispatch to Qt |

**Rules for a11y patches** (learned the hard way — Narrator freezes):
1. **Never reload web contents from inside a screen-reader query or
   activation callback** — the AX tree is torn down under the caller.
2. **GetHypertext() must preserve embedded-object placeholders** (U+FFFC):
   AXPosition iteration advances by hypertext length; returning text
   content (or empty) for object nodes makes positions never advance.
3. **textContent() must only carry text for text-ish roles** (kStaticText,
   kInlineTextBox, kTextField, kTextFieldWithComboBox). Exposing Value/Name
   as text on containers gives them a bogus characterCount → screen readers
   iterate per-character bounds over the whole subtree.

Debug: set `QTWEBENGINE_AX_DEBUG=1` before launching; output goes to
jasp-run.log with `[AX-DEBUG]` prefix.

### Test side (Tests/)

Platform-dispatched backends:
```
Tests/a11y_backends/
    __init__.py          → load_backend() dispatch (JASP_A11Y_BACKEND env or platform)
    atspi_backend.py     → Linux (AT-SPI2, existing behaviour)
    uia_backend.py       → Windows (pywinauto)
    ax_backend.py        → macOS (pyobjc, best-effort scaffold)
```

`accessibility_common.py` auto-selects the backend. Node API is duck-typed
to AT-SPI conventions: `get_role_name()`, `get_name()`, `get_child_count()`,
`get_child_at_index()`, `do_action()`, `grab_focus()`, `get_rect()`.

Windows test runner:
```powershell
powershell -File Tests\run_test_session_win.ps1 -Test Tests\test_accessibility_win.py [-Narrator] [-KeepJasp]
```

Venv: `Tests/a11y-venv` (pywinauto). Linux tests unchanged (AT-SPI path).

### Windows UIA quirks (pywinauto)

- The native file dialog is an **owned popup nested inside the main
  window's UIA subtree** — `Desktop.windows()` never returns it.
  Use `win.descendants(control_type="Window")` + `class_name == "#32770"`.
- QML `Button.invoke()` sometimes only focuses without pressing.
  Use invoke → verify → focus+SPACE → real-click fallback.
- The QML accessible tree is **shallow at startup** (~20 elements for the
  main window: title bar + ribbon + welcome page group). Deeper content
  (data table, results webview) populates when accessibility is engaged
  by interaction.
- `os.kill(pid, 0)` on Windows **kills the process** — use
  `ctypes.windll.kernel32.OpenProcess` for liveness checks.
- Window titles get a `*` suffix when dirty: `JASP*` — use prefix matching.
- Menu buttons **toggle**: invoking Main menu when it's already open
  closes it. Check for expected content before toggling again.
- Sending `{ESC}` (via `close_menu()`) cancels open file dialogs —
  never call it while a dialog is open.

## R-Interface (Windows)

Built as a separate MinGW CMake subproject inside the build dir
(`R-Interface/`), using Rtools45 compilers. It links against R.dll
from `jasp-build-screenreader\R\bin\x64`.

After an R version swap: delete `jasp-build-screenreader\R-Interface\`
and rebuild — it needs recompilation against the new R headers.

Tools libs (Rcpp, RInside, jaspModuleBundleManager) are restored via
`install-renv.R` + `install-tools.R` (configure-time). If renv's
staged-install hangs, install manually:
```powershell
& R\bin\x64\R.exe --slave --no-restore --no-save --file=_scripts\install-tools.R
```
Note: `Rscript --file=` doesn't work — use `R.exe --file=` instead.

If renv fails entirely (it can hang on staged installs on Windows):
```powershell
# Install packages directly as win.binary:
install.packages(c("rlang", ...), lib = "<target_lib>", type = "win.binary")
```

## Known remaining issues

- CSV preview window can freeze (Chromium `base/` code spin, stacks in
  `C:\Virtuoos\tools\freeze-dumps\spin3-full.txt`). Avoided by loading
  `.jasp` files instead of `.csv` for automation.
- Intermittent CPU bursts during Narrator polling (subside; may warrant
  investigation).
- macOS accessibility (ax_backend.py) is scaffolded but unvalidated on
  a real mac.

## Debugging tools

| Tool | Location |
|---|---|
| cdb (WinDbg) | `"C:\Program Files (x86)\Windows Kits\10\Debuggers\x64\cdb.exe"` |
| Freeze sampler | `C:\Virtuoos\tools\freeze-sampler.ps1` (detects CPU-spin, captures stacks) |
| Stack capture | `cdb -p <pid> -c "~0 k 60; qd"` |
| Map generation | `C:\Virtuoos\tools\relink-with-map.ps1` (relink with /MAP, parse RVAs) |
| RVA resolver | `C:\Virtuoos\tools\resolve-rvas2.py` (map → function names) |
| UIA walker | `C:\Virtuoos\tools\uia-walker.ps1` |

To resolve unpatched-build addresses: relink with /MAP into a scratch dir
(same inputs = same layout), parse the map for the symbol containing the RVA.
The map's preferred base is 0x18000000; runtime RVAs = address − actual base.
