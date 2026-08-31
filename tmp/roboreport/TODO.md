# RoboReport — What's Left to Implement

## Status Legend

- [x] **Done** — implemented and (where applicable) validated
- [~] **In progress / next**
- [ ] **Not started**

---

## Foundation (DONE)

- [x] **R package `jaspRoboReport`** (`Engine/jaspRoboReport/`)
  - RPC client (`rr_call`, all 13 tool wrappers)
  - Results adapter (`rr_results`, RDS reading + jaspBase wrapper stripping)
  - Pipeline helpers (`rr_get_options`, `rr_create_and_run`)
  - Utilities (`rr_select`, `rr_get`)
  - Formatting (`fmt_p`, `fmt_ci`, `fmt_effect_size`, `fmt_stat`, `fmt_mean_sd`)
  - Entry point (`run_script`, script contract: `roboreport_main(analysisId)`)

- [x] **jaspBase fixes** (`/home/sp42/jaspBase`)
  - Key-based naming in `toRObject()` (not localized titles)
  - Placeholder string handling in `deriveColumnType()` + `toRObject()`
  - Conditional RDS stripping via `JASP_RDS_STRIP` env var (XPtr + plot bloat)

- [x] **JASP desktop** — `ProcessHelper` sets `JASP_RESULTS_RDS=1` for engines
  (RDS saving on; `JASP_RDS_STRIP` is no longer set, so the RDS is saved
  unstripped — `rr_results()` handles both forms)

- [x] **First script** — `Resources/roboreport/jaspTTests/TTestIndependent.R`
  - Full pipeline: read options → plan → create sibling → extract → build → compose
  - Assumption-driven test selection (normality → Mann-Whitney, etc.)
  - Effect size analysis with practical significance interpretation

---

## Phase 4: C++ RoboReportManager + QML Button  ✅ DONE

Goal: clicking a button on an analysis fires the R script for that
analysis type via the existing RCmdEngine, end to end.

### Verified integration points (from codebase read)

| Concern | API | File |
|---|---|---|
| Singleton model | `AgentStateTracker::init()` pattern | `Desktop/ai/agentstatetracker.{h,cpp}` |
| RCmdEngine access | `EngineSync::singleton()->createRCmdEngine()` → `EngineRepresentation*` (idempotent) | `Desktop/engine/enginesync.h:53` |
| Fire R code | `_engine->runScriptOnProcess(const QString&)` | `Desktop/engine/enginerepresentation.h:41` |
| Output signals | `rCodeReturned(QString, int, bool)`, `rCodeReturnedLog(QString, bool)` | `enginerepresentation.h:159-163` |
| Idle guard | `_engine->idle()` — same guard RCommander uses | `Desktop/qquick/rcommander.cpp:46` |
| RPC server | `_rpcServer` on MainWindow; `start()`, `serverPort()` | `Desktop/rpc/jasprpcserver.{h,cpp}` |
| Resolve analysis by ID | `Analyses::analyses()->get(size_t id)` → `Analysis*` | `Desktop/analysis/analyses.cpp:907` (`_rpcResolveAnalysis`) |
| Analysis identity | `a->name()` → `std::string` ("TTestIndependent"), `a->module()` → `std::string` ("jaspTTests") | `Desktop/analysis/analysis.h:104,108` |
| MainWindow init site | After `AgentStateTracker::init();` at `mainwindow.cpp:196` | — |
| QML passthrough model | `Q_INVOKABLE void annotateAnalysis()` | `Desktop/mainwindow.h:202` |
| Button location | `AnalysisFormExpander.qml` `Row { id: buttons }` beside `annotateButton` (~L404) | — |

**No CMake changes needed.** `Desktop/CMakeLists.txt:35-36` uses
`file(GLOB_RECURSE ...)` over `*.h`/`*.cpp`, so a new
`Desktop/roboreport/` dir is picked up automatically. `Resources/` is
copied wholesale by the post-build `copy_directory` step (L77-84), so
scripts deploy automatically too.

### 4a. `RoboReportManager` singleton  ✅

**Location:** `Desktop/roboreport/roboreportmanager.{h,cpp}`

Public surface (mirrors IMPLEMENTATION.md §3.1):
```cpp
class RoboReportManager : public QObject
{
    Q_OBJECT
public:
    static RoboReportManager* manager();          // null-safe
    static void init(QObject* parent = nullptr);  // idempotent
    static void runForAnalysis(int analysisId);   // null-safe entry point
    static bool hasScript(const std::string& module, const std::string& analysis);
signals:
    void scriptStarted(int analysisId);
    void scriptOutput(int analysisId, const QString& line);
    void scriptFinished(int analysisId, bool success, const QString& errorMsg);
private:
    explicit RoboReportManager(QObject* parent);
    void _runForAnalysis(int analysisId);
    QString _resolveScriptPath(const std::string& module, const std::string& analysis) const;
    void _ensureEngine();     // lazy createRCmdEngine + signal wiring
    void _ensureRpcServer();  // start _rpcServer if not listening
    EngineRepresentation* _engine = nullptr;
    static RoboReportManager* _singleton;
};
```

**`_runForAnalysis` flow:**
1. Resolve `Analysis*` via `Analyses::analyses()->get(id)`. Null → log + return.
2. `_resolveScriptPath(a->module(), a->name())` → `QFile::exists()` check.
   No script → log "no RoboReport script for <module>::<name>" + return.
3. `_ensureEngine()` — `EngineSync::singleton()->createRCmdEngine()`; connect
   `rCodeReturned` / `rCodeReturnedLog` once.
4. `_ensureRpcServer()` — if `!MainWindow::singleton()->rpcServerListening()`
   (or equivalent), call `start()`.
5. Guard: `if (!_engine->idle()) { queue-or-skip; return; }`
6. Build typed R wrapper string:
   ```r
   tryCatch(
     jaspRoboReport::run_script(
       path       = "<abs path>",
       analysisId = <id>L,
       rpcHost    = "127.0.0.1",
       rpcPort    = <port>L
     ),
     error = function(e) cat("ROBOREPORT_ERROR:", conditionMessage(e), "\n")
   )
   ```
   Path escaping: wrap in single quotes, escape embedded single quotes.
7. `_engine->runScriptOnProcess(wrapper);`
8. `emit scriptStarted(id);` Output streams via the connected signals;
   `rCodeReturned` (with `hasError`) drives `scriptFinished`.

**Script path convention:** `Resources/roboreport/<module>/<AnalysisName>.R`,
resolved relative to the app's resource directory (same mechanism the
post-build copy uses). Add a small helper if JASP already exposes the
resources root; otherwise reuse `QStandardPaths` / app dir.

**RCmdEngine sharing:** Same `_rCmder` as RCommander. Both must check
`_engine->idle()`. No mutex — rely on single-threaded Qt event loop +
idle check (matches RCommander's pattern).

### 4b. QML button  ✅

**Location:** `Desktop/components/JASP/Widgets/AnalysisFormExpander.qml`,
inside `Row { id: buttons }` beside `annotateButton` (~L415).

```qml
MenuButton {
    id:             roboreportButton
    width:          height
    iconSource:     jaspTheme.iconPath + "/roboreport.svg"
    enabled:        expanderButton.expanded
                   && RoboReportManager.hasScript(formParent.myAnalysis.module,
                                                   formParent.myAnalysis.analysisName)
    visible:        myForm ? !myForm.isAnnotated : false
    onClicked:      mainWindow.roboreportAnalysis(formParent.myAnalysis.id)
    toolTip:        qsTr("Generate a RoboReport")
    radius:         height
    opacity:        editButton.opacity
}
```

**Question to resolve:** does `myAnalysis` expose module/name to QML? If
not, either expose them as `Q_PROPERTY` on Analysis, or have the button
always visible and let C++ no-op when no script exists. Cheapest path:
always show, hide via a `Q_INVOKABLE` check after the fact. Decide in 4a.

### 4c. MainWindow integration  ✅

Two changes:

1. `Desktop/mainwindow.cpp` after `AgentStateTracker::init();` (L196):
   ```cpp
   RoboReportManager::init(this);
   ```

2. `Desktop/mainwindow.h` beside `annotateAnalysis()` (L202):
   ```cpp
   Q_INVOKABLE void roboreportAnalysis(int id) { RoboReportManager::runForAnalysis(id); }
   ```

3. Include `roboreport/roboreportmanager.h` in `mainwindow.cpp`.

### 4d. Icon  ✅

`roboreport.svg` in both `QMLComponents/icons/lightTheme/` and
`QMLComponents/icons/darkTheme/`. Blue circular background with a
document + text lines + mini bar chart. Auto-registered via
`file(GLOB_RECURSE ICONS_RESOURCE_FILES ... "icons/*")` in
`QMLComponents/CMakeLists.txt` — no CMake changes needed.

### 4e. Verification  [ ] ← NEXT

End-to-end smoke test once 4a–4d land:
1. Build JASP.
2. Load a dataset, run an Independent Samples T-Test.
3. Click the RoboReport button.
4. Expect: a sibling analysis appears with the composed report
   (markdown + referenced tables), per `TTestIndependent.R`.
5. Watch `rCodeReturnedLog` / log panel for the R script output and any
   `ROBOREPORT_ERROR:` line.

If it fails, the failure mode is observable in the R output stream — no
silent disappearance.

---

## Phase 5: Polish

### Error handling UX  [ ]
- When `rr_call()` throws, the script aborts; the wrapper's `tryCatch`
  emits `ROBOREPORT_ERROR:<msg>` so C++ can detect it.
- C++ should parse for that sentinel on `rCodeReturned` and surface a
  friendly message (toast or failed-state annotation).
- Consider a "RoboReport failed: <error>" annotation instead of silence.

### Script discovery / manifest  [ ]
- Convention: `Resources/roboreport/<module>/<AnalysisName>.R`
- `hasScript()` = `QFile::exists()` on the convention path. Sufficient
  for now; revisit a manifest JSON only if discovery becomes a feature.

### Progress reporting  [ ]
- RCmdEngine streams `rCodeReturnedLog`. Could surface as a status bar
  message during long scripts. Defer until happy path works.

### Sibling title  [ ]
- Ideally titled "RoboReport: <original title>".
- Options: (a) add an `analysis_setTitle` RPC method (small C++ addition
  to `analyses.cpp`), or (b) set via the form mechanism. (a) is cleaner.

### Cleanup of old sibling analyses  [ ]
- Each run creates a sibling that stays in the workspace.
- Options: auto-hide/mark transient, or delete the sibling after
  composing and create a Report instead. Defer until UX is settled.

---

## Phase 6: More Scripts

Priority order (each follows `SCRIPT_GUIDE.md`):

1. [ ] `jaspDescriptives/Descriptives.R` — simple, no assumptions, baseline
2. [ ] `jaspAnova/Anova.R` — between-subjects ANOVA + post-hoc
3. [ ] `jaspRegression/LinearRegression.R` — multiple regression
4. [ ] `jaspTTests/TTestPairedSamples.R`
5. [ ] `jaspTTests/TTestOneSample.R`

---

## Open Questions

1. **Sibling visibility after RoboReport runs.** Currently stays in the
   workspace titled "<Original> (AI)". Hide it? Rename to "RoboReport: …"?
   → Decide alongside sibling-title work (Phase 5).

2. **Multiple siblings per run.** Current contract is one sibling per run.
   Complex reports that aggregate across analyses may want several. The
   RPC supports it (`analysis_create` can be called N times). Defer until
   a script actually needs it.

3. **Re-running RoboReport.** Press the button again → update existing
   report, or create a new one? Needs tracking of which sibling belongs
   to which source analysis. Defer.

4. **`jaspRoboReport` distribution.** ✅ DONE — Installed into its own
   R library (`JASP_ROBOREPORT_LIBRARY`) via `install-tools.R.in`,
   following the `jaspModuleBundleManager` pattern. A `renv.lock`
   captures httr2 + jsonlite + glue + transitive deps. The library is
   added to `R_LIBS` in `processhelper.cpp` so the RCmdEngine finds it
   at runtime. The install rules in `Install.cmake` deploy it
   automatically (copies the whole `Tools/` directory).

5. **QML exposure of analysis module/name.** Needed for button
   enable/visible logic. If not already `Q_PROPERTY`-exposed, decide
   between exposing them or always-showing the button. Resolve during 4b.
