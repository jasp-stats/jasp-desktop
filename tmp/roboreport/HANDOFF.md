# RoboReport — C++/JASP Handoff

> **Scope:** finishing the **C++ / JASP-desktop side** of RoboReport. The R
> side (scripts + the `jaspRoboReport` package) is mature and validated.
> Read this top-to-bottom before touching C++.
>
> Companion docs in this folder: `SCRIPT_GUIDE.md` (how to write scripts —
> the "study the AI annotation first" workflow, the `.meta` element-naming
> rule, validation playbook), `TODO.md` (full task ledger with file:line
> integration notes), `IMPLEMENTATION.md` (design history).

---

## 0. TL;DR — script resolution (RESOLVED)

> **Status: fixed.** The old module-vs-`Resources` *shadowing* bug is gone —
> the scripts now ship with their module and there is a single, well-defined
> resolution order. `Resources/roboreport/` was brought back only as the
> **lowest-priority** location (a demo fallback), so it can never shadow the
> module copy again.

Script resolution order (first match wins), implemented in
`RoboReportManager::_resolveScriptPath()`:

```
1. <appData>/roboreport/<module>/<Analysis>.R        ← user / dev override (wins)
2. <module>/inst/scripts/roboreport/<Analysis>.R     ← ships with the module
3. Resources/roboreport/<module>/<Analysis>.R        ← app-bundled demo fallback (lowest)
```

`<appData>` is JASP's roaming app-data dir (Linux: `~/.local/share/JASP/JASP`;
see the `jaspRoboReport` README for macOS/Windows). The app-data override is
the **recommended development workflow**: drop a script there to shadow the
module copy with no module reinstall and no JASP rebuild — the next button
click picks it up. Both `jaspTTests` scripts (frequentist + Bayesian) now live
in the module's `inst/scripts/roboreport/`, so the button runs the current
script for each. See Task 2 and Gotcha #1.

---

## 1. Current state

**Done & validated:**
- `RoboReportManager` C++ singleton (`Desktop/roboreport/`) — resolves
  scripts, ensures the RPC server + shared RCmdEngine, fires scripts, streams
  output, surfaces errors via `MessageForwarder::showWarning()`.
- QML button + icons; `MainWindow` integration (`init()` + `Q_INVOKABLE`).
- `jaspRoboReport` R package (RPC client, RDS adapter, formatters, pipeline
  helpers) — installed into its own R library and on `R_LIBS`.
- **Two working scripts**, validated end-to-end against live analyses:
  - `Resources/roboreport/jaspTTests/TTestIndependentSamples.R` (frequentist)
  - `Resources/roboreport/jaspTTests/TTestBayesianIndependentSamples.R` (Bayesian)
- **`$export` field on `jaspPlot`** — code-complete and **validated**: plot
  functions tag machine-readable data (posterior median, CIs, BF-across-priors);
  it flows through both the JSON results and the RDS and survives
  `JASP_RDS_STRIP`.

**Git status (branch `roboreport`, jasp-desktop) — much is UNCOMMITTED:**
```
 M Desktop/roboreport/roboreportmanager.cpp
 M Engine/jaspRoboReport/NAMESPACE
 M Engine/jaspRoboReport/R/{format,results,rpc-client,utils}.R
 M Resources/roboreport/jaspTTests/TTestIndependentSamples.R
?? Resources/roboreport/jaspTTests/TTestBayesianIndependentSamples.R
?? tmp/                                   (these docs)
```

**`$export` spans THREE repos, none committed:**
- `jasp-desktop` — `Engine/jaspBase/` is a **submodule**; bump its pointer
  *after* jaspBase is committed. (Do not edit the submodule in place.)
- `/home/sp42/jaspBase/` — `src/jaspPlot.{h,cpp}` (`_export` field,
  `setExport`/`getExport`, JSON + RDS + state save/restore) and
  `R/resultsRdsHelpers.R` (strip logic).
- `/home/sp42/modules-registry/Official/jaspTTests/` — `R/commonbayesianttest.R`
  tags `attr(plot, "machineRead_export")` → `plot$export` in the three Bayesian
  plot functions; `inst/scripts/` (untracked) holds the shadowing script copy.

---

## 2. C++/JASP tasks to finish (priority order)

### Task 1 — Commit & reconcile `$export` across the three repos  ⚠️ at-risk
The feature works but is uncommitted in three places; losing any working tree
breaks the Bayesian script. Order:
1. Commit jaspBase (`/home/sp42/jaspBase/`).
2. Bump the `Engine/jaspBase/` submodule pointer in jasp-desktop to that commit.
3. Commit the jaspTTests `$export` tagging.
4. Commit the jasp-desktop working-tree changes (manager, R package, scripts).

### Task 2 — Fix the script-shadowing duplication (Gotcha #1)  ✅ DONE
Resolved with **Option A (module-first)** plus a dev override:
- Scripts ship in the module's `inst/scripts/roboreport/` (both the frequentist
  and Bayesian `jaspTTests` scripts were moved there; the stale frequentist
  copy was overwritten with the fresh version).
- `Resources/roboreport/` was re-introduced as a **priority-3 (lowest)**
  app-bundled fallback for **demos** — it can ship a script for an analysis
  whose module ships none, but never overrides the module copy or the app-data
  override.
- A **priority-1 user override** was added at
  `<appData>/roboreport/<module>/<Analysis>.R` — the recommended way to iterate
  on a script during development without reinstalling the module. See the
  `jaspRoboReport` README ("Where scripts live").

### Task 3 — Sibling titling: add an `analysis_setTitle` RPC method
Goal: title the generated sibling "RoboReport: <original title>".
- `Analysis::setTitle(const std::string&)` **already exists**
  (`Desktop/analysis/analysis.h:127`); `write_report` already calls it
  (`analyses.cpp:1749`). Only the **RPC exposure is missing** — there is no
  `analysis_setTitle` method today.
- RPC handlers are registered in `Desktop/analysis/analyses.cpp` via
  `disp->registerMethod("name", [](const Json::Value& params) -> Json::Value {...})`
  (see `write_report` at ~L1697 for the pattern). Add, beside it:
  ```cpp
  disp->registerMethod("analysis_setTitle", [](const Json::Value& params) -> Json::Value
  {
      Analyses* ans = Analyses::analyses();
      int id = params["analysisId"].asInt();
      Analysis* a = ans->get(static_cast<size_t>(id));
      if (!a)
          return JaspRpcDispatcher::errorResult("Analysis not found: " + std::to_string(id));
      a->setTitle(params["title"].asString());
      Json::Value r; r["status"] = "success"; r["analysisId"] = id; r["title"] = a->title();
      return r;
  });
  ```
- Add the method to `Resources/JASP_RPC.json` (params: `analysisId`, `title`).
- Add an `rr_analysis_set_title()` wrapper in `jaspRoboReport/R/rpc-client.R`,
  and call it from the manager after a successful compose (or from scripts).

### Task 4 — Sibling lifecycle / cleanup
Each run creates a sibling that stays in the workspace. Decide:
- **Visibility:** hide / mark transient / rename (pairs with Task 3).
- **Alternative path:** an `analysis_createAnnotation` RPC already exists — it
  creates a read-only-form annotation of the source. Consider using it instead
  of sibling + `analysis_composeResults` if a frozen-form annotation is the
  desired UX.
- **Re-run behavior:** pressing the button again — update the existing report
  or create a new one? Needs tracking of which sibling belongs to which source.

### Task 5 — Progress reporting
RCmdEngine streams `rCodeReturnedLog`. Surface it as a status-bar / progress
message during long scripts (the manager already re-emits it as
`scriptOutput`). Defer until the happy path is settled.

### Task 6 — (Optional) script dropdown
When multiple scripts exist per analysis, present a chooser. The manager
already parses a `Name:` field from each script header
(`_parseScriptMetadata`) — use it for the menu label.

---

## 3. Architecture quick-reference

```
QML button → mainWindow.roboreportAnalysis(id)
  → RoboReportManager::runForAnalysis(id)        [Desktop/roboreport/]
      resolve (module,analysis) → script path     (module pkg first, Resources fallback)
      ensure RPC server listening; ensure shared RCmdEngine (idle-guarded)
      build typed R: jaspRoboReport::run_script(path, id, host, port)
      engine->runScriptOnProcess(wrapper)
  → R: run_script() sources script → roboreport_main(analysisId)
      rr_get_options → rr_create_and_run (sibling) → rr_results (RDS)
      build el_md()/el_ref() elements → rr_compose_results(sibling)
  → workspace: sibling shows the composed report
```

**Signal / error gotchas (already fixed in the working tree, don't regress):**
- RCmdEngine emits **`rCodeReturnedLog`**, *not* `rCodeReturned`, on
  completion. Completion detection lives on `rCodeReturnedLog`. Getting this
  wrong left `_activeId` set forever and blocked repeat clicks.
- Errors: the engine captures R errors via `jaspRCPP_getLastErrorMsg()` and
  reports `hasError=true` with the message. No sentinel hack — scripts either
  succeed or `stop("message")`. The manager shows a `MessageForwarder` warning.
- Engine is shared with RCommander; guard with `_engine->idle()` (single-
  threaded Qt event loop, no mutex — matches RCommander).

**Manager public surface** (`roboreportmanager.h`):
`manager()`, `init(parent)`, `runForAnalysis(id)`, `hasScript(module,analysis)`;
signals `scriptStarted`, `scriptOutput`, `scriptFinished(id, success, errorMsg)`.

---

## 4. Repos & key files

```
jasp-desktop/                              (branch: roboreport)
├── Desktop/roboreport/roboreportmanager.{h,cpp}   ← C++ singleton
├── Desktop/components/JASP/Widgets/AnalysisFormExpander.qml  ← button
├── Desktop/mainwindow.{h,cpp}             ← init + Q_INVOKABLE passthrough
├── Desktop/utilities/processhelper.{h,cpp}← JASP_RESULTS_RDS + R_LIBS
├── Desktop/analysis/analyses.cpp          ← RPC handlers (registerMethod); add setTitle here
├── Desktop/rpc/jasprpcdispatcher.{h,cpp}  ← dispatcher (registerMethod / _handlers)
├── Engine/jaspRoboReport/                 ← R package (RPC client + RDS adapter)
├── Engine/jaspBase/                       ← SUBMODULE — bump pointer, don't edit
├── Resources/roboreport/jaspTTests/*.R    ← scripts (fallback location)
├── Resources/JASP_RPC.json                ← RPC spec (add analysis_setTitle)
└── QMLComponents/icons/{dark,light}Theme/roboreport.svg

/home/sp42/jaspBase/                       ← jaspPlot $export (commit, then bump submodule)
/home/sp42/modules-registry/Official/jaspTTests/
    ├── R/commonbayesianttest.R            ← $export tagging
    └── inst/scripts/roboreport/           ← shadowing script copy (Task 2)
```

---

## 5. Validation playbook

1. **Build JASP**, load a dataset, run an Independent Samples T-Test.
2. **Click the RoboReport button.** Expect a sibling with the composed report
   (Abstract → Descriptives+raincloud → Assumption Checks → Inferential Tests
   → Conclusion). Watch the log for R output / errors.
3. **Make sure you look at the FRESH annotation** — each press creates a *new*
   sibling, so an older exported `analysis_N.html` shows an older script and
   will mislead you. (This stale-export confusion is how the shadowing bug
   stayed hidden.)
4. **MCP note:** the `jasp_analysis_composeResults` MCP tool can be
   unavailable and direct HTTP to the RPC port is sandbox-blocked. The script
   composes over HTTP at *runtime* (independent of MCP), so the button is the
   definitive end-to-end test. For offline validation, parse-check the script
   and simulate the prose builders standalone (see `SCRIPT_GUIDE.md` §Validating).

---

## 6. Open decisions

1. ~~**Canonical script location**~~ — **RESOLVED** (Task 2): module
   `inst/scripts/roboreport/` is canonical, with a priority-1 user/dev override
   at `<appData>/roboreport/<module>/<Analysis>.R`. `Resources/` is gone.
2. **Sibling vs annotation** — keep sibling + `composeResults`, or switch to
   `analysis_createAnnotation` (Task 4).
3. **Re-run semantics** — update existing vs create new (Task 4).
4. **Version enforcement** — script `Version:` constraint is checked but
   non-blocking (log only). Keep, or make it block?

---

## 7. Gotchas

1. **Script override shadowing** — `<appData>/roboreport/<module>/<Analysis>.R`
   (priority 1) silently wins over the module's `inst/scripts/roboreport/` copy
   (priority 2) and the app-bundled `Resources/roboreport/` demo fallback
   (priority 3). Great for development, but **remove your override when done**
   or it keeps shadowing the shipped script. See Task 2.
2. **`el_ref` names** — pass the exact `.meta` `name` (full underscore path for
   nested elements, e.g. `ttestDescriptives_table`), discovered by running the
   analysis. Never guess. (Details in `SCRIPT_GUIDE.md`.)
3. **`Engine/jaspBase/` is a submodule** — commit jaspBase separately, then
   bump the pointer; don't edit the submodule working tree.
4. **`$export` is uncommitted in three repos** — Task 1 before anything else.
5. **RCmdEngine completion is `rCodeReturnedLog`** — don't move completion
   detection back to `rCodeReturned`.
