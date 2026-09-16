# QtQml GC crash (`QV4::markDrain`) — investigation write-up

*Status: root-caused as a Qt/QML GC-heap lifetime bug; not fixed in JASP (would need a Qt-side change or a deeper ownership rework). All the JASP-side mitigations we *could* safely make are committed. This document is the complete record in case we want to file a Qt issue or revisit the fix later.*

## Crash signature

```
Process ... stopped
* thread #1, name = 'CrBrowserMain', queue = 'com.apple.main-thread'
    stop reason = EXC_BAD_ACCESS (code=1, address=0x10)      # or code=2, garbage/mangled addresses
    frame #0: QtQml`QV4::(anonymous namespace)::markDrain(QV4::GCStateMachine*, std::__1::variant<std::__1::monostate, QV4::GCIteratorStorage>&) + 232/236
    frame #1: QtQml`QV4::GCStateMachine::transition() + 1612
    frame #2: QtCore`QObject::event(QEvent*)
    ...
    frame #14: JASP`main at main.cpp (QCoreApplication::exec)
```

Observed manifestations (same root cause, different disguises depending on timing):

| Surface | Exit code |
|---|---|
| SIGSEGV | `-11` |
| SIGBUS | `-10` |
| SIGABRT (a sibling assert path, fixed separately) | `-6` |
| "Clean" exit 0 (Chromium's shutdown detector converts delivered signals to `exit(0)`; fuzzer summaries filter code 0 out — which is why it looked like a mysterious silent exit) | `0` |

The crash is on the main thread; QtWebEngine merely *renames* it (`CrBrowserMain`), it does not run the GC.

## Trigger condition (reproducible with the option fuzzer)

```
Tests/gatetest/fuzztest.py --module jaspMetaAnalysis --runs-per-analysis 4 --seed 0 --report /tmp/repro.json
```

Any RPC churn that does `analysis_remove` + a fresh `analysis_create` re-binding the same
analysis type, with results processing in between, at high rate. Roughly 60–70% of
shard runs die; position within the shard varies. It also appeared during the full
271-analysis sweep (first sweep died at 158/271 from the same root cause).

Not limited to the fuzzer: the same churn pattern (rapid analysis add/remove while
previous runs' results are being processed in the QML side) is reachable from a fast
user.

## What the crash is (verified)

- Qt **6.11.1** (Homebrew), `QV4::GCStateMachine::transition()` runs the GC **incrementally**
  by default: ~5 ms slices (`QV4_GC_TIMELIMIT` defaults to `(1000/60)/3` µs), suspended
  mid-cycle via a queued `mm->onEventLoop()` re-invocation (`src/qml/memory/qv4mm.cpp`).
- JASP destroys C++-backed QML objects (whole `AnalysisForm` item trees, controls, list
  models) from the same event loop at arbitrary points — most aggressively when
  `analysis_remove` lands while an R run is still in flight and a fresh `analysis_create`
  immediately re-instantiates the form.
- Qt's wrapper invalidation (`markWeakValues` → `keepAliveDuringGarbageCollection`,
  `callDestroyObject`, `cleanupDeletedQObjectWrappersInSweep`) handles wrappers discovered
  *unmarked* mid-cycle, but there is **no re-validation for wrappers already marked** when
  their QObject is destroyed between slices. The next `markDrain` slice dereferences the
  freed object. The Qt source comment in `redrainDuringSweep` explicitly acknowledges
  mid-cycle mutation hazards ("Especially when we call user code triggered by
  Component.onDestruction, but also when we run into a timeout").

## Why it confused us for so long

- Silent, no log output; the process just vanishes.
- Multiple disguises (SEGV/BUS/ABRT/exit-0) depending on timing.
- The fuzzer initially filtered exit code 0 out of summaries, so clean exits surfaced only
  as "connection refused" transport errors.
- Two debugging traps cost us time (now documented in `AGENTS.md`): lldb *attach* ends
  supervision after the sourced script finishes (use launch-under via a wrapper script
  with `process launch` + auto-continue breakpoints), and DYLD interposition of
  `exit/_exit` via `dlsym(RTLD_NEXT)` recurses into itself (use the raw `SYS_exit`).
- Flaky across identical runs (dies ~60–100% of shard runs depending on machine load),
  which defeated "passed twice in a row" style verification several times.

## Experiments that ruled things out

| Experiment | Result |
|---|---|
| `QML_DISABLE_DISK_CACHE=1` | no effect (2/2 still crashed) |
| `QV4_GC_TIMELIMIT=0` (atomic, non-incremental GC) | appeared to fix (3/3 clean) but crash recurred in later supervised runs — **the stale object is marked regardless of slicing** |
| Deferred teardown: `deleteLater` for Analysis + AnalysisForm instead of synchronous deletes | crash persists; kept anyway (correct pattern, see commit `032c74952`) |
| `QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1` | not yet tried — Qt's own mark-bitmap validator; would confirm the mark-state inconsistency and print `"Cross Validation Error on chunk ..."` |
| Guarding wrapper invalidation from JASP side (e.g. `QQmlEngine::setObjectOwnership(CppOwnership)` on the form tree) | not attempted; ownership is parent-chain based today |

## What we fixed anyway (committed, all verified)

Commit `032c74952` (bundle) and `6dd3f080a`:

1. **Results web view memory leak** (`Desktop/html/js/analyses.js`): `removeAnalysis`'s
   jQuery animation callback assigned `this.analyses = _.without(...)` with `this` =
   the animated DOM element, so the collection never shrank — every removed analysis
   (view + model + results, base64 plots included) leaked for the web view's lifetime.
   This is almost certainly the 35 GB RAM climb seen during long fuzz sweeps.
   Fixed by filtering the real lists (`analysesVar.analyses` / `analysesVar.views`) and
   calling the idempotent base `analysis.close()` (guarded by `_isClosed`).
2. **Deferred teardown** on the analysis removal path (`Analyses::removeAnalysis` /
   `Analyses::clear` / `AnalysisBase::destroyForm` / `~Analysis`): `deleteLater` instead
   of synchronous deletes of live QML item trees; `~Analysis` deletes the form
   synchronously only at the deferred quiet point (after the delegate's JS references
   are invalidated) so `AnalysisForm::analysis` cannot dangle.
3. **Stale-reply null-deref guard** in `EngineRepresentation::processAnalysisReply`.
4. **Tree-RSS watchdog** in the fuzzer (`--max-tree-rss-mb`, default 6000): samples
   desktop + QtWebEngine helpers + R engines and kills the tree before the machine
   chokes. Note `ps -o rss=` on macOS reports KB; WebEngine helpers are separate
   processes and must be included (early probes missed them and showed only ~2 GB).

Also related but distinct (documented in the fuzzer README): unbounded R simulation
loops (`jaspLearnBayes` with extreme `pointsToWin`), and a dispatcher hang with
`jaspVisualModeling/mixedmod` plot options.

## If we later want to file / fix it

- **Qt issue content we already have**: Qt 6.11.1, full `markDrain` backtrace, the
  mechanism (wrapper already marked, QObject destroyed between slices), and a repro
  recipe (option fuzzer, seed 0, jaspMetaAnalysis shard). The Qt source's own comment
  in `redrainDuringSweep` suggests they know this class of hazard exists.
- **Possible JASP-side deeper fix** (bigger than what we shipped): force GC to a quiet
  point *before* destroying the form tree, e.g. `QQmlEngine::collectGarbage()` +
  waiting for it, before `analysis->remove()`; or restructure so form trees are never
  freed while delegates/`Connections` still hold them (explicit
  `setObjectOwnership(..., CppOwnership)` plus nulling `myForm`/`myAnalysis` in QML on
  removal).
- **Possible quick regression test**: the fuzz shard above with `--seed 0` is the
  repro; a fix could be validated with `for i in 1..6; fuzztest.py --module
  jaspMetaAnalysis ...` (pre-fix: ~5/6 die; post-fix expectation: 0).
- **Qt 6.12 check**: worth building against 6.12 once available and re-running the
  shard; the GC state-machine code has been actively reworked upstream.