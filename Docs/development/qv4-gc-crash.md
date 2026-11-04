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

---

# Follow-up (2026-11-04): Linux / Qt 6.12.0 — root cause pinned, exonerations recorded

Reproduced and root-caused on Linux (Arch, `libQt6Qml.so.6.12.0`, gdb available).
The crash is now pinned to **Qt's incremental GC wrongly freeing live objects**,
with Qt's own cross-validator as the witness. JASP-side suspects below were
formally exonerated.

## Forensics (assembly level, from a coredump)

```
#0  QV4::MarkStack::drain(QDeadlineTimer)   libQt6Qml.so.6.12.0
    si_addr = 0x10, SEGV_MAPERR
    faulting instruction: mov 0x10(%rax),%rax   with rax = 0
```

Mapped against Qt 6.12.0 `qv4mm.cpp` (release build, `Q_ASSERT` compiled out):

```
Heap::Base *h = pop();                       // stale entry from the mark stack
h->internalClass->vtable->markObjects(h, this);
//   ^ load A succeeds: h points to READABLE memory whose header reads ZERO
//   ^ load B faults:  internalClass == NULL, vtable load = [NULL + 0x10]
```

So the popped mark-stack entry pointed into **freed-and-zeroed QV4 heap**
(decommitted chunk, recommitted as zeros) — a stale strong reference reached
during marking, not "just" a missed live wrapper.

## The witness: Qt's own cross-validator

`QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1` re-runs the full mark phase after the
incremental one and diffs the black bitmaps. Every repro run reports:

```
Cross Validation Error on chunk 116 on bitmap piece 21 and bit 1  With type "String"
Cross Validation Error on chunk 116 on bitmap piece 21 and bit 3  With type "String"
...
```

i.e. **the incremental mark phase misses live `String` objects**; the sweep then
frees them while still referenced; a later GC cycle marks through the dangling
reference into zeroed memory. With the validator on, the fresh (complete) bitmap
is what the sweep consumes — which is why it also *prevents* the crash (0/3).
JASP's `installQmlGcRescan` (`Desktop/utilities/qmlgcrescan.cpp`, upstream) only
re-runs the weak-values pass before the destroy phase — a sibling bug in a
different phase — and indeed does **not** prevent this crash.

## Experiment matrix (faithful RPC cadence, see "Repro recipes")

| Configuration | Deaths |
|---|---|
| default GC (incremental, ~5.5 ms) | 3/3, 3/3, 2/3, 3/3 |
| `QV4_GC_TIMELIMIT=1` (tiny slices) | 3/3 |
| `QV4_GC_TIMELIMIT=1000000` (≈ one-turn cycle) | 6/7 |
| `QV4_GC_TIMELIMIT=0` (synchronous mode) | **0/8** |
| `QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1` | **0/3** (+ missed-String reports) |

Notes:
- `QV4_GC_TIMELIMIT=0` behaves as a *practical* workaround here, but the
  mechanism is not "the bug needs slicing": sweeps still run (10 GC cycles
  observed in one surviving trial). Treat it as timing/interleaving luck until
  Qt says otherwise — do not recommend it as a fix. (The older macOS note below
  claims TIMELIMIT=0 "recurred" on Qt 6.11.1; version behaviour differs.)
- `QV4_GC_TIMELIMIT` was **never required** for the reproduction; the earlier
  "minimal repro" command included it needlessly.

## Wedge variant (observed once, explained)

One fuzzer run showed a different surface: the desktop stayed **alive but
unresponsive** for ~25 s (HTTP listener still open per `ss`, but requests timed
out — main thread confirmed stuck inside the GC traversal), then died with the
same SIGSEGV in `MarkStack::drain`. Interpretation: after the wrongful sweep,
marking walks a corrupted graph — garbage-but-plausible internal classes keep
the traversal alive (slow wedge) until it finally hits NULL (segfault). The
fuzzer previously misreported this as a run-level "systemic create regression";
it now classifies the eventual death as a crash (see below).

## Exonerations (all with controlled experiments, same cadence)

| Suspect | Experiment | Verdict |
|---|---|---|
| `ControlErrorMessage` ad-hoc items (raw-pointer `_controlErrorMessageCache` never pruned, unguarded walks in 6 sites, queued `invokeMethod`, QML write-backs during teardown) | churn with **zero** validation errors (boolean-only batches, `[ErrItem]` instrumentation) | **not the vehicle** — died 1/3 with 0 error items; also present-but-surviving 16-item runs. Still sketchy code worth a separate cleanup. |
| The fuzzer's dict-repr combo junk (`restrictedModelComparison` = `"{'label': ..., 'value': ...}"`) | FULL-minus-RMC 2/3, SANE_RMC 3/3, ONLY_RMC 2/3 | **not the trigger** — but it *was* a real fuzzer bug (fixed: combos now send the canonical `value` string). |
| Sweep-context "accumulation" (crash needs many analyses first) | single controlled sequence dies on first trial | **refuted** — one Anova×3 prefix + one RM batch + the create/remove cadence suffices. |
| `QV4_GC_TIMELIMIT=1` as amplifier/condition | matrix above | **not a condition**; default GC dies too. |

## Repro recipes (Linux, Qt 6.12.0)

One-liner (deterministic, ~3 s, dies in run 2 of `AnovaRepeatedMeasures`):

```
Tests/gatetest/.venv/bin/python Tests/gatetest/fuzztest.py \
    --module jaspAnova --runs-per-analysis 3 --timeout-per-run 90 --seed 101010148
```

(The fuzzer's combo generator changed, which shifts the RNG stream — if this
seed stops dying, any seed that reaches an `AnovaRepeatedMeasures` run after a
rejected run works; the crash is option-content-independent.)

Controlled cadence (what the fuzzer does, distilled): create `Anova` (outer) →
for 3 recorded batches: create fresh `Anova` → `analysis_run` (rejected) →
`analysis_remove` → remove outer → create `AnovaRepeatedMeasures` (outer) →
create fresh → run the recorded 38-option batch (rejected) → remove → create
probe → remove → remove outer. Dies ~1/3–3/3 per trial depending on GC mode.

## For the Qt bug report

- Qt 6.12.0, Linux/x86-64, release libQt6Qml.
- `QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1` output: live `String` objects missed
  by the incremental mark phase (several per GC cycle under QML churn).
- Crash: `QV4::MarkStack::drain` at `mov 0x10(%rax),%rax`, rax=0 — stale
  mark-stack entry pointing to freed-and-zeroed heap; assembly mapped to the
  `h->internalClass->vtable` load with the asserts compiled out.
- Repro vehicle: JASP + the option fuzzer recipe above (analysis form
  create/run/remove churn); wedge variant documented.
- Workarounds that empirically stop it: cross-validator mode (correct, 2× mark
  cost), `QV4_GC_TIMELIMIT=0` (timing-dependent, not a principled fix).