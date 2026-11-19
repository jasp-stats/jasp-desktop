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

# Follow-up (2026-11-04): Linux / Qt 6.12.0 — root cause found and fixed locally

Reproduced, root-caused **and fixed** on Linux (Arch, `libQt6Qml.so.6.12.0`, gdb).
The crash is a **missing write barrier in Qt's `QV4::IdentifierHash::addEntry`**
(`src/qml/jsruntime/qv4identifierhash.cpp`). JASP-side suspects below were
formally exonerated.

## The mechanism, end to end (proven — corrected after deeper instrumentation)

1. `IdentifierHashData` is a **GC root**: `IdentifierTable::markObjects` marks the
   identifiers stored in every registered hash's `entries` array *every* cycle,
   and the entries are **never pruned** (`IdentifierHashData` has no sweep).
2. Every key stored into that root array must therefore be marked if a GC cycle
   is ongoing. The **first** intern of a name is safe:
   `resolveStringEntry()` creates the string and the table's `addEntry()` assigns
   `str->identifier = PropertyKey::fromStringOrSymbol(...)`, and
   `fromStringOrSymbol` carries an insertion barrier (`WriteBarrier::markCustom`)
   that marks the string black immediately.
3. **The hole is the re-intern**: storing an *existing* identifier into another
   hash. `IdentifierHash::add(QString)` →
   `asPropertyKey(QString, ForceConversionToId)` → `resolveStringEntry()` finds
   the existing table entry and returns its identifier **without any barrier**
   (the barrier lives only in `fromStringOrSymbol`, i.e. on the create path),
   and `IdentifierHash::addEntry` stores that key into the new hash's root
   `entries` array unmarked.
4. When, in the *same* GC cycle, the previous holder hash is destroyed before
   `MarkGlobalObject` and the re-intern lands after it (but before `DoSweep`),
   the string is white at sweep time: the allocator sweep frees it and the
   table's `sweep()` prunes it from the (non-root) table arrays, while the new
   hash's never-pruned root array keeps the dangling pointer.
5. The next GC cycle: `IdentifierTable::markObjects` → `IdentifierHashData::markObjects`
   pushes the dangling identifier → `MarkStack::drain` pops it → header reads
   NULL → `mov 0x10(%rax),%rax` with rax=0 → **SIGSEGV**.

   (JASP's analysis churn does exactly step 4 constantly: the fuzzer destroys
   analysis contexts while `analysis_create` re-interns the same context-property
   names into fresh contexts.)

An instrumented build that logs every store of an unmarked string into a root
hash (`[HASH-STORE-UNMARKED]`) plus a sweep-time detector that checks whether a
dropped table entry is still referenced by a registered hash
(`[DANGLING-ROOT]`) confirmed this end to end: in the standalone repro every
mid-cycle unmarked store became a dangling root one sweep later; in JASP the
same detector fires 8× per repro run before the crash.

## How it was proven (instrumented Qt build)

Built `qtdeclarative` v6.12.0 (RelWithDebInfo, system-Qt ABI swap via
`LD_LIBRARY_PATH`) with temporary instrumentation in `MarkStack::push` and both
`drain()` overloads: print a backtrace whenever a pushed/popped entry reads
`internalClass == NULL` (freed-and-zeroed header). First repro run:

```
[STALE-PUSH] h=0x7ff4885d3c60 internalClass=NULL
  QV4::IdentifierTable::markObjects(MarkStack*)      ← the pusher
  QV4::ExecutionEngine::markObjects(MarkStack*)
  GCStateMachine::transition()                        (markGlobalObject state)
  QV4::MemoryManager::allocString(unsigned long)      ← GC triggered by allocation
  QV4::ExecutionEngine::fromData(QMetaType, ...)      (QVariant→JS string)
  QV4::Function::call / QQmlJavaScriptExpression::evaluate
  QQmlBoundSignalExpression::evaluate                 ← JASP signal → QML handler
[STALE-POP] h=0x7ff4885d3c60 (same entry, later in drain(deadline))
```

The cross-validator confirmed the identity of the missed objects: before the
fix, *every* run reported `Cross Validation Error ... With type "String"`
(4 per run); the missed Strings were exactly the identifier-hash entries.

## The fix (Qt-side, minimal)

Mirror the barrier that `asPropertyKeyImpl` already uses, in
`src/qml/jsruntime/qv4identifierhash.cpp` `addEntry`:

```cpp
d->entries[idx].identifier = identifier;
// Write barrier: entries[] is a root array - IdentifierHashData::markObjects marks
// the stored identifiers every cycle and they are never pruned. An identifier stored
// while a gc cycle is ongoing (allocations are white then, see memory/design.md)
// would otherwise stay unmarked, get swept, and leave a dangling pointer in entries[]
// that crashes the next cycle's marking (MarkStack::drain, NULL internalClass).
if (Heap::StringOrSymbol *storedId = identifier.asStringOrSymbol())
    WriteBarrier::markCustom(d->identifierTable->engine, [storedId](QV4::MarkStack *ms) {
        storedId->mark(ms);
    });
```

(plus `#include <private/qv4writebarrier_p.h>` and
`#include <private/qv4mm_p.h>`; the patch is kept at
`Docs/development/qt612-identifierhash-writebarrier.patch`.)

## Verification

| Run (default GC, deterministic seed 101010148 repro) | Result |
|---|---|
| instrumented Qt, **before** fix | crash within 1 run + `[STALE-PUSH]` naming `IdentifierTable::markObjects` |
| instrumented Qt, **after** fix, 4× | exit 0, 0 stale hits |
| after fix + `QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1` | **0 cross-validation errors** (was 4 per run) |

A broader 5-module fuzzer sweep with the patched lib ran as a regression check.
This supersedes the earlier macOS-era theory ("wrapper already marked, QObject
destroyed between slices"): the actual defect is the identifier-hash write
barrier, which is timing- and platform-independent and explains the SEGV/SIGBUS
variants alike.

## Standalone reproducer (no JASP)

`Docs/development/qt612-gc-repro/` contains a ~290-line public-API reproducer
with two scenarios (see its README) that exercises both defects on stock
Qt 6.12.0:

| check | unpatched 6.12.0 | with the two fixes |
|---|---|---|
| `intern` scenario: SIGSEGV in the collector | 3/3 runs, ~1 s | clean 4/4 runs |
| `intern` + cross-validator | 817 "String" errors/run | 0 errors |
| `weak` scenario: wrappers destroyed while rooted (`[WEAK-MISS]`) | 25 / 25 / 25 per run | 0 / 0 / 0 |
| JASPQuickTest | 180 pass / 4 pre-existing failures | identical |

It sets `QV4_GC_TIMELIMIT=1` internally to widen the incremental-GC window
(with the default ~5 ms slice budget a toy-size heap completes whole cycles
atomically and the mutator never runs mid-cycle); the production crash needs no
env vars because real QML churn dwarfs the toy loop. This reproducer is the
artifact to attach to the Qt bug reports.

## Second defect: the weak-values pass misses QObjects rooted mid-cycle

Qt's incremental GC decides QObjectWrapper keep-alive once per cycle
(`MarkWeakValues`, early): a wrapper is kept if its QObject — or the root of
its parent chain — satisfies `QQmlData::keepAliveDuringGarbageCollection` (no
QQmlData, `indestructible`, or `rootObjectInCreation`). A QObject that becomes
rooted *after* the pass ran in the current cycle (parented into a keep-alive
chain while a form is built) keeps a wrapper the collector never marks: the
destroy phase tears it down and the sweep frees it while the QObject is still
alive and in use. This is the defect JASP's `installQmlGcRescan`
(`Desktop/utilities/qmlgcrescan.cpp`, upstream #6372) works around by re-running
the weak pass to completion at `MarkReady`.

It is independent of the identifier-hash defect: different structure
(`PersistentValueStorage` weak slots vs `IdentifierHashData::entries`), different
phase, and each fix leaves the other defect untouched — JASP crashed on the
identifier-hash defect *with* the rescan installed, and the reproducer's `weak`
scenario produces missed wrappers with the identifier-hash barrier in place.
Both share only the crash site (`MarkStack::drain`), which is why they were easy
to conflate.

Fix (Qt-side, one block in `markReady`):
`Docs/development/qt612-weakvalues-rerun.patch` — re-run
`InitMarkWeakValues` + `MarkWeakValues` to completion at `MarkReady`, then hand
back a fresh slice so `InitCallDestroyObjects` redrains before anything is
destroyed. Verified with the reproducer's `weak` scenario: an instrumented
build flagging wrappers destroyed while their QObject is rooted reports
25/25/25 misses per run unpatched and 0/0/0 with the patch; JASPQuickTest is
identical to stock. Note Qt's own cross-validator does **not** detect this
defect (it re-marks before the destroy phase, so misses materializing in the
cross-validate-to-sweep window are invisible to it) — the JASP hook's
cross-validate numbers (48-137 missed objects per form creation -> 0) measured
the earlier part of the window. Once Qt ships the patch, JASP can remove
`installQmlGcRescan` (as its header already anticipates).

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