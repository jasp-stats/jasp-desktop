# Standalone reproducer: two missing-barrier defects in QtQml's incremental GC

Crashes unpatched Qt 6.12.0 (`qtdeclarative`) with a SIGSEGV in the QML JS
garbage collector, or (scenario `weak`) demonstrates the missed-wrapper
condition, using only public Qt API — no JASP, no fuzzer, seconds per run.

```
#0  QV4::Heap::Base::mark()                      (black-bit write into a freed chunk)
#1  QV4::IdentifierHashData::markObjects()       (pushes the dangling hash entry)
#2  QV4::IdentifierTable::markObjects()
#3  QV4::ExecutionEngine::markObjects()
#4  QV4::(anon)::markGlobalObject()              (GC state 1)
    ...  GCStateMachine::transition/step <- MemoryManager::runGC <- allocString
```

(Same defect family as JASP's fuzz-test crash; there the fault point is one
frame deeper — the dangling entry is popped in `MarkStack::drain` and the
zeroed header dereference faults.)

## Build & run

```bash
cmake -B build && cmake --build build
./build/repro intern            # scenario 1: dangling identifier-hash root
./build/repro weak              # scenario 2: weak-values pass misses rooted wrappers
```

Both scenarios share a prelude (a large live heap so a mark phase cannot finish
within one GC slice) and set `QV4_GC_TIMELIMIT=1` internally (1 ms slices, so
the mutator regularly runs mid-cycle; a toy-size heap otherwise completes whole
cycles atomically). The production crashes happen with default settings because
real QML churn dwarfs these toy loops.

## Scenario 1 (`intern`): dangling identifier-hash root

`IdentifierHashData::entries` is a GC root: `IdentifierTable::markObjects()`
marks the `PropertyKey`s stored in every registered hash's `entries` array at
the start of every cycle, and the entries are never pruned. The FIRST intern of
a name is safe (`PropertyKey::fromStringOrSymbol` carries an insertion
barrier). But re-interning an EXISTING name into another hash goes through
`IdentifierTable::asPropertyKey(const QString&)` -> `resolveStringEntry()`,
which finds the existing table entry and returns its identifier WITHOUT any
barrier; `IdentifierHash::addEntry()` then stores it into the new hash's root
array unmarked. When the previous holder hash was destroyed earlier in the same
cycle and the re-intern lands after `MarkGlobalObject` but before `DoSweep`,
the sweep frees the string while the root array keeps the dangling pointer; the
next cycle's marking crashes.

The trigger: `QQmlContext::setContextProperty()` with a name whose previous
holder context was destroyed. Each round interns a fresh name into a
short-lived holder context, then destroys it; an older name is re-interned into
a fresh long-lived context at a randomized delay.

## Scenario 2 (`weak`): the weak-values pass misses rooted wrappers

Early in each cycle the collector walks all QObjectWrappers once
(`MarkWeakValues`) and marks those whose QObject — or the root of its parent
chain — keeps them alive (`QQmlData::keepAliveDuringGarbageCollection`: no
QQmlData, `indestructible`, or `rootObjectInCreation`). A QObject that becomes
rooted AFTER the pass ran in the current cycle (`setParent()` into a keep-alive
chain — routine while QML forms are built) keeps a wrapper the collector never
marks: the destroy phase tears it down and the sweep frees it while the QObject
is still alive and rooted. Under real form churn this is the crash JASP works
around with `installQmlGcRescan` (`Desktop/utilities/qmlgcrescan.cpp`).

The trigger: component-created QML objects (own `keepAlive()` is false once
creation completes; `JavaScriptOwnership` clears `indestructible`) get a
wrapper via a QVariant-based JS store (`newQObject()` would set
CppOwnership->indestructible and make the wrapper unmissable), the JS reference
is dropped so the wrapper is only weakly held, and a random-delayed
`setParent()` under a plain C++ QObject (no QQmlData, hence a permanent
keep-alive anchor) lands after the pass. The rooted victims must outlive one
full GC cycle (a missed wrapper is only freed at that cycle's sweep).

The missed-wrapper condition is made observable by a 10-line instrumentation in
Qt's destroy phase (`InitCallDestroyObjects`): flag every wrapper that is about
to be destroyed while its QObject is rooted under a keep-alive chain — see
`[WEAK-MISS]` in the run output of an instrumented build. Note Qt's own
cross-validator (`QV4_MM_CROSS_VALIDATE_INCREMENTAL_GC=1`) does NOT catch this
defect: it re-marks before the destroy phase, so misses that materialize in the
cross-validate-to-sweep window are invisible to it.

## A/B verification (Qt 6.12.0, x86_64 Linux)

| check                                              | unpatched 6.12.0        | with the two fixes |
|----------------------------------------------------|-------------------------|--------------------|
| `intern`: SIGSEGV in the collector                 | 3/3 runs, ~1 s          | clean 4/4 runs     |
| `intern` + cross-validator                         | 817 "String" errors/run | 0 errors           |
| `weak`: wrappers destroyed while rooted (`[WEAK-MISS]`) | 25 / 25 / 25 per run | 0 / 0 / 0       |
| JASPQuickTest                                      | 180 pass / 4 pre-existing failures | identical |

The fixes (both one-liners in spirit, kept next to this reproducer):

* `Docs/development/qt612-identifierhash-writebarrier.patch` — insertion barrier
  in `IdentifierHash::addEntry` (scenario 1), mirroring the pattern
  `asPropertyKeyImpl()` already uses.
* `Docs/development/qt612-weakvalues-rerun.patch` — re-run the weak-value pass
  to completion at `MarkReady` (scenario 2), i.e. JASP's `installQmlGcRescan`
  logic as a Qt patch. Once Qt ships it, JASP can remove the hook.

Full write-up including the JASP-side evidence: `Docs/development/qv4-gc-crash.md`.
