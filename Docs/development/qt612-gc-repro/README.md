# Standalone reproducer: QtQml GC crash (dangling identifier-hash root)

Crashes unpatched Qt 6.12.0 (`qtdeclarative`) with a SIGSEGV in the QML JS
garbage collector, using only public Qt API — no JASP, no fuzzer, ~1 second.

```
#0  QV4::Heap::Base::mark()                      (black-bit write into a freed chunk)
#1  QV4::IdentifierHashData::markObjects()       (pushes the dangling hash entry)
#2  QV4::IdentifierTable::markObjects()
#3  QV4::ExecutionEngine::markObjects()
#4  QV4::(anon)::markGlobalObject()              (GC state 1)
    ...  GCStateMachine::transition/step <- MemoryManager::runGC <- allocString
```

(Same defect as JASP's fuzz-test crash; there the fault point is one frame
deeper — the dangling entry is popped in `MarkStack::drain` and the zeroed
header dereference faults. Both are the same dangling root.)

## Build & run

```bash
cmake -B build && cmake --build build
./build/repro                # SIGSEGV in ~1 s on unpatched Qt 6.12.0
```

## The bug

`IdentifierHashData::entries` is a **GC root**: `IdentifierTable::markObjects()`
marks the `PropertyKey`s stored in every registered hash's `entries` array at
the start of every cycle (`MarkGlobalObject`), and the entries are never
pruned. So every key stored into a hash must be marked if a GC cycle is
ongoing.

The **first** intern of a name is safe:
`IdentifierTable::asPropertyKey(const QString&)` → `resolveStringEntry()`
creates the string and calls the table's `addEntry()`, which assigns
`str->identifier = PropertyKey::fromStringOrSymbol(...)` — and
`fromStringOrSymbol` contains an insertion barrier (`WriteBarrier::markCustom`)
that marks the string black immediately.

But **re-interning an existing name into another hash** takes the found path in
`resolveStringEntry()`, which returns the existing entry's identifier **without
any barrier**, and `IdentifierHash::addEntry()` then stores that key into the
new hash's root `entries` array unmarked. When that store happens after
`MarkGlobalObject` of an ongoing cycle, the string is white at `DoSweep`:

* it is freed by the allocator sweep, and
* the table's `sweep()` drops it from its own arrays (they are not roots and
  are pruned), while
* the new hash's `entries` array — a root that is never pruned — keeps the
  dangling pointer.

The next cycle's `IdentifierTable::markObjects()` →
`IdentifierHashData::markObjects()` pushes that pointer and the collector
crashes on the freed string.

Three conditions must coincide, which is why the crash is timing-sensitive but
routine under real QML churn (JASP hits it within seconds of
create/destroy-analyses fuzzing):

1. a name that is (re-)interned into a *new* context's name cache while its
   previous holder hash is dead,
2. an ongoing incremental GC cycle that has already run `MarkGlobalObject`,
3. the store happening before that cycle's `DoSweep`.

## How the repro forces those conditions

* Each round interns a **fresh** name into a short-lived holder context, then
  destroys that context: the string loses its only root (the identifier table's
  own arrays are not roots).
* Every round re-interns an **older** name (holder already destroyed) into a
  fresh long-lived context — the unbarriered found-path store.
* Delays are randomized so the re-intern probes every phase of the GC cycle;
  whenever it lands after `MarkGlobalObject` but before `DoSweep`, the round
  produces a dangling root, and the next cycle crashes.
* A prelude grows a live heap so the mark phase cannot finish within one GC
  slice, and `QV4_GC_TIMELIMIT=1` shortens the slices to ~1 ms so the mutator
  regularly runs mid-cycle. (The env var only amplifies the window; the
  production crash happens with default settings because real QML churn is far
  bigger than this toy loop.)

## A/B verification (Qt 6.12.0, x86_64 Linux)

| libQt6Qml                                   | result                     |
|---------------------------------------------|----------------------------|
| stock 6.12.0                                | SIGSEGV, 3/3 runs, ~1 s    |
| 6.12.0 + one-barrier fix (see below)        | clean, 4/4 runs            |

The fix adds the missing insertion barrier where the key is stored into the
root array — `IdentifierHash::addEntry()`, mirroring the pattern
`asPropertyKeyImpl()` already uses:

```cpp
d->entries[idx].identifier = identifier;
if (Heap::StringOrSymbol *storedId = identifier.asStringOrSymbol())
    WriteBarrier::markCustom(d->identifierTable->engine, [storedId](QV4::MarkStack *ms) {
        storedId->mark(ms);
    });
```

Full write-up including the JASP-side evidence: `Docs/development/qv4-gc-crash.md`.
