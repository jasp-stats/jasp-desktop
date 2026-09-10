# JASP Desktop — Agent Guide

## Build

```bash
cmake -GNinja -S . -B build -DBUILD_TESTS=ON
cmake --build build --target CommonData    # library target only
cmake --build build                        # everything (slow)
cmake --build build --target JASP          # desktop app only

Add `-DINSTALL_R_MODULES=OFF` to skip building R modules (much faster build, but analyses won't run).
```

- Use the existing `build/` directory — it is already configured.
- Re-run `cmake build/` after adding new `.cpp`/`.h` files (CMake uses `GLOB_RECURSE` in `CommonData/CMakeLists.txt`).
- `librt` is auto-detected and linked by `Tools/CMake/Libraries.cmake`.
- Engine binary lands in `build/Desktop/` alongside the `JASP` executable.

## Tests

Most test targets depend on `JASPDesktopLib` (cannot build independently); `JASPTestColumnEncoderContext` depends only on `Common`.

```bash
cmake --build build --target JASPTest
xvfb-run build/Tests/JASPTest                           # run all (needs xvfb)
xvfb-run build/Tests/JASPTest testSyncerStartStopFileSyncing  # single test by name
ctest -R testDataImport --output-on-failure             # or via ctest
```

Test names (use `-functions` on binary to list all). There are SIX test executables — verify against ALL of them:
- `JASPTest` — data import + syncer tests
- `JASPTestEngine` — engine integration tests
- `JASPTestDebugData`, `JASPTestCsvPrev`, `JASPQuickTest`
- `JASPTestColumnEncoderContext` — encoder indirection/extra-encodings (depends only on `Common`, unlike the others)

To build and run everything in one go:

```bash
cmake --build build --target JASPTest JASPTestEngine JASPTestDebugData JASPTestCsvPrev JASPQuickTest JASPTestColumnEncoderContext
xvfb-run build/Tests/JASPTest
xvfb-run build/Tests/JASPTestEngine
xvfb-run build/Tests/JASPTestDebugData
xvfb-run build/Tests/JASPTestCsvPrev
QT_QPA_PLATFORM=offscreen xvfb-run build/Tests/JASPQuickTest
xvfb-run build/Tests/JASPTestColumnEncoderContext
```

For most tests, use `xvfb-run` (or combine `QT_QPA_PLATFORM=offscreen` with `xvfb-run`). `JASPQuickTest` requires both: `QT_QPA_PLATFORM=offscreen xvfb-run build/Tests/JASPQuickTest`. The test library is at `Tests/TestLibrary/`.

## Library architecture (dependency order)

```
Common → CommonData → QMLComponents → JASPEngine / JASPDesktopLib → JASP
```

| Library | Type | Description |
|---|---|---|
| `Common` | static | Shared utilities, `ColumnEncoder`, logging, JSON |
| `CommonData` | static | Data layer: `DataSet`, `Workspace`, `Column`, `Filter`, SQLite, R bridge |
| `QMLComponents` | static QML module | QML controls, URI `JASP.Controls` |
| `Desktop/JASPDesktopLib` | static | Desktop UI logic |
| `Engine/JASPEngine` | exec | R engine process |
| `SyntaxInterface` | shared | jaspSyntax R bridge lib |

## Key classes

- `DataSet` — owns columns, filters, `ColumnEncoder`, `DataSetSyncer`
- `Workspace` — owns map of datasets, manages shown/filter state
- `DataSetSyncer` — per-dataset file watcher + DB interval syncer (created in `DataSet` ctor)
- `ColumnEncoder` — per-dataset column name encoding singleton with context pointer (`ColumnEncoder::setCurrentEncoder()`)
- `DatabaseConnectionInfo` — DB interval polling timer (owned by `DataSetSyncer`)
- `DataSetPackage` — singleton desktop wrapper around Workspace

## Git notes

- No enforced prefix convention (`feature/`, `bugfix/` not used).
- Bot branches prefixed `bot` (e.g., `botDataSetSynch`, `botDev`).
- Upstream branch: `origin/development`. Forks: `joris/development`, `bruno/development`.

## Qt quirks

- Tests use `QApplication` (Widgets-based), need a display. Use `xvfb-run`.
- `#ifdef NOT_IGNORING_SYNCHING` — never defined anywhere, dead code.
- `FileEvent::FileSyncData` — was dead/never existed, now added to enum.
- `DataSet::setDataFileAndTimeStamp` (overload) exists alongside `setDataFile` (single string).

## Gate test / fuzzer (`Tests/gatetest/`)

- `run_gatetest.sh` → gate test (every analysis, default options, via jasp-mcp MCP layer).
- `fuzztest.py` → schema-guided option fuzzer (optionMeta kinds: checkbox/combo/variables/number/integer/percent/string/array).
- `gatecommon.py` → shared RPC harness (MCP-first with direct-JSON-RPC fallback), JASP process handling.
- JASP headless requires `-platform offscreen` (`-platform minimal`, used by `--hide`, crashes QtWebEngine's scene graph during blocking RPC waits). `--rpcPort=<n>` enables the RPC server at startup (persisted in user settings, like `--safeGraphics`).
- The fuzz/gate harness tolerates `validationError`/`rejected`/`fatalError` outcomes; crash/hang/wedge/`-32603` abort the run with a `.repro.json`.
- `jaspTestModule` is always skipped (dev-only module).

## Debugging JASP desktop crashes — pitfalls learned the hard way

- **lldb batch mode ends supervision at the first stop.** Any breakpoint hit (or attach-SIGSTOP handling quirk) ends the `-k`-scripted session, leaving the target stopped/frozen. Do not use `break set -n abort` style name matching: it resolves to *all* matching symbols (e.g. `Analysis::abort`, hit constantly during fuzzing — 121 locations). Pin exact symbols per library, e.g. `break set -n exit -s libsystem_c.dylib` / `-n __abort_message -s libc++abi.dylib`, and prefer `QCoreApplication::exit` / `QGuiApplication::quit` for clean-exit tracing.
- **Silent clean `exit(0)` is a real death mode** for JASP: Qt's `quitOnLastWindowClosed` default (true) is never disabled, so anything that closes/destroys the main QML window exits the whole app with code 0 — no log output, no signal. The fuzzer's summary filters exit code 0 out (`finish()`), so a clean quit shows up only as "connection refused" transport errors. Check `jasp_proc.returncode` when chasing "process died" reports.
- Attaching lldb mid-run changes timing; crashes may become hangs or clean exits. Prefer launching JASP *under* lldb (`lldb --batch -o "process launch" ...`) and driving it from outside, and let the process run to death under supervision instead of attaching late. Note: lldb batch **attaching** ends supervision after the sourced script finishes — for guaranteed supervision, launch under lldb via a wrapper script (`exec lldb --batch -s script -- real-binary "$@"`) with `process launch` as the first script line and auto-continue breakpoint commands (`breakpoint command add N -o bt -o continue`), and check the *fuzzer's own output* for the real JASP pid (`grep "JASP pid"`); `pgrep -f rpcPort` can match stale leftovers from earlier runs.
- **DYLD_INSERT_LIBRARIES interposition of `exit`/`_exit` via `dlsym(RTLD_NEXT)` recurses into itself** (the interpose table redirects RTLD_NEXT back to the interposer) — stack-overflow SIGSEGVs that poison the experiment. Interpose with the raw `SYS_exit` syscall instead (see the pattern used while chasing the QV4 GC crash; keep the dylib out of the fuzzer's own process expectations — it inherits DYLD_* so its own exits get logged too).
- The mysterious clean-exit(0) fuzz deaths resolved into **multiple real crash modes**: EXC_BAD_ACCESS in `QV4::markDrain` (QtQml GC marking, use-after-free on the QML heap — see Tests/gatetest/README.md), plus one-off SIGBUS. "Connection refused" transport errors mean the desktop process is already gone; `jasp_proc.returncode` tells the death mode (0 filtered by summaries, -6 SIGABRT, -10 SIGBUS, -11 SIGSEGV).
- The R engine processes detect parent death via heartbeat files (`temp/JASP-IPC-<pid>_heartbeat`); "no parent alive" in engine logs means the desktop process already died — check the *desktop* log and exit code.
- The IPC channel is a **single-slot mailbox** with a fixed-width id prefix (4 digits since `f268fa249`); each send overwrites the slot, so fast repeated engine messages can overwrite an unread reply. `processReplies` never reads while the desktop thinks the engine is `idle`.

## Conventions

- `#include` paths: Desktop headers use `data/datasetpackage.h`, CommonData/Common headers use flat `"dataset.h"`.
- When adding tests, add slots to `Tests/testall.h` and implementations to `Tests/testall.cpp` (no standalone test target needed).
- Always add `friend class DataSet;` if constructing `ColumnEncoder` directly.