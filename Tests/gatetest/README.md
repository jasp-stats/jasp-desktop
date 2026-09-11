# JASP Gate Test

Post-build/deploy smoke test: launches JASP headless, loads the standard
`debug.csv`, then **creates, runs and removes every analysis of every loaded
module** through the agent API, verifying each analysis reaches status
`complete` with non-error results.

The test drives JASP **through [jasp-mcp](https://github.com/jasp-stats/jasp-mcp)**
(the MCP layer AI agents use), as an MCP client. Every step that fails on the
MCP layer is automatically retried over direct JSON-RPC, so the report shows
immediately whether JASP itself or the MCP layer is broken:

| MCP   | direct | classification                     |
|-------|--------|------------------------------------|
| OK    | —      | pass                               |
| fail  | OK     | **MCP layer broken** (JASP OK)     |
| fail  | fail   | **JASP broken**                    |

It also validates:

- `rpc_discover` exposes every method the agent stack relies on
- `data_load` on `Resources/Data Sets/debug.csv` yields the expected shape
- default options returned by `analysis_create` are non-empty
- the workspace is empty again afterwards (`analysis_remove` works)
- the JASP process stays alive for the whole run (crash => immediate abort)

## Usage

```bash
# everything (can take a long while — hundreds of analyses)
Tests/gatetest/run_gatetest.sh

# one module only (good smoke test)
Tests/gatetest/run_gatetest.sh --module jaspTTests

# stop at the first failure, write a JSON report
Tests/gatetest/run_gatetest.sh --fail-fast --report /tmp/gate.json
```

`run_gatetest.sh` builds the `JASP` target if `build/Desktop/JASP` is missing,
creates `.venv` with `mcp` + `httpx`, installs the pinned `jasp-mcp` submodule,
and runs `gatetest.py`. Set `GATETEST_SCRIPT=fuzztest.py` to run the option
fuzzer instead (the venv/setup is shared).

## Running through CTest

With `-DBUILD_TESTS=ON` two smokes are always registered (both drive
`jaspTTests` only, a few minutes):

```bash
ctest -R "gateSmoke|fuzzSmoke" --output-on-failure
```

The full sweep is opt-in, since it runs for hours:

```bash
cmake -DBUILD_GATETEST=ON build/   # registers the gateTest target
ctest -R gateTest --output-on-failure
```

## Prerequisites

- Built JASP binary (`cmake --build build --target JASP`)
- `git submodule update --init Tests/gatetest/jasp-mcp`
  (without it the run completes over direct JSON-RPC but is flagged as failed,
  since the MCP layer is part of what the gate verifies)

## Options (gatetest.py)

| flag | default | meaning |
|---|---|---|
| `--jasp-bin` | `build/Desktop/JASP` | JASP executable to launch |
| `--port` | free port | port for the RPC server (`--rpcPort` is passed to JASP) |
| `--csv` | `Resources/Data Sets/debug.csv` | dataset to load |
| `--module` | all | restrict to module(s), repeatable |
| `--skip` / `--skip-file` | none | skip a module (`jaspFoo`) or analysis (`jaspFoo/Bar`) |
| `--startup-timeout` | 300 s | wait for JASP + RPC server to come up |
| `--timeout-per-analysis` | 120 s | per-analysis budget (blocking calls are capped at 55 s and polled, because jasp-mcp's HTTP client times out at 60 s) |
| `--fail-fast` | off | stop on first failing analysis |
| `--no-mcp` | off | bypass the MCP layer entirely (debugging the fallback path) |
| `--report` | none | write JSON summary (counts + all failure details) |

`jaspTestModule` is always skipped (dev-only module).

## How it works

1. Launches `JASP -platform offscreen --rpcPort=<port>` (`--rpcPort` enables the
   RPC server on startup; note this is persisted in JASP settings, same as
   `--safeGraphics`). `-platform offscreen` is mandatory: with `-platform
   minimal` (as used by `--hide`) QtWebEngine crashes the scene graph during
   the blocking `data_load`/`analysis_run` waits.
2. Waits for `ping` over direct JSON-RPC.
3. Starts `python -m jasp_mcp` (stdio MCP server) and connects as MCP client;
   `tools/list` proves dynamic discovery works.
4. `modules_list` → every `(module, analysis)` pair.
5. `data_load` with `delimiter: ","`.
6. Per analysis: `analysis_create` → `analysis_run {options:{}, wait:true}` →
   poll `analysis_results` while `status=="running"` → require `complete` and
   no `results.error` → `analysis_remove`.
7. Close-out: `analyses_list` must be empty.
8. Summary; exit code 0 only if nothing failed.

Any analysis that legitimately cannot run with default options should be added
to a skip file, with an issue reference in a comment.

---

# JASP Option Fuzzer (`fuzztest.py`)

Schema-guided fuzzing of analysis options through the same agent API. For every
analysis it generates mutations of the default options based on the
machine-readable `optionMeta` schema (kinds: checkbox / combo / variables /
number / integer / percent / string / array, including deliberately invalid
values), runs them via `analysis_run` and classifies the outcome.

```bash
# everything, ~8 mutations per analysis (30-60 min)
Tests/gatetest/.venv/bin/python Tests/gatetest/fuzztest.py --report /tmp/fuzz.json

# one module, quick
Tests/gatetest/.venv/bin/python Tests/gatetest/fuzztest.py --module jaspTTests --runs-per-analysis 5

# replay a recorded failure
Tests/gatetest/.venv/bin/python Tests/gatetest/fuzztest.py --repro /tmp/fuzz.json.repro.json
```

**Hunted (gate failures, immediate abort + `.repro.json`):** JASP process death,
hangs (run not terminal within `--timeout-per-run`), malformed responses, and an
engine-queue wedge (≥ `--max-consecutive-stuck` consecutive runs that never even
get scheduled — the audit-style wedge, observed to precede a SIGABRT).

**Tolerated and counted:** `validationError` / rejected options, R `fatalError`,
JSON-RPC validation errors. `-32603` internal errors and unexpected statuses are
listed as *suspicious* (they do not fail the run unless `--strict`).

**Reproducibility:** a fresh random SEED is printed at the start and end of every
run and stored in the report; `--seed N` replays it exactly (the RNG stream is
deterministic — a fresh analysis instance is created per mutation so no
server-side state leaks, and schema-derived orderings are normalised because
JASP's `choices` order varies between process starts). Every run's exact options
JSON is recorded in the report; crash-class failures additionally write a
minimal `.repro.json` that `--repro` can replay in isolation. Note that a seed
only reproduces against the *same version* of `fuzztest.py`: any change to the
mutation logic changes the RNG stream (the options JSON in the report/repro
always replays exactly, regardless).

Known findings from the first runs (as of this writing, unfixed — see issues):
- Huge numbers in numeric options reach JsonCpp `asInt()` and surface as
  `-32603` internal errors ("LargestInt out of Int range") instead of clean
  validation errors (`jaspDescriptives/Descriptives`). **FIXED** (option
  binding is now exception-safe; huge ints are range-checked).
- Garbage in the `raincloudPlots` `customizationTable` (data-entry options
  table) wedges the engine queue: analyses are never scheduled again, ending
  in a SIGABRT in one observed run. **FIXED** (engine aborts are acked; the
  resend-abort loop is closed).
- `Json::Value::find: requires objectValue` internal errors on
  `jaspAnova/Anova` with non-object options where objects are expected.
  **FIXED** (binding is exception-safe; malformed values are rejected).

Full-sweep findings (2026-09, ~2600 mutations across all 271 analyses):

- **FIXED** — `jaspMetaAnalysis/BayesianMetaAnalysis` + `FunnelPlot`: a
  caught option-bind exception could leave half-bound option arrays behind;
  the deferred QML path (`RadioButton` destruction → `setBoundValue` →
  `AnalysisBase::boundValue` → `_getParentBoundValue`) then threw
  `Json::LogicError` from inside a Qt signal handler → `std::terminate` →
  SIGABRT. Fixed by requiring objects in `_getParentBoundValue` array
  elements (analysisbase.cpp) and handling `aborted` acks for removed
  analyses.
- **Hang (unfixed)** — `jaspLearnBayes/LSgameofchance` and
  `LSgameofskill`: extreme or non-numeric `pointsToWin` (e.g. 1e308 or
  `"not-a-number"`) reaches the R simulation loop, which then runs (nearly)
  forever. Repro: `fuzztest.py --repro` with the saved `.repro.json`. The
  QML validators clamp user input; the RPC path bypasses them, so a range
  clamp at the option layer is the likely fix.
- **SIGSEGV in the QML JS GC (flaky, unfixed)** — under heavy churn
  (analysis_remove while a run is in flight + fresh analysis_create re-binding),
  the desktop dies in `QV4::markDrain` (QtQml incremental GC marking) with
  EXC_BAD_ACCESS on a mangled/PAC-style address: a stale JS reference to a
  freed C++-backed QML object. This is the same finding that ended the first
  full sweep at 158/271 as a mystery "exit code 0"; subsequent runs also died
  with SIGBUS (-10) or SIGSEGV (-11). Not a gate-harness artifact: reproduced
  under lldb launch-supervision with the stack captured, and predates the
  #6313 branch. Qt/QML ownership territory (JS heap vs AnalysisForm/destroyForm
  timing during remove-while-in-flight); needs a Qt-level or ownership fix.
  Full write-up: `Docs/development/qv4-gc-crash.md`.

  Full-sweep data point (2026-09-10, commit `309fe5227`, seed 77,
  `--restart-on-death`): **269/271 analyses × 8 mutations = 2152 runs in 95
  min**, surviving 2 QV4 GC crashes (both in jaspAnova, recorded and restarted
  through). Outcome distribution: 1092 complete, 1020 rejected (graceful),
  35 fatalError (tolerated R errors), 3 validationError, 2 crash (the GC
  occurrences). Zero wedges, zero dispatcher hangs, zero watchdog kills — the
  mixedmod wedge (below) is gone and the web-view JS leak fix held (tree RSS
  stayed under the 5 GB cap for the entire sweep).
- **Dispatcher hang** — `jaspVisualModeling/mixedmod` with some
  fuzzed plot options: the R analysis completes, the desktop receives the
  (large, plot-heavy) results, and then the RPC dispatcher stops answering
  (`analysis_remove` times out). **FIXED** as a side effect of the deferred
  analysis-teardown change (commit `032c74952`): the original seed-234304393
  shard that wedged now completes cleanly. A saved repro is kept in
  `Tests/gatetest/repros/` for regression checking.
