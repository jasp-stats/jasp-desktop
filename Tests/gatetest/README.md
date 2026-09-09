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
and runs `gatetest.py`.

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
minimal `.repro.json` that `--repro` can replay in isolation.

Known findings from the first runs (as of this writing, unfixed — see issues):
- Huge numbers in numeric options reach JsonCpp `asInt()` and surface as
  `-32603` internal errors ("LargestInt out of Int range") instead of clean
  validation errors (`jaspDescriptives/Descriptives`).
- Garbage in the `raincloudPlots` `customizationTable` (data-entry options
  table) wedges the engine queue: analyses are never scheduled again, ending
  in a SIGABRT in one observed run.
- `Json::Value::find: requires objectValue` internal errors on
  `jaspAnova/Anova` with non-object options where objects are expected.
