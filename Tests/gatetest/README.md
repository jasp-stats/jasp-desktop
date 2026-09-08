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
