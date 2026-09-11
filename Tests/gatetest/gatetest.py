#!/usr/bin/env python3
"""
JASP gate test.

Launches JASP headless with its RPC (MCP) server enabled, loads the standard
debug.csv, then creates + runs + removes every analysis of every loaded module
through the agent API, verifying each reaches "complete" with non-error results.

The test drives JASP through jasp-mcp (the MCP layer AI agents use). Any step
that fails on the MCP layer is automatically retried over direct JSON-RPC so the
report immediately shows whether JASP itself or the MCP layer is broken.

Exit codes: 0 = all good, 1 = gate failure, 130 = interrupted.

Stdlib only for direct mode; MCP mode additionally needs the `mcp` package and
jasp-mcp importable (see run_gatetest.sh).
"""

from __future__ import annotations

import argparse
import datetime
import json
import os
import sys
import tempfile
import time

from gatecommon import (
    MCP_SAFE_WAIT_MS,
    DirectRpcClient,
    GateClient,
    GateFailure,
    collect_analyses,
    connect_mcp,
    find_free_port,
    load_data,
    log,
    parse_csv,
    shutdown_jasp,
    start_jasp,
    wait_for_server,
)

TERMINAL_OK = {"complete"}
TERMINAL_FAIL = {"validationError", "fatalError"}

EXPECTED_RPC_METHODS = [
    "ping", "rpc_discover", "modules_list", "analyses_list", "data_load",
    "data_load_status", "data_info", "analysis_create", "analysis_run",
    "analysis_results", "analysis_remove",
]


def run_one_analysis(client: GateClient, module: str, analysis: str, cfg) -> tuple[float, str]:
    """Create, run and remove one analysis. Returns (duration, layer). Raises GateFailure."""
    t0 = time.time()
    layer_used = "direct"

    _, created = client.call("analysis_create", {"module": module, "analysis": analysis}, timeout_s=90)
    analysis_id = created.get("analysisId")
    if analysis_id is None:
        raise GateFailure("analysis_create returned no analysisId")
    options = created.get("options")
    if not isinstance(options, dict) or not options:
        raise GateFailure("analysis_create returned empty/missing default options")

    try:
        deadline = time.time() + cfg.timeout_per_analysis
        first = True
        while True:
            remaining = deadline - time.time()
            if remaining <= 0:
                raise GateFailure(f"timeout after {cfg.timeout_per_analysis}s (analysis never reached a terminal state)")
            wait_ms = int(min(MCP_SAFE_WAIT_MS, remaining * 1000))
            method, params = (
                ("analysis_run", {"analysisId": analysis_id, "options": {}, "wait": True, "timeoutMs": wait_ms})
                if first
                else ("analysis_results", {"analysisId": analysis_id, "wait": True, "timeoutMs": wait_ms})
            )
            first = False
            layer_used, res = client.call(method, params, timeout_s=wait_ms / 1000 + 30)
            status = res.get("status")
            if status in TERMINAL_OK:
                results = res.get("results")
                if not isinstance(results, dict) or not results:
                    raise GateFailure(f"status 'complete' but results missing/empty: {json.dumps(results)[:200]}")
                if results.get("error") or results.get("errorMessage"):
                    raise GateFailure(f"results contain an error: {str(results.get('errorMessage', results.get('error')))[:300]}")
                return time.time() - t0, layer_used
            if status in TERMINAL_FAIL:
                msg = res.get("message") or ""
                err = ""
                if isinstance(res.get("results"), dict):
                    err = str(res["results"].get("errorMessage", ""))[:300]
                raise GateFailure(f"analysis finished with status '{status}' {msg} {err}".strip())
            if status != "running":
                raise GateFailure(f"unexpected analysis status: {status!r}")
    finally:
        try:
            client.call("analysis_remove", {"analysisId": analysis_id}, timeout_s=60)
        except GateFailure as e:
            log(f"WARNING: analysis_remove failed for {module}/{analysis} (id {analysis_id}): {e}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main() -> int:
    cfg = parse_args()
    failures: list[dict] = []

    if not os.path.isfile(cfg.csv):
        log(f"FATAL: csv not found: {cfg.csv}")
        return 1
    expected_rows, expected_cols, colnames = parse_csv(cfg.csv)

    jasp_bin = cfg.jasp_bin
    if not os.path.isfile(jasp_bin):
        log(f"FATAL: JASP binary not found: {jasp_bin} (pass --jasp-bin)")
        return 1

    port = cfg.port if cfg.port else find_free_port()
    url = f"http://127.0.0.1:{port}/rpc"
    log(f"JASP gate test starting: {jasp_bin} on port {port}")

    jasp_log_path = cfg.jasp_log or os.path.join(tempfile.gettempdir(), f"jasp-gatetest-{os.getpid()}.log")
    jasp_proc = start_jasp(jasp_bin, port, jasp_log_path, cfg.jasp_extra_args)
    log(f"JASP pid {jasp_proc.pid}, log: {jasp_log_path}")

    client = GateClient(DirectRpcClient(url), None)

    try:
        wait_for_server(client, jasp_proc, cfg.startup_timeout)
        log("JASP RPC server is up.")

        if not cfg.no_mcp:
            connect_mcp(client, url)

        # Spec sanity: every method we rely on must be exposed.
        try:
            _, disc = client.call("rpc_discover", {}, timeout_s=60)
            exposed = {m["name"] for m in disc.get("methods", [])}
            missing = [m for m in EXPECTED_RPC_METHODS if m not in exposed]
            if missing:
                failures.append({"type": "spec", "detail": f"methods missing from rpc_discover: {missing}"})
        except GateFailure as e:
            failures.append({"type": "spec", "detail": f"rpc_discover failed: {e}"})

        # Data
        try:
            load_data(client, cfg.csv, expected_rows, expected_cols)
        except GateFailure as e:
            failures.append({"type": "data", "detail": str(e)})
            log(f"FATAL: data load failed, aborting: {e}")
            return finish(failures, client, jasp_proc, cfg, 0, 0)

        pairs = collect_analyses(client, cfg.module, cfg.skip, cfg.skip_file)
        if not pairs:
            failures.append({"type": "modules_list", "detail": "modules_list returned no analyses"})
            return finish(failures, client, jasp_proc, cfg, 0, 0)
        log(f"Found {len(pairs)} analyses across {len({m for m, _ in pairs})} modules. Starting run...")

        done = 0
        t_start = time.time()
        for module, analysis in pairs:
            done += 1
            if jasp_proc.poll() is not None:
                failures.append({
                    "type": "crash",
                    "module": module, "analysis": analysis,
                    "detail": f"JASP process exited (code {jasp_proc.returncode}) mid-run after {done - 1}/{len(pairs)} analyses",
                })
                log(f"FATAL: JASP crashed/exited while testing {module}/{analysis}")
                break
            label = f"[{done}/{len(pairs)}] {module}/{analysis}"
            try:
                dt, layer = run_one_analysis(client, module, analysis, cfg)
                log(f"{label} ... OK ({dt:.1f}s, via {layer})")
            except GateFailure as e:
                log(f"{label} ... FAIL: {e}")
                failures.append({"type": "analysis", "module": module, "analysis": analysis, "detail": str(e)})
                if cfg.fail_fast:
                    log("fail-fast enabled, stopping here")
                    break
            except KeyboardInterrupt:
                raise

        # Close-out: workspace should be empty again.
        try:
            _, listing = client.call("analyses_list", {}, timeout_s=60)
            leftover = listing.get("analyses", [])
            if leftover:
                failures.append({
                    "type": "cleanup",
                    "detail": f"{len(leftover)} analyses left in workspace after test: "
                              f"{[a.get('id') for a in leftover][:10]}",
                })
        except GateFailure as e:
            failures.append({"type": "cleanup", "detail": f"analyses_list failed: {e}"})

        elapsed = time.time() - t_start
        return finish(failures, client, jasp_proc, cfg, done, elapsed)

    except KeyboardInterrupt:
        log("\nInterrupted.")
        return 130
    except GateFailure as e:
        failures.append({"type": "infra", "detail": str(e)})
        log(f"FATAL: {e}")
        return finish(failures, client, jasp_proc, cfg, 0, 0)
    finally:
        if client.mcp is not None:
            client.mcp.close()
        shutdown_jasp(jasp_proc)


def finish(failures, client, jasp_proc, cfg, done, elapsed) -> int:
    if jasp_proc.poll() is not None and jasp_proc.returncode not in (0,):
        failures.append({"type": "crash", "detail": f"JASP exited with code {jasp_proc.returncode}"})

    if client.mcp_reason:
        failures.append({"type": "mcp-layer", "detail": client.mcp_reason})

    jasp_failures = [f for f in failures if f["type"] in ("analysis", "data", "crash", "cleanup", "modules_list", "spec", "infra")]
    mcp_failures = [f for f in failures if f["type"] == "mcp-layer"]

    log("\n" + "=" * 70)
    log("GATE TEST SUMMARY")
    log("=" * 70)
    log(f"Analyses run:        {done} ({elapsed:.0f}s)")
    log(f"JASP failures:       {len(jasp_failures)}")
    log(f"MCP layer failures:  {len(mcp_failures)}" + (f"  (also {client.mcp_failures} per-call fallbacks)" if client.mcp_failures else ""))
    if failures:
        for f in failures:
            where = f"{f.get('module', '')}/{f.get('analysis', '')}: " if f.get("module") else ""
            log(f"  - [{f['type']}] {where}{f['detail'][:400]}")
    log("=" * 70)

    if cfg.report:
        with open(cfg.report, "w") as f:
            json.dump({
                "timestamp": datetime.datetime.now().isoformat(),
                "analyses_done": done,
                "mcp_ok": client.mcp is not None and client.mcp_ok,
                "failures": failures,
            }, f, indent=2)
        log(f"Report written to {cfg.report}")

    return 1 if failures else 0


def parse_args() -> argparse.Namespace:
    repo = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--jasp-bin", default=os.path.join(repo, "build", "Desktop", "JASP"))
    p.add_argument("--port", type=int, default=0, help="RPC server port (default: pick a free port)")
    p.add_argument("--csv", default=os.path.join(repo, "Resources", "Data Sets", "debug.csv"))
    p.add_argument("--module", action="append", default=[], help="only test this module (repeatable)")
    p.add_argument("--skip", action="append", default=[], help="skip a module ('jaspFoo') or analysis ('jaspFoo/Bar')")
    p.add_argument("--skip-file", default=None, help="file with lines of 'jaspFoo' or 'jaspFoo/Bar' to skip (# comments allowed)")
    p.add_argument("--startup-timeout", type=float, default=300, help="seconds to wait for JASP + RPC server (default 300)")
    p.add_argument("--timeout-per-analysis", type=float, default=120, help="seconds per analysis (default 120)")
    p.add_argument("--fail-fast", action="store_true")
    p.add_argument("--no-mcp", action="store_true", help="skip the jasp-mcp layer, drive JSON-RPC directly")
    p.add_argument("--jasp-extra-args", default="", help="extra args for the JASP process (shell-quoted)")
    p.add_argument("--jasp-log", default=None, help="where to write JASP stdout/stderr (default: temp file)")
    p.add_argument("--report", default=None, help="write a JSON summary report to this path")
    return p.parse_args()


if __name__ == "__main__":
    sys.exit(main())
