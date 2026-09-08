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
import asyncio
import concurrent.futures
import csv
import datetime
import json
import os
import shlex
import socket
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request

TERMINAL_OK = {"complete"}
TERMINAL_FAIL = {"validationError", "fatalError"}

# jasp-mcp's JASPClient has a hard-coded 60s HTTP timeout, so every blocking
# RPC call must stay below that and rely on polling for longer work.
MCP_SAFE_WAIT_MS = 55000

EXPECTED_RPC_METHODS = [
    "ping", "rpc_discover", "modules_list", "analyses_list", "data_load",
    "data_load_status", "data_info", "analysis_create", "analysis_run",
    "analysis_results", "analysis_getOptions", "analysis_remove",
]

TEST_MODULE = "jaspTestModule"  # dev playground module, not shippable surface


class GateFailure(Exception):
    pass


class RpcError(GateFailure):
    """JSON-RPC error or transport failure talking to JASP directly."""


class McpError(GateFailure):
    """Failure inside the MCP layer (tool missing, non-JSON reply, ...)."""


class McpBroken(McpError):
    """MCP layer unusable; disable it for the remainder of the run."""


# ---------------------------------------------------------------------------
# Backends
# ---------------------------------------------------------------------------

class DirectRpcClient:
    def __init__(self, url: str):
        self.url = url
        self._id = 0

    def call(self, method: str, params: dict, timeout_s: float = 120.0) -> dict:
        self._id += 1
        payload = {"jsonrpc": "2.0", "id": self._id, "method": method, "params": params}
        req = urllib.request.Request(
            self.url,
            data=json.dumps(payload).encode("utf-8"),
            headers={"Content-Type": "application/json"},
        )
        try:
            with urllib.request.urlopen(req, timeout=timeout_s) as resp:
                body = json.load(resp)
        except urllib.error.HTTPError as e:
            detail = ""
            try:
                detail = e.read().decode("utf-8", "replace")[:300]
            except Exception:
                pass
            raise RpcError(f"{method}: HTTP {e.code} {detail}") from e
        except (OSError, TimeoutError, urllib.error.URLError) as e:
            raise RpcError(f"{method}: transport error: {e}") from e
        except json.JSONDecodeError as e:
            raise RpcError(f"{method}: non-JSON response") from e

        if "error" in body:
            err = body["error"]
            raise RpcError(f"{method}: {err.get('message', str(err))} (code {err.get('code')})")
        return body.get("result") or {}


class McpRpcClient:
    """Talks to jasp-mcp (MCP stdio server) as an MCP client."""

    def __init__(self, jasp_url: str):
        self.url = jasp_url
        self._session = None
        self._tools: set[str] = set()
        self._ready = threading.Event()
        self._stopped = threading.Event()
        self._thread: threading.Thread | None = None
        self._loop = None
        self._start_error: str | None = None

    def connect(self, timeout: float = 30.0) -> None:
        self._thread = threading.Thread(target=self._thread_main, daemon=True)
        self._thread.start()
        if not self._ready.wait(timeout):
            raise McpBroken(f"jasp-mcp did not initialize within {timeout}s: {self._start_error}")

    def _thread_main(self) -> None:
        try:
            self._loop = asyncio.new_event_loop()
            asyncio.set_event_loop(self._loop)
            self._loop.run_until_complete(self._main())
        except Exception as e:
            self._start_error = f"{type(e).__name__}: {e}"
            self._ready.set()  # unblock connect() so it can report the error
        finally:
            self._loop.close()

    async def _main(self) -> None:
        from mcp import ClientSession, StdioServerParameters
        from mcp.client.stdio import stdio_client

        env = dict(os.environ)
        env["JASP_RPC_URL"] = self.url
        env["PYTHONUNBUFFERED"] = "1"
        params = StdioServerParameters(command=sys.executable, args=["-m", "jasp_mcp"], env=env)
        async with stdio_client(params) as (read, write):
            async with ClientSession(read, write) as session:
                await session.initialize()
                tools = await session.list_tools()
                self._tools = {t.name for t in tools.tools}
                self._session = session
                self._ready.set()
                # Park until close() sets the threading.Event
                await asyncio.get_running_loop().run_in_executor(None, self._stopped.wait)

    def call(self, method: str, params: dict, timeout_s: float = 120.0) -> dict:
        if not self._ready.is_set() or self._session is None:
            raise McpBroken(f"jasp-mcp not connected: {self._start_error}")
        tool = "jasp_" + method
        if tool not in self._tools:
            raise McpError(f"tool '{tool}' not registered by jasp-mcp (has {len(self._tools)} tools)")

        async def _invoke():
            res = await self._session.call_tool(tool, params or {})
            text = "".join(c.text for c in res.content if getattr(c, "text", None))
            try:
                return json.loads(text)
            except json.JSONDecodeError:
                raise McpError(f"MCP tool '{tool}' returned non-JSON: {text[:300]}")

        fut = asyncio.run_coroutine_threadsafe(_invoke(), self._loop)
        try:
            return fut.result(timeout=timeout_s + 15)
        except concurrent.futures.TimeoutError as e:
            raise McpBroken(f"MCP call '{tool}' timed out after {timeout_s + 15}s") from e

    def close(self) -> None:
        self._stopped.set()
        if self._thread is not None:
            self._thread.join(timeout=15)


class GateClient:
    """Adapter that tries MCP first and falls back to direct JSON-RPC."""

    def __init__(self, direct: DirectRpcClient, mcp: McpRpcClient | None):
        self.direct = direct
        self.mcp = mcp
        self.mcp_ok = mcp is not None
        self.mcp_reason: str | None = None
        self.mcp_failures = 0

    def call(self, method: str, params: dict, timeout_s: float = 120.0) -> tuple[str, dict]:
        if self.mcp is not None and self.mcp_ok:
            try:
                return "mcp", self.mcp.call(method, params, timeout_s)
            except McpBroken as e:
                self.mcp_ok = False
                self.mcp_reason = str(e)
                self.mcp_failures += 1
                log(f"MCP layer broken, falling back to direct JSON-RPC for the rest of this run: {e}")
            except McpError as e:
                self.mcp_failures += 1
                log(f"MCP call failed ({e}); retrying over direct JSON-RPC to isolate the layer")
                try:
                    return "mcp-fail-direct-ok", self.direct.call(method, params, timeout_s + 30)
                except RpcError as e2:
                    raise GateFailure(f"{method}: MCP: {e} | direct: {e2}") from e2
        return "direct", self.direct.call(method, params, timeout_s + 30)


def log(msg: str) -> None:
    print(msg, flush=True)


# ---------------------------------------------------------------------------
# Test steps
# ---------------------------------------------------------------------------

def find_free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def parse_csv(path: str) -> tuple[int, int, list[str]]:
    with open(path, newline="", encoding="utf-8-sig") as f:
        reader = csv.reader(f)
        header = next(reader)
        rows = list(reader)
    return len(rows), len(header), [h for h in header if h]


def wait_for_server(client: GateClient, jasp_proc: subprocess.Popen, startup_timeout: float) -> None:
    deadline = time.time() + startup_timeout
    while time.time() < deadline:
        if jasp_proc.poll() is not None:
            raise GateFailure(f"JASP exited (code {jasp_proc.returncode}) before the RPC server came up")
        try:
            client.direct.call("ping", {}, timeout_s=10)
            return
        except RpcError:
            time.sleep(1)
    raise GateFailure(f"RPC server did not answer 'ping' within {startup_timeout}s")


def collect_analyses(client: GateClient, cfg) -> list[tuple[str, str]]:
    _, res = client.call("modules_list", {}, timeout_s=60)
    skips = set(cfg.skip)
    only = set(cfg.module)
    if cfg.skip_file:
        with open(cfg.skip_file) as f:
            for line in f:
                line = line.split("#", 1)[0].strip()
                if line:
                    skips.add(line)
    pairs = []
    for mod in res.get("modules", []):
        name = mod.get("name", "")
        if name == TEST_MODULE or name in skips or name in ("jaspTestModule",):
            continue
        if only and name not in only:
            continue
        for ana in mod.get("analyses", []):
            key_full = f"{name}/{ana['name']}"
            if key_full in skips:
                continue
            pairs.append((name, ana["name"]))
    return pairs


def load_data(client: GateClient, cfg, expected_rows: int, expected_cols: int) -> None:
    _, res = client.call(
        "data_load",
        {"path": cfg.csv, "delimiter": ",", "wait": True, "timeoutMs": MCP_SAFE_WAIT_MS},
        timeout_s=120,
    )
    status = res.get("status")
    if status != "success":
        raise GateFailure(f"data_load did not succeed (status={status!r}, message={res.get('message')!r})")
    rows, cols = res.get("rowCount"), res.get("columnCount")
    if rows != expected_rows or cols != expected_cols:
        raise GateFailure(
            f"loaded dataset shape mismatch: got {rows}x{cols}, expected {expected_rows}x{expected_cols} from {cfg.csv}"
        )
    log(f"Loaded {os.path.basename(cfg.csv)}: {rows} rows x {cols} columns")


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
    jasp_log = open(jasp_log_path, "wb")

    # -platform offscreen is required: with `-platform minimal` (used by --hide)
    # QtWebEngine's render delegate crashes the scene graph as soon as a
    # nested event loop lets the web view repaint (EXC_BAD_ACCESS in
    # QSGTexture::resolveInterface via NativeSkiaOutputDeviceMetal::texture).
    args = [jasp_bin, "-platform", "offscreen", f"--rpcPort={port}"] + shlex.split(cfg.jasp_extra_args)
    jasp_proc = subprocess.Popen(args, stdout=jasp_log, stderr=subprocess.STDOUT)
    log(f"JASP pid {jasp_proc.pid}, log: {jasp_log_path}")

    client = GateClient(DirectRpcClient(url), None)

    try:
        wait_for_server(client, jasp_proc, cfg.startup_timeout)
        log("JASP RPC server is up.")

        if not cfg.no_mcp:
            try:
                mcp = McpRpcClient(url)
                mcp.connect()
                client.mcp = mcp
                client.mcp_ok = True
                log("jasp-mcp layer connected.")
            except Exception as e:
                client.mcp_reason = f"could not set up jasp-mcp: {type(e).__name__}: {e}"
                log(f"WARNING: {client.mcp_reason}")

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
            load_data(client, cfg, expected_rows, expected_cols)
        except GateFailure as e:
            failures.append({"type": "data", "detail": str(e)})
            log(f"FATAL: data load failed, aborting: {e}")
            return finish(failures, client, jasp_proc, cfg, 0, 0)

        pairs = collect_analyses(client, cfg)
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
        if jasp_proc.poll() is None:
            jasp_proc.terminate()
            try:
                jasp_proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                jasp_proc.kill()
        jasp_log.close()


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
