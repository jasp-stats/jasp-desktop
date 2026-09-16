#!/usr/bin/env python3
"""
Shared harness for the JASP gate test (gatetest.py) and fuzz test (fuzztest.py).

Launches JASP headless with its RPC (MCP) server enabled and provides RPC
clients that drive JASP through jasp-mcp (the MCP layer AI agents use) with an
automatic fallback to direct JSON-RPC, so failures immediately show whether
JASP itself or the MCP layer is broken.

Stdlib only for direct mode; MCP mode additionally needs the `mcp` package and
jasp-mcp importable (see run_gatetest.sh).
"""

from __future__ import annotations

import asyncio
import concurrent.futures
import csv
import json
import os
import shlex
import socket
import subprocess
import sys
import threading
import time
import urllib.error
import urllib.request

# jasp-mcp's JASPClient has a hard-coded 60s HTTP timeout, so every blocking
# RPC call must stay below that and rely on polling for longer work.
MCP_SAFE_WAIT_MS = 55000

TEST_MODULE = "jaspTestModule"  # dev playground module, not shippable surface


class GateFailure(Exception):
    pass


class RpcError(GateFailure):
    """JSON-RPC error or transport failure talking to JASP directly."""


class McpError(GateFailure):
    """Failure inside the MCP layer (tool missing, non-JSON reply, ...)."""


class McpBroken(McpError):
    """MCP layer unusable; disable it for the remainder of the run."""


def log(msg: str) -> None:
    print(msg, flush=True)


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


# ---------------------------------------------------------------------------
# JASP process + common test steps
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


def start_jasp(jasp_bin: str, port: int, log_path: str, extra_args: str = "") -> subprocess.Popen:
    """Launch JASP headless with the RPC server on the given port."""
    # -platform offscreen is required: with `-platform minimal` (used by --hide)
    # QtWebEngine's render delegate crashes the scene graph as soon as a
    # nested event loop lets the web view repaint (EXC_BAD_ACCESS in
    # QSGTexture::resolveInterface via NativeSkiaOutputDeviceMetal::texture).
    jasp_log = open(log_path, "wb")
    try:
        args = [jasp_bin, "-platform", "offscreen", f"--rpcPort={port}"] + shlex.split(extra_args)
        return subprocess.Popen(args, stdout=jasp_log, stderr=subprocess.STDOUT)
    finally:
        jasp_log.close()  # the child inherited the fd; we do not need ours anymore


def shutdown_jasp(jasp_proc: subprocess.Popen) -> None:
    if jasp_proc.poll() is None:
        jasp_proc.terminate()
        try:
            jasp_proc.wait(timeout=10)
        except subprocess.TimeoutExpired:
            jasp_proc.kill()


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


def connect_mcp(client: GateClient, url: str) -> None:
    """Attach the MCP layer to the client if possible; failure is recorded, not raised."""
    try:
        mcp = McpRpcClient(url)
        mcp.connect()
        client.mcp = mcp
        client.mcp_ok = True
        log("jasp-mcp layer connected.")
    except Exception as e:
        client.mcp_reason = f"could not set up jasp-mcp: {type(e).__name__}: {e}"
        log(f"WARNING: {client.mcp_reason}")


def collect_analyses(client: GateClient, module_filter: list[str], skip: list[str],
                     skip_file: str | None = None) -> list[tuple[str, str]]:
    _, res = client.call("modules_list", {}, timeout_s=60)
    skips = set(skip)
    only = set(module_filter)
    if skip_file:
        with open(skip_file) as f:
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


def load_data(client: GateClient, csv_path: str, expected_rows: int, expected_cols: int) -> None:
    _, res = client.call(
        "data_load",
        {"path": csv_path, "delimiter": ",", "wait": True, "timeoutMs": MCP_SAFE_WAIT_MS},
        timeout_s=120,
    )
    status = res.get("status")
    if status != "success":
        raise GateFailure(f"data_load did not succeed (status={status!r}, message={res.get('message')!r})")
    rows, cols = res.get("rowCount"), res.get("columnCount")
    if rows != expected_rows or cols != expected_cols:
        raise GateFailure(
            f"loaded dataset shape mismatch: got {rows}x{cols}, expected {expected_rows}x{expected_cols} from {csv_path}"
        )
    log(f"Loaded {os.path.basename(csv_path)}: {rows} rows x {cols} columns")
