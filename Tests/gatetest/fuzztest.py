#!/usr/bin/env python3
"""
JASP option fuzzer.

Schema-guided fuzzing of analysis options through the agent API: for every
analysis (filtered like the gate test), generates mutations of the default
options based on the machine-readable optionMeta schema (kinds: checkbox,
combo, variables, number, integer, percent, string, array), runs them via
analysis_run and classifies the outcome.

What we hunt (gate failures, immediate abort with a repro file):
  - JASP process death (SIGSEGV/SIGABRT/...)
  - hangs (a run still not terminal after --timeout-per-run)
  - malformed / non-JSON responses

What we tolerate (counted, never fatal):
  - validationError / "error" with a message (graceful rejection)
  - R fatalError (R's own error path)
  - JSON-RPC validation errors (-32602 etc.)
  - -32603 internal errors are counted as "suspicious" and listed prominently
    but do not fail the run unless --strict is given

Every run's exact options JSON is recorded, and the seed is printed and stored,
so any run is reproducible with --seed (the RNG stream is deterministic for a
given seed + analysis order).  Use --repro <repro.json> to replay a recorded
failure in isolation.

Exit codes: 0 = nothing found, 1 = crash/hang/malformed (or suspicious with
--strict), 130 = interrupted.

Stdlib only for direct mode; MCP mode additionally needs the `mcp` package and
jasp-mcp importable (see run_gatetest.sh).
"""

from __future__ import annotations

import argparse
import datetime
import json
import os
import random
import sys
import tempfile
import time

from gatecommon import (
    MCP_SAFE_WAIT_MS,
    DirectRpcClient,
    GateClient,
    GateFailure,
    RpcError,
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

CRASH_CLASSES = ("crash", "hang", "malformed")

UNICODE_CRUD = "¤_α_а_א_ሀ_あ_🚀_\x01\x7f"
LONG_STRING = "x" * 10000
R_INJECTION = "'; system('touch /tmp/jasp-fuzz-was-here'); x <- '"

NUMBERS = [0, -1, 1, -0.0, 1e-12, 1e300, -1e300, 12345.6789, 2**53, 2**63 - 1, -(2**63)]


# ---------------------------------------------------------------------------
# Option mutation
# ---------------------------------------------------------------------------

def _columns_by_type(client: GateClient) -> dict[str, list[str]]:
    _, info = client.call("data_info", {}, timeout_s=60)
    by_type: dict[str, list[str]] = {}
    for col in info.get("columns", []):
        by_type.setdefault(col.get("type", "unknown"), []).append(col.get("name", ""))
    return by_type


def _columns_matching(allowed_types: list[str], by_type: dict[str, list[str]]) -> list[str]:
    """Columns whose type is in allowedTypes (a column may match several types)."""
    out: list[str] = []
    for col_type in sorted(by_type):  # sorted: JASP's column/type ordering varies between starts
        if col_type in allowed_types:
            out.extend(sorted(by_type[col_type]))
    return out


def _pick_invalid_for_variables(rng: random.Random, by_type: dict[str, list[str]]) -> list:
    """Values that are deliberately wrong for a variables option."""
    all_names = sorted(n for names in by_type.values() for n in names)
    pool: list = [
        ["__fuzz_nonexistent_column__"],
        [""],
        [all_names[0], all_names[0]] if all_names else [],
        [],
        ["unicode"],  # the unicode column of debug.csv
        all_names[:40],
        {"value": all_names[:1], "types": ["scale"]},  # object-where-array
        "single_name_instead_of_array",
        [None],
        [123],
    ]
    return [rng.choice(pool)]


def _value_for_kind(rng: random.Random, meta: dict, by_type: dict[str, list[str]],
                    valid_probability: float) -> object:
    """Generate one value for an option based on its optionMeta entry."""
    kind = meta.get("kind")
    shape = meta.get("shape")

    if kind == "checkbox":
        if rng.random() < valid_probability:
            return not bool(meta.get("default", False)) if isinstance(meta.get("default"), bool) else rng.choice([True, False])
        return rng.choice(["yes", 1, None, []])

    if kind == "combo":
        # sorted(): the order of JASP's choices array varies between process starts,
        # which would make the RNG stream irreproducible for the same seed.
        choices = sorted(str(c) for c in (meta.get("choices") or []))
        if choices and rng.random() < valid_probability:
            return rng.choice(choices)
        return rng.choice(["__fuzz_nonexistent__", "", UNICODE_CRUD, None, 0, True])

    if kind in ("number", "integer", "percent"):
        if rng.random() < valid_probability:
            default = meta.get("default")
            if isinstance(default, (int, float)) and not isinstance(default, bool):
                jitter = rng.choice([0, 1, -1, 0.5, -0.5, 1e-6, 100])
                value = default + jitter if kind != "integer" else int(default + rng.choice([0, 1, -1, 7]))
                if kind == "percent":
                    value = rng.choice([0, 0.5, 0.95, 1])
                return value
            return rng.choice([0, 1, 0.95, -1])
        # invalid / extreme
        pick = rng.choice(NUMBERS + [None, "not-a-number", True, [1]])
        if kind == "integer" and isinstance(pick, float) and pick == int(pick):
            pick = int(pick)
        return pick

    if kind == "variables":
        shape_obj = shape if isinstance(shape, dict) else {}
        value_shape = shape_obj.get("value", shape_obj) if isinstance(shape_obj, dict) else None
        is_single = meta.get("single", True)
        if rng.random() < valid_probability:
            allowed = meta.get("allowedTypes") or []
            types_map = meta.get("types") if isinstance(meta.get("types"), dict) else None
            if types_map:
                # types+value shape: pick plausible columns for each entry of types
                cols = []
                for t in types_map.get("types", []) if isinstance(types_map.get("types"), list) else []:
                    candidates = _columns_matching([t], by_type)
                    cols.append(rng.choice(candidates) if candidates else rng.choice([n for ns in by_type.values() for n in ns]))
                value = cols[0] if (is_single and cols) else cols
            else:
                candidates = _columns_matching(allowed, by_type) if allowed else sorted(n for ns in by_type.values() for n in ns)
                if not candidates:
                    candidates = sorted(n for ns in by_type.values() for n in ns)
                value = rng.choice(candidates) if is_single else rng.sample(candidates, k=min(len(candidates), rng.randint(1, 3)))
            if isinstance(value_shape, dict) and isinstance(value_shape.get("types"), list) and isinstance(value, list):
                return {"types": value_shape["types"][:len(value)], "value": value}
            return value
        return rng.choice(_pick_invalid_for_variables(rng, by_type))

    if kind == "string":
        if rng.random() < valid_probability:
            default = meta.get("default")
            if isinstance(default, str) and default:
                return default
            return rng.choice(["a", "fuzz", UNICODE_CRUD])
        return rng.choice(["", " ", LONG_STRING, UNICODE_CRUD, R_INJECTION, "'", '"', "\\\\", "\n\r\t", None, 42, []])

    if kind == "array":
        if rng.random() < valid_probability and isinstance(shape, list) and shape:
            return shape
        if rng.random() < valid_probability and isinstance(shape, dict):
            return shape
        return rng.choice([[], [1, 2, 3], [None], ["a", 1], {"nested": {"deep": [1]}}, "not-an-array", None,
                           [LONG_STRING]])

    # Unknown kind / anything else: poke it with type mismatches and null.
    return rng.choice([None, 0, "", [], {}, True, UNICODE_CRUD])


def mutate_options(rng: random.Random, defaults: dict, option_meta: dict,
                   by_type: dict[str, list[str]], max_fields: int = 3,
                   invalid_probability: float = 0.4) -> dict:
    """Build one fuzzed options dict: start from the defaults, mutate 1..max_fields fields."""
    options = json.loads(json.dumps(defaults))  # deep copy

    if rng.random() < 0.05:
        # occasionally attack the top-level contract itself
        attack = rng.choice([
            "__fuzz_unknown_option__",
            "__fuzz_unknown_option__",
        ])
        return {attack: rng.choice([1, "x", None, {"deep": {"deeper": [LONG_STRING]}}])}

    known = [name for name in options.keys()]
    meta_keys = [name for name in option_meta.keys() if name not in known]
    candidates = known + meta_keys
    if not candidates:
        return options

    for name in rng.sample(candidates, k=min(len(candidates), rng.randint(1, max_fields))):
        meta = option_meta.get(name)
        if isinstance(meta, dict) and meta.get("kind"):
            options[name] = _value_for_kind(rng, meta, by_type, valid_probability=1.0 - invalid_probability)
        else:
            options[name] = rng.choice([None, 0, "", [], {}, True, LONG_STRING, UNICODE_CRUD])
    return options


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

def classify(status: str | None) -> str:
    if status == "complete":
        return "complete"
    if status == "validationError":
        return "validationError"
    if status == "fatalError":
        return "fatalError"
    if status == "error":
        return "rejected"
    return f"unexpected:{status!r}"


KNOWN_REJECT_CODES = (-32602, -32700, -32600, -32601)


def classify_failure(message: str) -> str:
    """Classify a combined MCP+direct failure message by its JSON-RPC error code.

    The dispatcher turns handler exceptions into -32603 (e.g. an option value so
    large that JsonCpp's asInt() throws), which JASP survives: internal-error,
    not crash. Only transport-level failures (process dying) are crash-class.
    """
    if "(code -32603)" in message:
        return "internal-error"
    if any(f"(code {c})" in message for c in KNOWN_REJECT_CODES):
        return "rejected"
    return "crash"


def run_one_mutation(client: GateClient, analysis_id: int, options: object,
                     timeout_per_run: float) -> tuple[str, str | None, dict]:
    """Run one fuzzed options dict. Returns (outcome_class, layer, response)."""
    deadline = time.time() + timeout_per_run
    first = True
    while True:
        remaining = deadline - time.time()
        if remaining <= 0:
            return "hang", "direct", {}
        wait_ms = int(min(MCP_SAFE_WAIT_MS, remaining * 1000))
        method, params = (
            ("analysis_run", {"analysisId": analysis_id, "options": options, "wait": True, "timeoutMs": wait_ms})
            if first
            else ("analysis_results", {"analysisId": analysis_id, "wait": True, "timeoutMs": wait_ms})
        )
        first = False
        try:
            layer, res = client.call(method, params, timeout_s=wait_ms / 1000 + 30)
        except RpcError as e:
            outcome = classify_failure(str(e))
            if outcome == "crash":
                return "crash", "direct", {"message": str(e)}
            return outcome, "direct", {"message": str(e)}
        except GateFailure as e:
            # Both layers failed (MCP error + direct error). Look at the codes in the
            # combined message: JASP-side errors (-32603 etc.) mean JASP is alive.
            outcome = classify_failure(str(e))
            if outcome == "crash":
                return "crash", "direct", {"message": str(e)}
            return outcome, "direct", {"message": str(e)}
        status = res.get("status")
        if status in ("running", "empty"):
            # 'empty' right after analysis_run means the analysis is not even scheduled
            # yet (engine queue saturation). Poll both; only the deadline decides.
            if time.time() >= deadline:
                return ("hang" if status == "running" else "stuck-empty"), layer, res
            continue
        outcome = classify(status)
        return outcome, layer, res


def write_repro(path: str, seed: int, module: str, analysis: str, csv_path: str, options: object, detail: str) -> None:
    repro = {
        "seed": seed,
        "module": module,
        "analysis": analysis,
        "csv": csv_path,
        "options": options,
        "detail": detail,
        "hint": "Replay with: fuzztest.py --repro " + path,
    }
    with open(path, "w") as f:
        json.dump(repro, f, indent=2)
    log(f"REPRO written to {path}")


def replay(cfg) -> int:
    """Replay a recorded failure: --repro repro.json"""
    with open(cfg.repro) as f:
        repro = json.load(f)
    log(f"Replaying failure from {cfg.repro}: {repro['module']}/{repro['analysis']} (seed {repro.get('seed')})")

    port = find_free_port()
    url = f"http://127.0.0.1:{port}/rpc"
    jasp_log = cfg.jasp_log or os.path.join(tempfile.gettempdir(), f"jasp-fuzz-repro-{os.getpid()}.log")
    jasp_proc = start_jasp(cfg.jasp_bin, port, jasp_log, cfg.jasp_extra_args)
    client = GateClient(DirectRpcClient(url), None)
    outcome = "unknown"
    try:
        wait_for_server(client, jasp_proc, cfg.startup_timeout)
        if not cfg.no_mcp:
            connect_mcp(client, url)
        load_data(client, repro["csv"], *parse_csv(repro["csv"])[:2])
        created = call_or_reject(client, "analysis_create", {"module": repro["module"], "analysis": repro["analysis"]})
        aid = created.get("analysisId")
        outcome, _, _ = run_one_mutation(client, aid, repro["options"], cfg.timeout_per_run)
        log(f"Replay outcome: {outcome}")
        log("REPRODUCED" if outcome in CRASH_CLASSES else "NOT reproduced (fixed or flaky?)")
        return 1 if outcome in CRASH_CLASSES else 0
    except GateFailure as e:
        log(f"Replay failed to set up: {e}")
        return 1
    finally:
        if client.mcp is not None:
            client.mcp.close()
        shutdown_jasp(jasp_proc)


def call_or_reject(client: GateClient, method: str, params: dict, timeout_s: float = 90) -> dict:
    layer, res = client.call(method, params, timeout_s=timeout_s)
    return res


def main() -> int:
    cfg = parse_args()

    if cfg.repro:
        return replay(cfg)

    if not os.path.isfile(cfg.csv):
        log(f"FATAL: csv not found: {cfg.csv}")
        return 1
    if not os.path.isfile(cfg.jasp_bin):
        log(f"FATAL: JASP binary not found: {cfg.jasp_bin} (pass --jasp-bin)")
        return 1

    seed = cfg.seed if cfg.seed is not None else random.SystemRandom().randrange(2**31)
    rng = random.Random(seed)
    log("=" * 70)
    log(f"JASP OPTION FUZZER")
    log(f"SEED: {seed}          <- reproduce this exact run with: --seed {seed}")
    log("=" * 70)

    expected_rows, expected_cols, _ = parse_csv(cfg.csv)
    port = cfg.port if cfg.port else find_free_port()
    url = f"http://127.0.0.1:{port}/rpc"

    jasp_log_path = cfg.jasp_log or os.path.join(tempfile.gettempdir(), f"jasp-fuzztest-{os.getpid()}.log")
    jasp_proc = start_jasp(cfg.jasp_bin, port, jasp_log_path, cfg.jasp_extra_args)
    log(f"JASP pid {jasp_proc.pid}, log: {jasp_log_path}")

    client = GateClient(DirectRpcClient(url), None)
    records: list[dict] = []
    crashes: list[dict] = []
    totals: dict[str, int] = {}

    def bump(cls: str) -> None:
        totals[cls] = totals.get(cls, 0) + 1

    try:
        wait_for_server(client, jasp_proc, cfg.startup_timeout)
        log("JASP RPC server is up.")
        if not cfg.no_mcp:
            connect_mcp(client, url)
        load_data(client, cfg.csv, expected_rows, expected_cols)

        by_type = _columns_by_type(client)
        pairs = collect_analyses(client, cfg.module, cfg.skip, cfg.skip_file)
        if not pairs:
            log("FATAL: modules_list returned no analyses")
            return 1
        total_runs = len(pairs) * cfg.runs_per_analysis
        log(f"Fuzzing {len(pairs)} analyses x {cfg.runs_per_analysis} runs = {total_runs} runs...")

        done = 0
        analyses_done = 0
        last_mutation: dict | None = None  # most recent options we sent, for crash reports
        consecutive_stuck = 0              # runs that never even got scheduled (engine queue wedged)
        t_start = time.time()
        for module, analysis in pairs:
            analyses_done += 1
            if jasp_proc.poll() is not None:
                detail = f"JASP process exited (code {jasp_proc.returncode}) while fuzzing"
                crashes.append({"type": "crash", "module": module, "analysis": analysis, "detail": detail,
                                "last_options": last_mutation})
                log(f"FATAL: {detail} {module}/{analysis}")
                if last_mutation is not None and cfg.report:
                    write_repro(cfg.report + ".repro.json", seed, module, analysis, cfg.csv, last_mutation, detail)
                break
            try:
                _, created = client.call("analysis_create", {"module": module, "analysis": analysis}, timeout_s=90)
                analysis_id = created.get("analysisId")
                defaults = created.get("options")
                option_meta = created.get("optionMeta")
                if analysis_id is None or not isinstance(defaults, dict):
                    records.append({"module": module, "analysis": analysis, "outcome": "create-failed"})
                    bump("create-failed")
                    continue
            except GateFailure as e:
                records.append({"module": module, "analysis": analysis, "outcome": "create-failed", "detail": str(e)})
                bump("create-failed")
                if cfg.fail_fast:
                    break
                continue

            label = f"[{analyses_done}/{len(pairs)}] {module}/{analysis}"
            try:
                for i in range(cfg.runs_per_analysis):
                    done += 1
                    options = mutate_options(rng, defaults, option_meta or {}, by_type)
                    last_mutation = options if isinstance(options, dict) else None

                    # Fresh analysis per mutation: reusing one instance leaks server-side
                    # option state between runs, which makes outcomes (and hence the RNG
                    # stream via optionMetaDelta) irreproducible. create/remove per run is
                    # cheap and makes --seed truly replay a run.
                    try:
                        _, fresh = client.call("analysis_create", {"module": module, "analysis": analysis}, timeout_s=90)
                        fresh_id = fresh.get("analysisId")
                        if fresh_id is None:
                            raise GateFailure("analysis_create returned no analysisId")
                    except GateFailure as e:
                        rec = {"module": module, "analysis": analysis, "run": i + 1,
                               "options": options, "outcome": "create-failed", "message": str(e)}
                        records.append(rec)
                        bump("create-failed")
                        continue

                    try:
                        outcome, layer, res = run_one_mutation(client, fresh_id, options, cfg.timeout_per_run)
                    except GateFailure as e:
                        # Both layers failed; classify by the error codes in the message
                        # (JASP-side -32603 etc. mean JASP itself is still alive).
                        outcome = classify_failure(str(e))
                        res = {"message": str(e)}
                    finally:
                        try:
                            client.call("analysis_remove", {"analysisId": fresh_id}, timeout_s=60)
                        except GateFailure as e:
                            log(f"WARNING: analysis_remove failed for {module}/{analysis}: {e}")

                    rec = {"module": module, "analysis": analysis, "run": i + 1,
                           "options": options, "outcome": outcome,
                           "message": str(res.get("message") or res.get("results", {}).get("errorMessage", ""))[:300] if isinstance(res, dict) else ""}
                    records.append(rec)
                    bump(outcome)

                    if outcome in CRASH_CLASSES:
                        detail = rec.get("message") or outcome
                        crashes.append({"type": outcome, "module": module, "analysis": analysis,
                                        "options": options, "detail": detail})
                        log(f"{label} run {i + 1} ... {outcome.upper()}: {detail[:200]}")
                        if cfg.report:
                            write_repro(cfg.report + ".repro.json", seed, module, analysis, cfg.csv, options, detail)
                        raise _StopFuzzing()

                    # Engine-queue wedge detector: a run that never even got scheduled
                    # ("stuck-empty") is how the audit-style wedges manifest. One can be
                    # transient saturation; several in a row mean the queue is wedged and
                    # every remaining run would just burn its timeout, so abort as a crash
                    # (the wedge is really-quite-broken and in one observed case was
                    # followed by a SIGABRT).
                    consecutive_stuck = consecutive_stuck + 1 if outcome == "stuck-empty" else 0
                    if consecutive_stuck >= cfg.max_consecutive_stuck:
                        detail = (f"engine queue wedged: {consecutive_stuck} consecutive runs never got "
                                  f"scheduled within {cfg.timeout_per_run:.0f}s each (last message: "
                                  f"{rec.get('message', '')[:150]})")
                        crashes.append({"type": "wedged", "module": module, "analysis": analysis,
                                        "options": options, "detail": detail})
                        log(f"{label} run {i + 1} ... WEDGED: {detail}")
                        if cfg.report:
                            write_repro(cfg.report + ".repro.json", seed, module, analysis, cfg.csv, options, detail)
                        raise _StopFuzzing()

                    if outcome == "internal-error":
                        log(f"{label} run {i + 1} ... SUSPICIOUS internal error: {rec['message'][:150]}")
                    elif outcome.startswith("unexpected"):
                        log(f"{label} run {i + 1} ... SUSPICIOUS unexpected status: {outcome}")
                    elif outcome in ("validationError", "rejected", "fatalError") and cfg.verbose:
                        log(f"{label} run {i + 1} ... {outcome}: {rec['message'][:120]}")

                    if outcome.startswith("unexpected") and cfg.fail_fast:
                        break

                log(f"{label} ... done ({cfg.runs_per_analysis} runs)")
            except _StopFuzzing:
                break
            finally:
                try:
                    client.call("analysis_remove", {"analysisId": analysis_id}, timeout_s=60)
                except GateFailure as e:
                    log(f"WARNING: analysis_remove failed for {module}/{analysis}: {e}")

        elapsed = time.time() - t_start

        # summary
        log("\n" + "=" * 70)
        log("OPTION FUZZ SUMMARY")
        log("=" * 70)
        log(f"SEED:               {seed}   (reproduce with --seed {seed})")
        log(f"Analyses fuzzed:    {analyses_done}  ({total_runs} runs planned, {elapsed:.0f}s)")
        for cls in sorted(totals):
            marker = "  <-- SUSPICIOUS" if cls == "internal-error" else ""
            log(f"  {cls:<20} {totals[cls]}{marker}")
        if crashes:
            log(f"\nCRASH-CLASS FAILURES: {len(crashes)}")
            for c in crashes:
                log(f"  - [{c['type']}] {c['module']}/{c['analysis']}: {str(c.get('detail'))[:300]}")
        suspicious = [r for r in records
                      if r.get("outcome") == "internal-error" or str(r.get("outcome", "")).startswith("unexpected")]
        if suspicious:
            log(f"\nSuspicious outcomes: {len(suspicious)} (does not fail the run unless --strict)")
            for r in suspicious[:10]:
                log(f"  - {r['module']}/{r['analysis']} run {r.get('run')}: [{r['outcome']}] {r.get('message', '')[:200]}")
        log("=" * 70)

        if cfg.report:
            with open(cfg.report, "w") as f:
                json.dump({
                    "timestamp": datetime.datetime.now().isoformat(),
                    "seed": seed,
                    "runs_done": done,
                    "analyses_done": analyses_done,
                    "totals": totals,
                    "crashes": crashes,
                    "suspicious": [r for r in records if r.get("outcome") == "internal-error"],
                    "records": records,
                }, f, indent=2)
            log(f"Report written to {cfg.report}")

        failed = bool(crashes) or (cfg.strict and bool(suspicious))
        return 1 if failed else 0

    except KeyboardInterrupt:
        log("\nInterrupted.")
        return 130
    except GateFailure as e:
        log(f"FATAL: {e}")
        return 1
    finally:
        if client.mcp is not None:
            client.mcp.close()
        shutdown_jasp(jasp_proc)


class _StopFuzzing(Exception):
    pass


def parse_args() -> argparse.Namespace:
    repo = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--jasp-bin", default=os.path.join(repo, "build", "Desktop", "JASP"))
    p.add_argument("--port", type=int, default=0, help="RPC server port (default: pick a free port)")
    p.add_argument("--csv", default=os.path.join(repo, "Resources", "Data Sets", "debug.csv"))
    p.add_argument("--module", action="append", default=[], help="only fuzz this module (repeatable)")
    p.add_argument("--skip", action="append", default=[], help="skip a module ('jaspFoo') or analysis ('jaspFoo/Bar')")
    p.add_argument("--skip-file", default=None, help="file with skip entries (# comments allowed)")
    p.add_argument("--runs-per-analysis", type=int, default=8, help="mutations per analysis (default 8)")
    p.add_argument("--timeout-per-run", type=float, default=60, help="seconds per run incl. polling (default 60)")
    p.add_argument("--max-consecutive-stuck", type=int, default=3,
                   help="abort as wedged after this many consecutive runs that never got scheduled (default 3)")
    p.add_argument("--startup-timeout", type=float, default=300)
    p.add_argument("--seed", type=int, default=None, help="RNG seed (default: fresh random, printed for reproduction)")
    p.add_argument("--fail-fast", action="store_true", help="stop after the first analysis that misbehaves")
    p.add_argument("--no-mcp", action="store_true", help="skip the jasp-mcp layer, drive JSON-RPC directly")
    p.add_argument("--strict", action="store_true", help="fail the run on -32603 internal errors too")
    p.add_argument("--verbose", action="store_true", help="log every tolerated outcome (validationError etc.)")
    p.add_argument("--jasp-extra-args", default="", help="extra args for the JASP process (shell-quoted)")
    p.add_argument("--jasp-log", default=None, help="where to write JASP stdout/stderr (default: temp file)")
    p.add_argument("--report", default=None, help="write a JSON report to this path")
    p.add_argument("--repro", default=None, help="replay a failure recorded in a *.repro.json file")
    return p.parse_args()


if __name__ == "__main__":
    sys.exit(main())
