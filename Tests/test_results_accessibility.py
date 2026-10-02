#!/usr/bin/env python3
"""
Check JASP results window accessibility for Sleep.jasp Descriptives output.
Assumes JASP is already running with Sleep.jasp loaded.
"""

import json
import os
import re
import sys
import time
from accessibility_common import (
    click_element, find_document_web, find_all,
    generate_key_event, setup_jasp_app, KEY_DOWN,
)


def _cdp_port():
    flags = os.environ.get("QTWEBENGINE_CHROMIUM_FLAGS", "")
    match = re.search(r"--remote-debugging-port=(\d+)", flags)
    return int(match.group(1)) if match else 9223


def _cdp_ws_for_results():
    try:
        import urllib.request
        import websocket
    except Exception as exc:
        print(f"  CDP unavailable: {exc}")
        return None

    try:
        with urllib.request.urlopen(f"http://127.0.0.1:{_cdp_port()}/json", timeout=3) as response:
            pages = json.load(response)
        target = next((p for p in pages if p.get("title") == "Results"), None)
        if not target:
            return None
        return websocket.create_connection(target["webSocketDebuggerUrl"], timeout=5)
    except Exception as exc:
        print(f"  CDP connect failed: {exc}")
        return None


def _cdp_evaluate(ws, expression):
    ws.send(json.dumps({
        "id": 1,
        "method": "Runtime.evaluate",
        "params": {"expression": expression, "returnByValue": True},
    }))
    while True:
        message = json.loads(ws.recv())
        if message.get("id") == 1:
            return message.get("result", {}).get("result", {}).get("value")


def add_unique(items, item):
    if item not in items:
        items.append(item)


def scan_state(root):
    state = {
        "desc_stats_found": False,
        "boxplots_found": False,
        "tables": [],
        "sections": [],
        "images": [],
    }
    for role, name, _child in find_all(root):
        role_l = role.lower()
        name_l = name.lower()
        if "descriptive statistics" in name_l:
            state["desc_stats_found"] = True
        if "boxplots" in name_l or "box plot" in name_l:
            state["boxplots_found"] = True
        if "table" in role_l and name:
            add_unique(state["tables"], (role, name))
        if "section" in role_l and name:
            add_unique(state["sections"], (role, name))
        if "image" in role_l or "graphic" in role_l:
            add_unique(state["images"], (role, name))
    return state


def merge_state(target, source):
    target["desc_stats_found"] = target["desc_stats_found"] or source["desc_stats_found"]
    target["boxplots_found"] = target["boxplots_found"] or source["boxplots_found"]
    for key in ("tables", "sections", "images"):
        for item in source[key]:
            add_unique(target[key], item)


def scroll_with_cdp(root, state):
    ws = _cdp_ws_for_results()
    if not ws:
        return False
    try:
        size = json.loads(_cdp_evaluate(ws, 'JSON.stringify({h:document.body.scrollHeight, vh:window.innerHeight})'))
        total = max(int(size.get("h") or 0), 0)
        viewport = max(int(size.get("vh") or 600), 100)
        steps = max(4, min(12, int(total / viewport) + 2))
        print(f"  Scrolling results with CDP in {steps} steps ({total}px tall)")
        for i in range(steps + 1):
            y = int(total * i / steps)
            _cdp_evaluate(ws, f"window.scrollTo(0,{y}); void 0")
            time.sleep(0.7)
            merge_state(state, scan_state(root))
            if state["desc_stats_found"] and state["boxplots_found"]:
                return True
        return True
    finally:
        ws.close()


def scroll_with_keys(doc, root, state):
    print("  Scrolling results with key events...")
    click_element(doc)
    time.sleep(0.5)
    for _ in range(40):
        generate_key_event(KEY_DOWN)
        time.sleep(0.05)
        if state["desc_stats_found"] and state["boxplots_found"]:
            break
    time.sleep(1)
    merge_state(state, scan_state(root))


def main():
    app, main_window = setup_jasp_app(timeout=30, main_window_names=("JASP", "Sleep"))
    if not main_window:
        print("FAIL: JASP main window not found via AT-SPI2")
        sys.exit(1)
    print(f"JASP found, main window has {main_window.get_child_count()} children")

    print("\n--- Waiting for results to render ---")
    time.sleep(8)

    doc = find_document_web(app)
    if not doc:
        print("FAIL: document web not found")
        sys.exit(1)

    state = scan_state(main_window)
    if not scroll_with_cdp(main_window, state):
        scroll_with_keys(doc, main_window, state)

    doc = find_document_web(app) or doc
    print(f"  Document: '{doc.get_name()}', {doc.get_child_count()} children")

    print(f"\n  Sections ({len(state['sections'])}):")
    for r, n in state["sections"]:
        print(f"    {r}: \"{n}\"")

    print(f"\n  Tables ({len(state['tables'])}):")
    for r, n in state["tables"]:
        print(f"    {r}: \"{n}\"")

    print(f"\n  Images/Graphics ({len(state['images'])}):")
    for r, n in state["images"]:
        print(f"    {r}: \"{n}\"")

    print(f"\n{'='*60}")
    print("RESULTS:")
    print(f"  'Descriptive Statistics' found: {state['desc_stats_found']}")
    print(f"  'BoxPlots' found:            {state['boxplots_found']}")

    if state["desc_stats_found"] and state["boxplots_found"]:
        print("  PASS - Both expected result elements accessible!")
        return 0
    else:
        print("  FAIL - Some expected result elements missing")
        return 1


if __name__ == "__main__":
    sys.exit(main())
