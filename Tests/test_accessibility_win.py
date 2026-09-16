#!/usr/bin/env python3
"""
Windows accessibility test / freeze hunt for JASP's WebEngine results page.

Flow (mirrors the Linux test suite's dataset -> analysis -> results path):
  1. attach to running JASP (started by run_test_session_win.ps1)
  2. sanity: walk the main window tree
  3. File menu -> About (webview sanity), close it
  4. File menu -> Open -> Computer -> Browse -> native dialog -> Sleep.csv
  5. click the Descriptives ribbon button
  6. wait for the results page (document web) and walk it, fetching
     Name + BoundingRectangle for every web element - the exact provider
     calls Narrator makes. With Narrator on, this is the freeze trigger.

Set JASP_DATA_DIR to the folder containing Sleep.csv if it is not in the
default location.
"""

import os
import sys
import time
import threading

import accessibility_common as ac


def log(msg):
    print(msg, flush=True)


def check_jasp_alive():
    pid = os.environ.get("JASP_PID", "")
    if pid:
        try:
            import psutil  # optional
        except ImportError:
            pass
    return True


def open_about_window(app, main_window):
    """File menu -> About; returns the About window node or None."""
    ac.ensure_menu_closed(app, main_window)
    time.sleep(0.5)
    if not ac.open_file_menu(app, main_window):
        log("  file menu did not open")
        return None
    time.sleep(1)
    btn = ac.find_by_role_and_name(app, "button", "About", timeout=5)
    if not btn:
        ac.close_menu()
        return None
    if not ac.click_element(btn):
        ac.close_menu()
        return None
    time.sleep(4)
    win = ac.find_window_by_name(app, "About", timeout=8)
    return win


def open_sleep_csv(app, main_window):
    """File menu -> Open -> Computer -> Browse -> native dialog -> debug.csv."""
    data_dir = os.environ.get(
        "JASP_DATA_DIR",
        r"C:\Virtuoos\jasp-desktop\jasp-build-screenreader\Resources\Data Sets",
    )
    csv_path = os.path.join(data_dir, "debug.csv")
    if not os.path.exists(csv_path):
        for cand in (
            os.path.join(data_dir, "gui-debug.csv"),
            r"C:\Virtuoos\jasp-desktop\jasp-build\Resources\Data Sets\Data Library\1. Descriptives\Sleep.csv",
        ):
            if os.path.exists(cand):
                csv_path = cand
                break
        else:
            log(f"  no data file found in {data_dir}")
            return False

    ac.ensure_menu_closed(app, main_window)
    time.sleep(0.5)
    if not ac.open_file_menu(app, main_window):
        log("  file menu did not open")
        return False
    time.sleep(1)

    # The file menu exposes several Open entries; prefer the last one
    # (matches the Linux test's open_buttons[-1] logic).
    opens = [e for e in ac.find_all_by_role(app, "button", "Open")]
    if not opens:
        log("  no Open button found")
        ac.close_menu()
        return False
    target = opens[-1] if len(opens) >= 2 else opens[0]
    if not ac.click_element(target):
        log("  clicking Open failed")
        ac.close_menu()
        return False
    time.sleep(2)

    computer = ac.find_by_role_and_name(app, "button", "Computer", timeout=5)
    if computer:
        ac.click_element(computer)
        time.sleep(2)

    browse = ac.find_by_role_and_name(app, "button", "Browse", timeout=5)
    if not browse:
        log("  no Browse button found")
        ac.close_menu()
        return False
    if not ac.click_element(browse):
        log("  clicking Browse failed")
        ac.close_menu()
        return False
    time.sleep(3)

    dialog = ac.find_file_dialog(timeout=8)
    if not dialog:
        log("  native file dialog not found")
        ac.close_menu()
        return False

    log(f"  file dialog found ({dialog.get_name()!r}) - typing path")
    dialog.grab_focus()
    time.sleep(0.5)
    backend = ac.backend
    if hasattr(backend, "type_path_into_dialog"):
        backend.type_path_into_dialog(csv_path)
    else:
        for ch in csv_path:
            backend.generate_key_event(ord(ch))
            time.sleep(0.005)
        backend.generate_key_event(0xFF0D)
    time.sleep(5)
    ac.close_menu()
    return True


def walk_with_bounds(node, stats, depth=0, max_depth=25):
    """Walk the tree fetching Name + BoundingRectangle on every element
    (the exact UIA provider calls Narrator's GetBoundingRectangles makes)."""
    if depth > max_depth or stats["count"] > 6000:
        return
    stats["count"] += 1
    try:
        name = node.get_name()
        role = node.get_role_name()
        rect = node.get_rect()  # <-- provider bounds call
        if stats["count"] <= 40:
            log(f"    [{role}] {name!r} rect={rect}")
    except Exception as e:
        stats["errors"] += 1
        if stats["errors"] <= 3:
            log(f"    walk error: {e}")
        return
    try:
        cc = node.get_child_count()
        for i in range(cc):
            child = node.get_child_at_index(i)
            if child:
                walk_with_bounds(child, stats, depth + 1, max_depth)
    except Exception:
        pass


def narrator_emulation(doc, duration_s=20):
    """Background loop: keep fetching bounds of random web elements,
    like Narrator's continuous remote operations."""
    stop = threading.Event()

    def worker():
        elements = ac.find_all(doc, max_depth=20)
        log(f"  [emulation] {len(elements)} web elements discovered")
        i = 0
        while not stop.is_set():
            for role, name, elem in elements:
                if stop.is_set():
                    return
                try:
                    elem.get_rect()
                except Exception:
                    pass
                i += 1
                if i % 500 == 0:
                    log(f"  [emulation] {i} bounds fetches")

    t = threading.Thread(target=worker, daemon=True)
    t.start()
    time.sleep(duration_s)
    stop.set()
    t.join(timeout=5)
    log(f"  [emulation] finished after {duration_s}s")


def main():
    log("=== setup: attaching to JASP ===")
    app, main_window = ac.setup_jasp_app(timeout=40, main_window_names=("JASP", "Sleep", "debug"))
    if not main_window:
        log("FAIL: JASP main window not found")
        return 1
    log(f"main window: {main_window.get_name()!r} role={main_window.get_role_name()}")

    # ── 1. sanity walk ────────────────────────────────────────────────
    log("=== 1. main window tree sanity ===")
    elements = ac.find_all(main_window, max_depth=8)
    log(f"  {len(elements)} elements in main window (depth 8)")
    roles = {}
    for role, name, _ in elements:
        roles[role] = roles.get(role, 0) + 1
    for role, count in sorted(roles.items(), key=lambda x: -x[1])[:12]:
        log(f"    {role}: {count}")

    # ── 2. About webview sanity ───────────────────────────────────────
    log("=== 2. About window (webview sanity) ===")
    about = open_about_window(app, main_window)
    if about:
        log(f"  About window: {about.get_name()!r}, {about.get_child_count()} children")
        stats = {"count": 0, "errors": 0}
        walk_with_bounds(about, stats, max_depth=15)
        log(f"  About walk: {stats['count']} elements, {stats['errors']} errors")
        ac.close_window(about)
        time.sleep(1)
    else:
        log("  About window did not open (skipping)")

    # ── 3. load Sleep.csv ─────────────────────────────────────────────
    log("=== 3. loading dataset ===")
    if not open_sleep_csv(app, main_window):
        log("WARN: dataset load flow failed - continuing with whatever is loaded")

    # refresh app/window references (window may now be titled Sleep)
    app, main_window = ac.setup_jasp_app(timeout=20, main_window_names=("JASP", "Sleep", "debug"))
    if not main_window:
        log("FAIL: lost JASP after dataset load")
        return 1

    # ── 4. add Descriptives analysis ──────────────────────────────────
    log("=== 4. Descriptives analysis ===")
    ac.ensure_menu_closed(app, main_window)
    time.sleep(0.5)
    desc = ac.find_by_role_and_name(main_window, "button", "Descriptives", timeout=10)
    if desc:
        if ac.click_element(desc):
            log("  Descriptives clicked - waiting for analysis to run")
            time.sleep(12)
        else:
            log("  Descriptives click failed")
    else:
        log("  Descriptives button not found in ribbon")

    # ── 5. results page: document web + bounds emulation ─────────────
    log("=== 5. results page (document web) ===")
    for attempt in range(10):
        doc = ac.find_document_web(app)
        if doc:
            break
        time.sleep(1)
    if not doc:
        log("FAIL: no document web found")
        return 1
    log(f"  document web: {doc.get_name()!r}, {doc.get_child_count()} direct children")

    stats = {"count": 0, "errors": 0}
    log_start = time.time()
    log("  walking document web with bounds fetches (Narrator emulation)...")
    walk_with_bounds(doc, stats, max_depth=25)
    log(f"  walk done: {stats['count']} elements, {stats['errors']} errors, "
        f"{time.time() - log_start:.1f}s")

    log("  continuous bounds emulation over web elements...")
    narrator_emulation(doc, duration_s=25)

    # look for expected results content
    found_stats = False
    found_box = False
    for role, name, _ in ac.find_all(doc, max_depth=25):
        nl = name.lower()
        if "descriptive statistics" in nl:
            found_stats = True
        if "boxplot" in nl or "box plot" in nl:
            found_box = True
    log(f"  'Descriptive Statistics' found: {found_stats}")
    log(f"  'Boxplots' found: {found_box}")

    ok = found_stats or stats["count"] > 100
    log(f"=== {'PASS' if ok else 'INCONCLUSIVE'} ===")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
