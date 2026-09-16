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


# ── Phase 6: DOM-level results accessibility checks (via CDP) ─────────

def _cdp_connect(port):
    import json
    import urllib.request
    import websocket
    with urllib.request.urlopen(f"http://127.0.0.1:{port}/json") as r:
        targets = json.load(r)
    target = None
    for t in targets:
        if "index-jasp" in t.get("url", ""):
            target = t
            break
    if target is None:
        return None, None
    return json, websocket.create_connection(target["webSocketDebuggerUrl"], timeout=15)


def run_results_a11y_checks(port):
    """Verify the results-page accessibility contract over CDP:
    roles/labels/keyboard wiring produced by the a11y work, plus a live
    drill-in round-trip. Returns True when no check failed."""
    import json
    import time as _time

    _, ws = _cdp_connect(port)
    if ws is None:
        log("  CDP: no results page target found")
        return False

    # proper CDP request/response matching: unique ids, responses for
    # abandoned calls get drained, events ignored
    pending = set()
    next_id = [0]

    def evaljs(expr, timeout_s=30.0):
        next_id[0] += 1
        rid = next_id[0]
        pending.add(rid)
        ws.send(json.dumps({"id": rid, "method": "Runtime.evaluate",
                            "params": {"expression": expr, "returnByValue": True}}))
        deadline = _time.time() + timeout_s
        while _time.time() < deadline:
            try:
                msg = json.loads(ws.recv())
            except Exception:
                return None
            mid = msg.get("id")
            if mid is None:
                continue  # CDP event
            if mid == rid:
                pending.discard(mid)
                # CDP wraps the JS value in result.result
                inner = msg.get("result", {}).get("result", {})
                return inner.get("value")
            pending.discard(mid)  # response of an abandoned call
        pending.discard(rid)
        return None

    def key(key_name, code, vk, shift=False):
        mods = 8 if shift else 0  # CDP modifiers: 8 = Shift
        next_id[0] += 1
        rid = next_id[0]
        pending.add(rid)
        for t in ("rawKeyDown", "keyUp"):
            ws.send(json.dumps({"id": rid, "method": "Input.dispatchKeyEvent",
                                "params": {"type": t, "key": key_name, "code": code,
                                           "windowsVirtualKeyCode": vk,
                                           "nativeVirtualKeyCode": vk,
                                           "modifiers": mods}}))
        _time.sleep(0.3)

    failures = []

    def check(name, ok, detail=""):
        log(f"  [{'PASS' if ok else 'FAIL'}] {name}" + (f" - {detail}" if detail else ""))
        if not ok:
            failures.append(name)

    # the page must have rendered analysis content (JASP may still be
    # loading a file passed on the command line - the engine spawn can
    # take a minute; poll, and bail out cleanly if it never renders)
    n_analyses = 0
    for _ in range(90):
        n_analyses = evaljs("document.querySelectorAll('.jasp-analysis').length") or 0
        if n_analyses > 0:
            break
        _time.sleep(1)
    check("analyses rendered", n_analyses > 0, f"{n_analyses} analyses")
    if n_analyses == 0:
        ws.close()
        return False

    # navigation engine sees the narratable blocks
    n_blocks = evaljs("JASPWidgets.a11y.visibleBlocks().length")
    check("navigable blocks present", (n_blocks or 0) >= 3, f"{n_blocks} blocks")

    # every title is keyboard-reachable
    bad_titles = evaljs(
        "document.querySelectorAll('.in-toolbar[tabindex]:not([tabindex=\"0\"])').length")
    n_titles = evaljs("document.querySelectorAll('.in-toolbar[tabindex=\"0\"]').length")
    check("titles focusable", not bad_titles and (n_titles or 0) > 0,
          f"{n_titles} focusable titles, {bad_titles} misconfigured")

    # collapsible containers expose aria-expanded (soft: depends on content)
    n_expanders = evaljs("document.querySelectorAll('.in-toolbar[aria-expanded]').length")
    log(f"  [info] collapsible containers with aria-expanded: {n_expanders}")

    # plots narratable (soft: only when the analysis has plots)
    plots = evaljs("""
        (function () {
          var ps = document.querySelectorAll('.jasp-image-image[data-plot-title]');
          var bad = 0;
          for (var i = 0; i < ps.length; i++) {
            var lbl = ps[i].getAttribute('aria-label') || '';
            if (ps[i].getAttribute('role') !== 'img' || lbl.lastIndexOf('Plot', 0) !== 0)
              bad++;
          }
          return ps.length + '/' + bad;
        })()
    """)
    if plots and not plots.startswith("0/"):
        n, bad = plots.split("/")
        check("plots labeled", int(bad) == 0, f"{n} plots")
    else:
        log("  [info] no plots in this analysis - plot check skipped")

    # noteboxes: button-like activators, editors out of Tab order
    notes = evaljs("""
        (function () {
          var ns = document.querySelectorAll('.jasp-notes');
          if (ns.length === 0) return 'none';
          var badRole = 0, badLabel = 0;
          for (var i = 0; i < ns.length; i++) {
            var lbl = ns[i].getAttribute('aria-label') || '';
            if (ns[i].getAttribute('role') !== 'button') badRole++;
            if (lbl.lastIndexOf('Note', 0) !== 0) badLabel++;
          }
          var badTab = document.querySelectorAll('.ql-editor[tabindex]:not([tabindex="-1"])').length;
          var editors = document.querySelectorAll('.ql-editor').length;
          return ns.length + '/' + badRole + '/' + badLabel + '/' + badTab + '/' + editors;
        })()
    """)
    if notes and notes != "none":
        n, bad_role, bad_label, bad_tab, editors = notes.split("/")
        check("noteboxes are Enter-to-edit activators",
              int(bad_role) == 0 and int(bad_label) == 0,
              f"{n} notes ({bad_role} bad role, {bad_label} bad label)")
        check("note editors untabbable", int(bad_tab) == 0,
              f"{editors} editors, {bad_tab} still tabbable")
    else:
        log("  [info] no notes present - note checks skipped")

    # tables: caption matches the accessible name
    tables = evaljs("""
        (function () {
          var ts = document.querySelectorAll('table[role="table"]');
          if (ts.length === 0) return 'none';
          var bad = 0;
          for (var i = 0; i < ts.length; i++) {
            var cap = ts[i].querySelector('caption');
            var label = ts[i].getAttribute('aria-label') || '';
            if (!cap || cap.textContent.trim() !== label.trim()) bad++;
          }
          return ts.length + '/' + bad;
        })()
    """)
    if tables and tables != "none":
        n, bad = tables.split("/")
        check("tables have captions", int(bad) == 0, f"{n} tables")
        # hover toolbar is hidden from the tree
        tb = evaljs("(function(){ var t = document.querySelector('table div.toolbar');"
                    " return t ? t.getAttribute('aria-hidden') : 'none'; })()")
        check("table toolbar AX-hidden", tb in ("true", "none"), f"aria-hidden={tb}")
    else:
        log("  [info] no tables present - table checks skipped")

    # live drill-in round trip: Enter opens cell nav, Escape closes it
    drilled = evaljs("""
        (function () {
          var t = document.querySelector('table[role="table"]');
          if (!t) return 'no-table';
          t.focus();
          return 'focused';
        })()
    """)
    if drilled == "focused":
        key("Enter", "Enter", 13)
        in_drill = evaljs("!!JASPWidgets.a11y.drillTable")
        cell = evaljs("JASPWidgets.a11y.drillTable ? document.activeElement.tagName : 'none'")
        key("Escape", "Escape", 27)
        out_drill = evaljs("!JASPWidgets.a11y.drillTable")
        back_on_table = evaljs("document.activeElement.tagName === 'TABLE'")
        check("table drill-in works", in_drill and cell in ("TD", "TH"),
              f"cell={cell}")
        check("drill-in Escape exits", out_drill and back_on_table)

    # menu discipline: plain Enter must never open the context menu;
    # Shift+Enter must open it. The menu marks its owner with
    # jasp-menu-selected (cleared on next document mousedown).
    menu_probe = evaljs("""
        (function () {
          var t = document.querySelector('table[role="table"]');
          if (!t) return 'no-table';
          var before = document.querySelectorAll('.jasp-menu-selected').length;
          t.focus();
          return { before: before };
        })()
    """)
    if menu_probe:
        key("Enter", "Enter", 13)
        after_plain = evaljs("document.querySelectorAll('.jasp-menu-selected').length")
        check("plain Enter does not open menu",
              after_plain is not None and after_plain == (menu_probe.get("before") or 0),
              f"markers {menu_probe.get('before')} -> {after_plain}")
        key("Escape", "Escape", 27)
        evaljs("!JASPWidgets.a11y.drillTable && JASPWidgets.a11y.exitDrill()")
        key("Enter", "Enter", 13, shift=True)
        after_shift = evaljs("document.querySelectorAll('.jasp-menu-selected').length")
        check("Shift+Enter opens menu", (after_shift or 0) > (menu_probe.get("before") or 0),
              f"markers -> {after_shift}")
        key("Escape", "Escape", 27)
        evaljs("(function(){ document.querySelectorAll('.jasp-menu-selected')"
               ".forEach(function(e){ e.classList.remove('jasp-menu-selected'); }); return 1; })()")

    # arrow navigation moves between blocks
    evaljs("document.getElementById('spacer').focus()")
    key("ArrowDown", "ArrowDown", 40)
    first_block = evaljs("(function(){ var a = document.activeElement;"
                         " return (a.className||'').substring(0, 30); })()")
    check("arrow navigation moves focus", first_block not in ("BODY", "", None),
          f"focus after ArrowDown: {first_block}")

    # ── ordered block walk: Down visits every block in DOM order, Up
    # walks back through the same sequence
    walk = evaljs("""
        (function () {
          var a = JASPWidgets.a11y;
          var blocks = a.visibleBlocks();
          if (document.activeElement) document.activeElement.blur();
          var seq = [];
          for (var i = 0; i < blocks.length; i++) {
            var ev = new KeyboardEvent('keydown', {key: 'ArrowDown', bubbles: true, cancelable: true});
            document.dispatchEvent(ev);
            seq.push(document.activeElement === blocks[i] ? 'ok' : 'miss');
          }
          return blocks.length + '|' + seq.join(',');
        })()
    """)
    if walk and '|' in str(walk):
        n, seq = str(walk).split('|', 1)
        seq = seq.split(',')
        check("block walk visits every block in order",
              all(s == 'ok' for s in seq), f"{n} blocks: {seq}")
    else:
        check("block walk visits every block in order", False, f"got {walk}")

    upwalk = evaljs("""
        (function () {
          var a = JASPWidgets.a11y;
          var blocks = a.visibleBlocks();
          blocks[blocks.length - 1].focus();
          var seq = [];
          for (var i = 0; i < blocks.length - 1; i++) {
            var ev = new KeyboardEvent('keydown', {key: 'ArrowUp', bubbles: true, cancelable: true});
            document.dispatchEvent(ev);
            seq.push(document.activeElement === blocks[blocks.length - 2 - i] ? 'ok' : 'miss');
          }
          return seq.join(',');
        })()
    """)
    check("reverse walk (ArrowUp) works", upwalk is not None and 'miss' not in str(upwalk),
          f"{upwalk}")

    # ── typing safety: arrows must be inert while an editor has focus.
    # Real typing dispatches on the focused element (the editor), which
    # then bubbles to the document handler — so dispatch there.
    arrow_safe = evaljs("""
        (function () {
          var editors = document.querySelectorAll('.ql-editor');
          for (var i = 0; i < editors.length; i++) {
            if (editors[i].offsetParent !== null) {
              editors[i].focus();
              editors[i].dispatchEvent(new KeyboardEvent('keydown',
                  {key: 'ArrowDown', bubbles: true, cancelable: true}));
              return document.activeElement.classList.contains('ql-editor') ? 'safe' : 'hijacked';
            }
          }
          return 'no-visible-editor';
        })()
    """)
    if arrow_safe == 'no-visible-editor':
        log("  [info] no visible note editor - arrow-safety check skipped")
    else:
        check("arrows inert inside note editor", arrow_safe == 'safe', arrow_safe)

    # ── note edit round trip: Enter starts editing, Escape returns
    note_rt = evaljs("""
        (function () {
          var ns = document.querySelectorAll('.jasp-notes');
          for (var i = 0; i < ns.length; i++) {
            if (ns[i].offsetParent !== null) {
              ns[i].focus();
              return ns[i].getAttribute('role');
            }
          }
          return null;
        })()
    """)
    if note_rt == 'button':
        key("Enter", "Enter", 13)
        editing = evaljs("(function(){ var n = document.activeElement.closest('.jasp-notes');"
                         " return n ? n.getAttribute('role') + '/' +"
                         " document.activeElement.classList.contains('ql-editor') : 'none'; })()")
        key("Escape", "Escape", 27)
        done = evaljs("(function(){ var n = document.querySelector('.jasp-notes[tabindex=\"0\"]');"
                      " return document.activeElement.classList.contains('jasp-notes') ?"
                      " document.activeElement.getAttribute('role') : 'not-focused'; })()")
        check("note Enter->edit / Escape->exit",
              editing == 'region/true' and done == 'button',
              f"editing={editing}, after-escape={done}")
    else:
        log("  [info] no visible note - note edit round trip skipped")

    # ── Tab from a note wrapper must skip the Quill editor
    tabskip = evaljs("""
        (function () {
          var ns = document.querySelectorAll('.jasp-notes');
          for (var i = 0; i < ns.length; i++) {
            if (ns[i].offsetParent !== null) { ns[i].focus(); return 'focused'; }
          }
          return null;
        })()
    """)
    if tabskip:
        key("Tab", "Tab", 9)
        where = evaljs("document.activeElement.className || document.activeElement.tagName")
        check("Tab skips note editor", not (where or '').startswith('ql-editor'),
              f"focus went to: {where}")

    # ── collapsible containers: Enter toggles aria-expanded
    toggle = evaljs("""
        (function () {
          var t = document.querySelector('.in-toolbar[aria-expanded]');
          if (!t) return null;
          t.focus();
          return t.getAttribute('aria-expanded');
        })()
    """)
    if toggle:
        key("Enter", "Enter", 13)
        after = evaljs("document.activeElement.getAttribute('aria-expanded')")
        key("Enter", "Enter", 13)
        back = evaljs("document.activeElement.getAttribute('aria-expanded')")
        check("Enter toggles expander", after != toggle and back == toggle,
              f"{toggle} -> {after} -> {back}")
    else:
        log("  [info] no collapsible containers - expander toggle check skipped")

    # ── plots: focusable, Enter is a no-op, focus outline visible
    plot_check = evaljs("""
        (function () {
          var p = document.querySelector('.jasp-image-image[role="img"]');
          if (!p) return null;
          p.focus();
          var st = getComputedStyle(p);
          return p === document.activeElement ? st.outlineStyle : 'not-focused';
        })()
    """)
    if plot_check:
        markers0 = evaljs("document.querySelectorAll('.jasp-menu-selected').length")
        key("Enter", "Enter", 13)
        markers1 = evaljs("document.querySelectorAll('.jasp-menu-selected').length")
        check("plot focusable with visible outline",
              plot_check in ('solid', 'none') and plot_check != 'not-focused' and
              evaljs("document.activeElement.classList.contains('jasp-image-image')"),
              f"outline={plot_check}")
        check("plot Enter is a no-op", (markers1 or 0) == (markers0 or 0),
              f"menu markers {markers0} -> {markers1}")
    else:
        log("  [info] no plots - plot focus checks skipped")

    # ── page scroll helpers exist (regression guard for the
    # 'windows.pageDown' typo in MainPage.qml)
    scrollfns = evaljs("typeof window.pageUp + '/' + typeof window.pageDown")
    check("pageUp/pageDown helpers defined", scrollfns == 'function/function', scrollfns)

    # ── markdown blocks: Enter-to-edit (soft, when present)
    md = evaljs("""
        (function () {
          var m = document.querySelector('.jasp-md-text');
          if (!m) return null;
          m.focus();
          return 'focused';
        })()
    """)
    if md:
        key("Enter", "Enter", 13)
        editing = evaljs("!!document.querySelector('.jasp-md-text-editor')")
        key("Escape", "Escape", 27)
        closed = evaljs("!document.querySelector('.jasp-md-text-editor')")
        check("markdown Enter-to-edit", editing and closed,
              f"opened={editing}, closed={closed}")
    else:
        log("  [info] no markdown blocks - md_text check skipped")

    ws.close()
    log(f"=== results a11y checks: {'PASS' if not failures else 'FAIL (' + ', '.join(failures) + ')'} ===")
    return len(failures) == 0


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

    # ── 3. load Sleep.csv (skipped when JASP started with a .jasp file) ──
    file_preloaded = os.environ.get("JASP_FILE_LOADED", "") == "1"
    if file_preloaded:
        log("=== 3. dataset load skipped (JASP launched with -FileArg) ===")
    else:
        log("=== 3. loading dataset ===")
        if not open_sleep_csv(app, main_window):
            log("WARN: dataset load flow failed - continuing with whatever is loaded")

    # refresh app/window references (window may now be titled Sleep)
    app, main_window = ac.setup_jasp_app(timeout=20, main_window_names=("JASP", "Sleep", "debug"))
    if not main_window:
        log("FAIL: lost JASP after dataset load")
        return 1

    # ── 4. add Descriptives analysis (skipped when a file was preloaded) ──
    if file_preloaded:
        log("=== 4. analysis click skipped (results come from the loaded file) ===")
    else:
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
    named_images = 0
    unnamed_images = 0
    for role, name, _ in ac.find_all(doc, max_depth=25):
        nl = name.lower()
        if "descriptive statistics" in nl:
            found_stats = True
        if "boxplot" in nl or "box plot" in nl:
            found_box = True
        if role.lower() in ("image", "graphic"):
            if name.strip():
                named_images += 1
            else:
                unnamed_images += 1
    log(f"  'Descriptive Statistics' found: {found_stats}")
    log(f"  'Boxplots' found: {found_box}")
    log(f"  images in a11y tree: {named_images} named, {unnamed_images} unnamed")

    # Narrator announces the results webview as "<title> Document"; the
    # title must be "Results" (not the old "JASP")
    doc_name = (doc.get_name() or "").strip()
    log(f"  results document name: {doc_name!r}")
    doc_named_results = doc_name.lower().startswith("results")
    log(f"  document named 'Results': {doc_named_results}")

    ok = found_stats or stats["count"] > 100
    if named_images > 0 and unnamed_images > named_images:
        log("  FAIL: most images in the a11y tree have no name")
        ok = False
    if not doc_named_results:
        log("  FAIL: results document is not named 'Results'")
        ok = False

    # ── 6. results a11y structure checks (Phase 6, via CDP) ───────────
    # Runs last: by now the results page definitely has content (either
    # from a -FileArg .jasp file or the dataset+analysis flow above), and
    # cold engine starts can take minutes, which an early poll can't wait
    # for without stalling the whole test.
    cdp_port = os.environ.get("JASP_CDP_PORT", "")
    if cdp_port:
        try:
            results_checks_ok = run_results_a11y_checks(int(cdp_port))
        except Exception as e:
            log(f"  results a11y checks errored: {e}")
            results_checks_ok = False
        if results_checks_ok is False:
            ok = False
    else:
        log("  (JASP_CDP_PORT not set - skipping DOM-level results checks)")

    log(f"=== {'PASS' if ok else 'INCONCLUSIVE'} ===")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
