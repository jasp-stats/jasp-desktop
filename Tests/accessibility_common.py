#!/usr/bin/env python3
"""
Shared utilities for JASP accessibility tests using AT-SPI2.
Import from this module to avoid duplicating tree-search, click,
and window-discovery logic across test files.
"""

import time
import sys
import os
from pathlib import Path

try:
    gi = __import__("gi")
    gi.require_version("Atspi", "2.0")
    from gi.repository import Atspi, GLib

    _GLIB_HANDLER_DONE = False
    if not _GLIB_HANDLER_DONE:
        _GLIB_HANDLER_DONE = True
        def _glib_suppress_fatal(domain, level, message, user_data):
            pass
        for dm in ("GLib", "GLib-GObject", "dbind"):
            GLib.log_set_handler(dm, GLib.LogLevelFlags.LEVEL_ERROR, _glib_suppress_fatal, None)
except ImportError as e:
    print(f"PyGObject not available: {e}")
    sys.exit(77)


class JASPCrashed(Exception):
    """Raised when the JASP process has died."""
    pass


_JASP_PID = None


def _get_jasp_pid():
    global _JASP_PID
    if _JASP_PID is None:
        raw = os.environ.get("JASP_PID", "")
        if raw:
            _JASP_PID = int(raw)
    return _JASP_PID


def is_jasp_alive():
    """Return True if JASP is still running, False if it has died."""
    pid = _get_jasp_pid()
    if pid is None:
        return True
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def require_jasp_alive():
    """Raise JASPCrashed if JASP has exited."""
    if not is_jasp_alive():
        raise JASPCrashed("JASP process has died")


# ── key code constants ────────────────────────────────────────────────

KEY_ENTER      = 0xFF0D
KEY_ESCAPE     = 0xFF1B
KEY_DOWN       = 0xFF54
KEY_RIGHT      = 0xFF53
KEY_RETURN     = 0xFF0D
KEY_CONTROL_L  = 0xFFE3
KEY_CONTROL_R  = 0xFFE4


def close_window_via_shortcut():
    """Send Ctrl+W (close window shortcut) via AT-SPI keyboard events."""
    Atspi.generate_keyboard_event(KEY_CONTROL_L, None, Atspi.KeySynthType.PRESS)
    time.sleep(0.02)
    Atspi.generate_keyboard_event(ord('w'), None, Atspi.KeySynthType.PRESS)
    time.sleep(0.02)
    Atspi.generate_keyboard_event(ord('w'), None, Atspi.KeySynthType.RELEASE)
    time.sleep(0.02)
    Atspi.generate_keyboard_event(KEY_CONTROL_L, None, Atspi.KeySynthType.RELEASE)
    time.sleep(0.5)


def close_window(window_frame, timeout=3):
    """
    Close a window frame via AT-SPI. Tries the Close button first,
    then grabs focus and sends Escape.
    """
    close_btn = find_by_role_and_name(window_frame, "button", "Close", timeout=timeout)
    if close_btn:
        click_element(close_btn)
        time.sleep(1)
        return True
    try:
        ci = window_frame.get_component_iface()
        ci.grab_focus()
        time.sleep(0.3)
    except Exception:
        pass
    generate_key_event(KEY_ESCAPE)
    time.sleep(1)
    return True


def repo_root():
    """Absolute path to the JASP-screenreader repo root."""
    return Path(__file__).resolve().parent.parent.parent


def jasp_binary():
    """Absolute path to the locally built JASP binary."""
    return repo_root() / "jasp-desktop" / "build" / "Desktop" / "JASP"


def tool_script(name):
    """Absolute path to a script in the Tests/ directory."""
    return Path(__file__).resolve().parent / name


# ── tree search helpers ──────────────────────────────────────────────


def find_all(obj, depth=0, max_depth=30):
    """Return list of (role, name, child) tuples for all descendants."""
    elements = []
    if depth > max_depth:
        return elements
    try:
        cc = obj.get_child_count()
        for i in range(cc):
            child = obj.get_child_at_index(i)
            role = child.get_role_name() or "unknown"
            name = child.get_name() or ""
            elements.append((role, name, child))
            elements.extend(find_all(child, depth + 1, max_depth))
    except Exception:
        pass
    return elements


def find_all_by_role(parent, role, name_contains=None, depth=0, max_depth=30):
    """Return list of descendants matching a specific role."""
    results = []
    if depth > max_depth:
        return results
    try:
        cc = parent.get_child_count()
        for i in range(cc):
            child = parent.get_child_at_index(i)
            if child.get_role_name().lower() == role.lower():
                if name_contains is None or name_contains.lower() in child.get_name().lower():
                    results.append(child)
            results.extend(find_all_by_role(child, role, name_contains, depth + 1, max_depth))
    except Exception:
        pass
    return results


def find_by_name(parent, name, role=None, timeout=5, recursive=True):
    """Search for an element by name (optionally by role) with retries."""
    name_lower = name.lower()
    for _ in range(timeout * 2):
        try:
            result = _search(parent, name_lower, role, recursive)
            if result:
                return result
        except Exception:
            pass
        require_jasp_alive()
        time.sleep(0.5)
    return None


def _search(obj, name_lower, role, recursive):
    for i in range(obj.get_child_count()):
        try:
            child = obj.get_child_at_index(i)
            if name_lower in child.get_name().lower():
                if role is None or child.get_role_name().lower() == role.lower():
                    return child
            if role is not None and child.get_role_name().lower() == role.lower():
                if child.get_name().lower() == name_lower:
                    return child
            if recursive:
                result = _search(child, name_lower, role, recursive)
                if result:
                    return result
        except Exception:
            pass
    return None


def find_by_desc(parent, description, timeout=3):
    """Find element by accessible description."""
    desc_lower = description.lower()
    for _ in range(timeout * 2):
        for i in range(parent.get_child_count()):
            try:
                child = parent.get_child_at_index(i)
                if desc_lower in (child.get_description().lower() or ""):
                    return child
                result = _search_desc(child, desc_lower)
                if result:
                    return result
            except Exception:
                pass
        require_jasp_alive()
        time.sleep(0.5)
    return None


def _search_desc(obj, desc_lower):
    for i in range(obj.get_child_count()):
        try:
            child = obj.get_child_at_index(i)
            if desc_lower in (child.get_description().lower() or ""):
                return child
            result = _search_desc(child, desc_lower)
            if result:
                return result
        except Exception:
            pass
    return None


def find_by_role_and_name(parent, role_name, name, timeout=5):
    """Search for an element by role and name (checks parent node, too)."""
    role_lower = role_name.lower()
    name_lower = name.lower()
    for _ in range(timeout * 2):
        try:
            result = _search_by_role_and_name(parent, role_lower, name_lower)
            if result:
                return result
        except Exception:
            pass
        require_jasp_alive()
        time.sleep(0.5)
    return None


def _search_by_role_and_name(obj, role_lower, name_lower):
    try:
        if role_lower == obj.get_role_name().lower():
            if name_lower in (obj.get_name() or "").lower():
                return obj
    except Exception:
        pass
    try:
        for i in range(obj.get_child_count()):
            child = obj.get_child_at_index(i)
            result = _search_by_role_and_name(child, role_lower, name_lower)
            if result:
                return result
    except Exception:
        pass
    return None


def find_file_dialog(timeout=10):
    """Find any non-JASP file dialog / frame on the AT-SPI desktop."""
    for _ in range(timeout * 2):
        try:
            desktop = Atspi.get_desktop(0)
            for i in range(desktop.get_child_count()):
                a = desktop.get_child_at_index(i)
                for j in range(a.get_child_count()):
                    try:
                        c = a.get_child_at_index(j)
                        role = c.get_role_name()
                        if role in ("frame", "dialog", "file chooser") and c.get_child_count() > 0:
                            name = c.get_name()
                            if name not in ("JASP", "Data Preview") and "jasp" not in name.lower():
                                return c
                    except Exception:
                        pass
        except Exception:
            pass
        require_jasp_alive()
        time.sleep(0.5)
    return None


def grab_window_focus():
    """Grab X11 focus for the first JASP frame window."""
    try:
        desktop = Atspi.get_desktop(0)
        for i in range(desktop.get_child_count()):
            a = desktop.get_child_at_index(i)
            if "jasp" in a.get_name().lower():
                for j in range(a.get_child_count()):
                    try:
                        c = a.get_child_at_index(j)
                        if c.get_role_name() == "frame":
                            c.grab_focus()
                            return True
                    except Exception:
                        pass
    except Exception:
        pass
    return False


def setup_jasp_app(timeout=30, main_window_names=None):
    """Initialize AT-SPI, find JASP, dismiss dialogs. Returns (app, main_window)."""
    Atspi.init()
    app, main_window = find_jasp_app(timeout=timeout, main_window_names=main_window_names)
    if not main_window:
        return None, None
    for _ in range(5):
        dismiss_dialogs(app)
        time.sleep(1)
    return app, main_window


def robust_search(get_app, func, *args, max_retries=3):
    """Call func(app, *args), retrying with fresh app references on failure."""
    for attempt in range(max_retries):
        require_jasp_alive()
        try:
            app = get_app()
            if not app:
                time.sleep(1.5)
                continue
            result = func(app, *args)
            if result is not None and (not isinstance(result, (list, tuple)) or len(result) > 0):
                return result
            time.sleep(1.5)
        except Exception as e:
            if "no longer exists" in str(e) or "Did not receive a reply" in str(e):
                time.sleep(1.5)
    return func(get_app(), *args)


def open_file_menu(app, main_window):
    """Click Main menu and verify the file menu opened."""
    close_menu()
    time.sleep(1)
    btn = find_by_role_and_name(main_window, "button", "Main menu")
    if not btn:
        return False
    if not click_element(btn):
        return False
    time.sleep(5)
    elements = find_all(app, max_depth=6)
    names = [name.lower() for _, name, _ in elements]
    return any("save as" in n or "export results" in n for n in names)


# ── interaction helpers ──────────────────────────────────────────────


def click_element(element):
    """Click an accessible element via its first click/press/activate action."""
    try:
        n_actions = element.get_n_actions()
    except AttributeError:
        try:
            n_actions = element.get_action_count()
        except AttributeError:
            try:
                action_iface = element.query_action()
                n_actions = action_iface.get_n_actions()
            except Exception:
                try:
                    element.do_action(0)
                    return True
                except Exception:
                    pass
                return False

    if n_actions == 0:
        return False

    for a in range(n_actions):
        try:
            aname = element.get_action_name(a).lower()
        except Exception:
            aname = ""
        if "click" in aname or "press" in aname or "activate" in aname or "toggle" in aname or n_actions == 1:
            try:
                element.do_action(a)
                return True
            except Exception:
                pass

    try:
        element.do_action(0)
        return True
    except Exception:
        pass

    return False


def get_jasp_app():
    """Get a fresh reference to the JASP application with the most children (main window app)."""
    try:
        desktop = Atspi.get_desktop(0)
        best = None
        best_cc = -1
        for i in range(desktop.get_child_count()):
            a = desktop.get_child_at_index(i)
            if "jasp" in a.get_name().lower():
                cc = a.get_child_count()
                if cc > best_cc:
                    best_cc = cc
                    best = a
        return best
    except Exception:
        pass
    return None


# ── app/window discovery ─────────────────────────────────────────────


def find_jasp_app(timeout=30, main_window_names=None):
    """
    Wait for JASP to start and return (app, main_window).
    JASP may register multiple applications on the AT-SPI bus;
    main_window_names is a set/tuple of acceptable frame names.
    Defaults to ('JASP',) for bare startup; pass ('JASP', 'Sleep') etc.
    when loading a .jasp file by name.
    If None, accepts any frame with > 3 children.
    """
    if main_window_names is None:
        main_window_names = ("JASP",)

    Atspi.init()
    app = None
    main_window = None
    for attempt in range(timeout):
        require_jasp_alive()
        time.sleep(1)
        try:
            desktop = Atspi.get_desktop(0)
            for i in range(desktop.get_child_count()):
                a = desktop.get_child_at_index(i)
                if "jasp" not in a.get_name().lower():
                    continue
                for j in range(a.get_child_count()):
                    try:
                        c = a.get_child_at_index(j)
                        if c.get_role_name() == "frame" and c.get_child_count() > 3:
                            if c.get_name() in main_window_names:
                                app = a
                                main_window = c
                                break
                    except Exception:
                        pass
                if main_window:
                    break
        except Exception:
            pass
        if main_window:
            break
    return app, main_window


def find_document_web(app):
    """
    Find the most content-rich document web in the JASP app.
    JASP has multiple document web elements (help, about, community, results).
    Returns the one with the largest descendant count.

    NOTE: QtWebEngine may nest an inner 'document web' child inside the
    outer 'document web'. Callers that need the innermost content root
    should check for single-child nesting and recurse:
        if doc.get_child_count() == 1:
            inner = doc.get_child_at_index(0)
            if 'document' in inner.get_role_name().lower():
                doc = inner
    """
    all_docs = find_all_by_role(app, "document web")
    if not all_docs:
        return None

    best_doc = None
    best_total = 0
    for d in all_docs:
        try:
            total = d.get_child_count()
            for j in range(min(d.get_child_count(), 3)):
                try:
                    cc = d.get_child_at_index(j)
                    total += cc.get_child_count()
                except Exception:
                    pass
        except Exception:
            continue
        if total > best_total:
            best_total = total
            best_doc = d
    return best_doc


def dismiss_dialogs(app):
    """Click Close/OK buttons on any startup dialogs."""
    for i in range(app.get_child_count()):
        try:
            child = app.get_child_at_index(i)
            if child.get_role_name() != "frame":
                continue
            name = child.get_name()
            if name in ("JASP", "Data Preview"):
                continue
            for j in range(child.get_child_count()):
                try:
                    gc = child.get_child_at_index(j)
                    gc_name = gc.get_name().lower()
                    if gc_name in ("close", "continue", "ok", "accept", "no encryption", "skip"):
                        for a in range(gc.get_action_count()):
                            if "click" in gc.get_action_name(a).lower():
                                gc.do_action(a)
                                return
                except Exception:
                    pass
        except Exception:
            pass


def find_window_by_name(app, window_name, timeout=10, role_name=None):
    """Find a frame/window child of app by name, with retry.
    If app is None, searches across all desktop apps.
    If role_name is given, match that role instead of 'frame'/'window'."""
    wl = window_name.lower()
    roles = (role_name,) if role_name else ("frame", "window")
    for _ in range(timeout * 2):
        desktop = Atspi.get_desktop(0)
        apps_to_search = [desktop.get_child_at_index(i) for i in range(desktop.get_child_count())] if app is None else [app]
        for a in apps_to_search:
            try:
                for j in range(a.get_child_count()):
                    try:
                        c = a.get_child_at_index(j)
                        if c.get_role_name() in roles and wl in c.get_name().lower():
                            return c
                    except Exception:
                        pass
            except Exception:
                pass
        require_jasp_alive()
        time.sleep(0.5)
    return None


def find_all_buttons(parent, max_depth=8):
    """Return list of (name, element) tuples for all button-role descendants."""
    results = []
    elements = find_all(parent, max_depth=max_depth)
    for role, name, child in elements:
        if "button" in role.lower() and not child.get_name().startswith(("Tool",)):
            results.append((name, child))
    return results


def find_menu_items(parent):
    """Return list of (name, element) for all menu-item-role descendants."""
    results = []
    elements = find_all(parent, max_depth=8)
    for role, name, child in elements:
        if "menu item" in role.lower():
            results.append((name, child))
    return results


def find_menu_items_global(app, timeout=3):
    """
    Find all menu items in the AT-SPI tree, including those in transient
    popup windows that are not direct children of the app.
    Searches both the app subtree and the desktop for transient frames.
    """
    items = find_menu_items(app)
    if items:
        return items

    for _ in range(timeout * 4):
        try:
            desktop = Atspi.get_desktop(0)
            for i in range(desktop.get_child_count()):
                a = desktop.get_child_at_index(i)
                for j in range(a.get_child_count()):
                    try:
                        c = a.get_child_at_index(j)
                        if c.get_role_name() in ("popup menu", "window", "frame", "dialog", "popup tool tip"):
                            items = find_menu_items(c)
                            if items:
                                return items
                    except Exception:
                        pass
        except Exception:
            pass
        time.sleep(0.25)
    return items


def generate_key_event(keyval):
    """Send a key event via AT-SPI to generate keyboard events."""
    try:
        Atspi.generate_keyboard_event(keyval, None, Atspi.KeySynthType.SYM)
        return True
    except Exception:
        return False


def type_text(text, delay=0.01):
    """Type a string character-by-character via AT-SPI keyboard events."""
    for ch in text:
        generate_key_event(ord(ch))
        time.sleep(delay)


def edit_cell_text(app, new_value, timeout=5):
    """
    Find the editable text cell in the data table and set its content
    via the AT-SPI EditableText interface, then commit with Enter.

    Qt6 maps QML Accessible.EditableText to AT-SPI role "text".
    Returns True on success, None if the element was not found.
    Raises AssertionError if set_text_contents fails.
    """
    editable = None
    for role_name in ("editable text", "text", "edit bar"):
        editable = find_by_role_and_name(app, role_name, "Edit cell value", timeout=timeout)
        if editable:
            break
    if not editable:
        editable = find_by_name(app, "Edit cell value", timeout=timeout)
    if not editable:
        return None

    try:
        ei = editable.get_editable_text_iface()
        ei.set_text_contents(new_value)
    except Exception:
        raise AssertionError(f"set_text_contents failed on '{editable.get_name()}'")

    time.sleep(0.3)
    generate_key_event(KEY_ENTER)
    time.sleep(1.5)
    return True


def set_editable_text(element, new_value, commit_with_enter=True):
    """
    Set the text of any AT-SPI editable text element via
    get_editable_text_iface, then optionally commit with Enter.

    Returns True on success, raises Exception on failure.
    """
    try:
        ei = element.get_editable_text_iface()
        ei.set_text_contents(new_value)
    except Exception as e:
        raise RuntimeError(f"set_text_contents failed on '{element.get_name()}': {e}")

    if commit_with_enter:
        time.sleep(0.3)
        generate_key_event(KEY_ENTER)
        time.sleep(1.5)
    return True




def has_focus(element):
    """Check if an AT-SPI element has keyboard focus."""
    try:
        return element.get_state_set().contains(Atspi.StateType.FOCUSED)
    except Exception:
        return False


def find_focused(app, max_depth=20):
    """Find the focused element in the AT-SPI tree under app."""
    try:
        all_elements = find_all(app, max_depth=max_depth)
        for role, name, elem in all_elements:
            try:
                if elem.get_state_set().contains(Atspi.StateType.FOCUSED):
                    return elem
            except Exception:
                pass
    except Exception:
        pass
    return None


def close_menu():
    """Send Escape key to close any open menus/dialogs."""
    for _ in range(3):
        try:
            Atspi.generate_keyboard_event(KEY_ESCAPE, None, Atspi.KeySynthType.SYM)
        except Exception:
            pass
        time.sleep(0.3)


def ensure_menu_closed(app, main_window):
    """Press Escape until the accessible tree returns to baseline size."""
    try:
        close_menu()
        time.sleep(0.5)
        dismiss_dialogs(app)
        time.sleep(0.5)
    except Exception:
        pass


def wait_for_element(parent, name, role="button", timeout=10):
    """Wait for an element by name and role to appear, with retries."""
    for _ in range(timeout * 2):
        result = find_by_name(parent, name, role, timeout=1)
        if result:
            return result
        time.sleep(0.5)
    return None


def count_tree_elements(obj, max_depth=8):
    """Count total accessible elements in a subtree."""
    elements = find_all(obj, max_depth=max_depth)
    return len(elements)


# ── debug helpers ────────────────────────────────────────────────────


def dump_tree(obj, depth=0, max_depth=3):
    """Dump accessible tree for debugging."""
    if depth > max_depth:
        return
    try:
        n = obj.get_name()
        r = obj.get_role_name()
        d = getattr(obj, "get_description", lambda: "")()
        cc = obj.get_child_count()
        info = f"{'  ' * depth}{n!r} role={r}"
        if d:
            info += f" desc={d!r}"
        info += f" cc={cc}"
        print(info)
        for i in range(min(cc, 10)):
            try:
                child = obj.get_child_at_index(i)
                dump_tree(child, depth + 1, max_depth)
            except Exception:
                pass
    except Exception:
        print(f"{'  ' * depth}[error]")