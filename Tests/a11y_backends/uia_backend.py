#!/usr/bin/env python3
"""
Windows UIA backend for JASP accessibility tests, built on pywinauto
(backend="uia").

Node objects wrap pywinauto UIA wrappers and expose the AT-SPI-style duck
API the shared test helpers expect. UIA ControlType names are mapped to
AT-SPI role names so test assertions stay identical across platforms.
"""

import os
import re
import time

from a11y_backends import A11yError

try:
    from pywinauto import Desktop
    from pywinauto.keyboard import send_keys
except ImportError as e:
    raise A11yError(
        f"pywinauto not available: {e}\n"
        "Install with: pip install pywinauto"
    )

_UA_DESKTOP = Desktop(backend="uia")


def _esc_send_keys(text):
    """Escape text for pywinauto.keyboard.send_keys special syntax."""
    return re.sub(r"([+^%~(){}])", r"{\1}", text)


# UIA ControlType -> AT-SPI style role names (lowercase), chosen so that
# existing test assertions ("button", "document web", "menu item", ...)
# keep matching on Windows.
CONTROL_TYPE_ROLES = {
    "Button":         "button",
    "SplitButton":    "button",
    "Document":       "document web",
    "MenuItem":       "menu item",
    "Menu":           "menu",
    "MenuBar":        "menu bar",
    "CheckBox":       "check box",
    "RadioButton":    "radio button",
    "Table":          "table",
    "DataGrid":       "table",
    "Edit":           "entry",
    "ComboBox":       "combo box",
    "Text":           "text",
    "Pane":           "panel",
    "Group":          "section",
    "Custom":         "section",
    "Window":         "frame",
    "Dialog":         "dialog",
    "List":           "list",
    "ListItem":       "list item",
    "Spinner":        "spin box",
    "ToolBar":        "tool bar",
    "Tree":           "tree",
    "TreeItem":       "tree item",
    "HyperLink":      "link",
    "Image":          "image",
    "Slider":         "slider",
    "ProgressBar":    "progress bar",
    "TabItem":        "page tab",
    "Tab":            "page tab list",
    "TitleBar":       "title bar",
    "StatusBar":      "status bar",
    "Separator":      "separator",
    "Header":         "column header",
    "ToolTip":        "tool tip",
    "Thumb":          "scroll bar",
    "Calendar":       "calendar",
}


def _esc(text):
    return text.replace("{", "{{").replace("}", "}}")


class UiaNode:
    """AT-SPI-style duck-typed wrapper around a pywinauto UIA wrapper."""

    __slots__ = ("_w",)

    def __init__(self, wrapper):
        self._w = wrapper

    @property
    def raw(self):
        return self._w

    # ── AT-SPI duck API ──────────────────────────────────────────────

    def get_name(self):
        try:
            return self._w.element_info.name or ""
        except Exception:
            return ""

    def get_role_name(self):
        try:
            ct = self._w.element_info.control_type
        except Exception:
            return "unknown"
        if not ct:
            return "unknown"
        return CONTROL_TYPE_ROLES.get(ct, ct.lower())

    def get_description(self):
        try:
            return self._w.element_info.description or ""
        except Exception:
            return ""

    def get_child_count(self):
        try:
            return len(self._w.children())
        except Exception:
            return 0

    def get_child_at_index(self, index):
        try:
            kids = self._w.children()
            if 0 <= index < len(kids):
                return UiaNode(kids[index])
        except Exception:
            pass
        return None

    # actions: click_element() tries get_n_actions()/get_action_name/do_action
    _ACTION_METHODS = ("invoke", "toggle", "select", "expand", "check")

    def get_n_actions(self):
        return 1

    def get_action_name(self, index):
        return "click"

    def do_action(self, index=0):
        w = self._w
        for meth in self._ACTION_METHODS:
            try:
                getattr(w, meth)()
                return True
            except Exception:
                continue
        # last resort: focus + space (works for many QML buttons)
        try:
            w.set_focus()
            send_keys("{SPACE}")
            return True
        except Exception:
            return False

    def get_component_iface(self):
        """Shim object exposing grab_focus(), mirroring AT-SPI Component."""
        node = self

        class _ComponentShim:
            def grab_focus(self):
                node.grab_focus()

        return _ComponentShim()

    def grab_focus(self):
        try:
            self._w.set_focus()
            return True
        except Exception:
            return False

    def get_rect(self):
        """(left, top, width, height) - triggers the provider bounds call."""
        try:
            r = self._w.rectangle()
            return (int(r.left), int(r.top), int(r.width), int(r.height))
        except Exception:
            return (0, 0, 0, 0)

    def __repr__(self):
        try:
            return f"<UiaNode '{self.get_name()}' role={self.get_role_name()}>"
        except Exception:
            return "<UiaNode <?>>"


class UiaAppNode:
    """Pseudo 'application' node aggregating all top-level windows of a pid."""

    __slots__ = ("_pid",)

    def __init__(self, pid):
        self._pid = pid

    def _windows(self):
        out = []
        try:
            for w in _UA_DESKTOP.windows(process=self._pid):
                try:
                    out.append(UiaNode(w))
                except Exception:
                    pass
        except Exception:
            pass
        return out

    def get_role_name(self):
        return "application"

    def get_name(self):
        return "JASP"

    def get_description(self):
        return ""

    def get_child_count(self):
        return len(self._windows())

    def get_child_at_index(self, index):
        wins = self._windows()
        if 0 <= index < len(wins):
            return wins[index]
        return None

    def __repr__(self):
        return f"<UiaAppNode pid={self._pid}>"


class UiaBackend:
    platform = "uia"

    def __init__(self):
        self._pid = None
        # JASP_PID is how the runner tells tests which process to attach to
        self._pid = int(os.environ["JASP_PID"]) if os.environ.get("JASP_PID", "") else None

    def init(self):
        pass

    # ── application discovery ────────────────────────────────────────

    def app_nodes(self):
        """All application roots we can see: the JASP pid, plus every other
        visible top-level window's owner process (used by dialog discovery)."""
        out = []
        if self._pid:
            out.append(UiaAppNode(self._pid))
        return out

    def _window_wrappers_for_pid(self, pid):
        out = []
        try:
            for w in _UA_DESKTOP.windows(process=pid):
                try:
                    out.append(UiaNode(w))
                except Exception:
                    pass
        except Exception:
            pass
        return out

    def find_jasp_app(self, timeout=30, main_window_names=None):
        if main_window_names is None:
            main_window_names = ("JASP",)
        app = UiaAppNode(self._pid) if self._pid else None
        deadline = time.time() + timeout
        fallback = None
        while time.time() < deadline:
            if app is None:
                time.sleep(1)
                continue
            for win in self._window_wrappers_for_pid(self._pid):
                if win.get_role_name() == "frame" and win.get_child_count() >= 3:
                    wname = win.get_name()
                    if any(wname == n or wname.startswith(n) for n in main_window_names):
                        return app, win
                    # any frame window of the JASP process is a fine fallback:
                    # loaded datasets replace the title entirely ("Sleep (C:\...)")
                    if fallback is None:
                        fallback = win
            if fallback is not None:
                return app, fallback
            time.sleep(1)
        return app, None

    def get_jasp_app(self):
        return UiaAppNode(self._pid) if self._pid else None

    def find_window_by_name(self, app, window_name, timeout=10, role_name=None):
        wl = window_name.lower()
        roles = (role_name,) if role_name else ("frame", "dialog")
        deadline = time.time() + timeout
        while time.time() < deadline:
            apps = [app] if app is not None else self.app_nodes()
            for a in apps:
                for j in range(a.get_child_count()):
                    c = a.get_child_at_index(j)
                    if c and c.get_role_name() in roles and wl in c.get_name().lower():
                        return c
            time.sleep(0.5)
        return None

    def find_file_dialog(self, timeout=10):
        """Find the file dialog. On Windows the native IFileDialog is an
        owned popup that UIA nests INSIDE the main window's subtree, so
        Desktop.windows() never returns it - search the window subtree."""
        deadline = time.time() + timeout
        while time.time() < deadline:
            if self._pid:
                for win in self._window_wrappers_for_pid(self._pid):
                    try:
                        for el in win.raw.descendants(control_type="Window"):
                            if el.element_info.class_name == "#32770":
                                return UiaNode(el)
                    except Exception:
                        pass
            time.sleep(0.5)
        return None

    def grab_window_focus(self):
        if not self._pid:
            return False
        for win in self._window_wrappers_for_pid(self._pid):
            if win.get_role_name() == "frame":
                return win.grab_focus()
        return False

    # ── keyboard synthesis ───────────────────────────────────────────

    _KEY_MAP = {
        0xFF0D: "{ENTER}", 0xFF1B: "{ESC}", 0xFF54: "{DOWN}", 0xFF53: "{RIGHT}",
        0xFF52: "{UP}", 0xFF51: "{LEFT}", 0xFF09: "{TAB}", 0xFF08: "{BACKSPACE}",
        0xFFE3: "^", 0xFFE4: "^", 0xFFE1: "+", 0xFFE2: "+",
    }

    def generate_key_event(self, keyval):
        try:
            if keyval in self._KEY_MAP:
                send_keys(self._KEY_MAP[keyval])
            elif 0x20 <= keyval < 0x7F:
                send_keys(_esc(chr(keyval)))
            else:
                return False
            return True
        except Exception:
            return False

    def ctrl_w(self):
        send_keys("^w")

    def press_escape(self):
        try:
            send_keys("{ESC}")
            return True
        except Exception:
            return False

    def type_path_into_dialog(self, path):
        """Type a file path into the focused filename edit and confirm."""
        send_keys(_esc(path) + "{ENTER}", pause=0.05)

    # ── node-level platform ops ──────────────────────────────────────

    def is_focused(self, node):
        try:
            return bool(node.raw.has_focus())
        except Exception:
            return False

    def set_editable_text(self, node, text):
        w = node.raw
        for meth in ("set_edit_text", "set_value"):
            try:
                getattr(w, meth)(text)
                return True
            except Exception:
                continue
        try:
            w.set_focus()
            send_keys("^a")
            send_keys(_esc(text), pause=0.02)
            return True
        except Exception as e:
            raise A11yError(f"set_editable_text failed on '{node.get_name()}': {e}")

    def shutdown(self):
        pass
