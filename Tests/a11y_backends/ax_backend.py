#!/usr/bin/env python3
"""
macOS Accessibility (AX) backend for JASP accessibility tests, built on
pyobjc's ApplicationServices (HIServices).

Node objects wrap AXUIElementRef instances and expose the AT-SPI-style duck
API the shared test helpers expect. VoiceOver reads through this same API,
so this backend is also the tool for debugging the macOS freeze/crash.

STATUS: implemented, best-effort — needs validation on a real mac.
"""

import time

from a11y_backends import A11yError

try:
    import ApplicationServices as AS
    from ApplicationServices import (
        AXUIElementCreateApplication,
        AXUIElementCopyAttributeValue,
        AXUIElementPerformAction,
        AXUIElementSetAttributeValue,
        AXUIElementIsAttributeSettable,
        kAXChildrenAttribute,
        kAXRoleAttribute,
        kAXTitleAttribute,
        kAXDescriptionAttribute,
        kAXFocusedAttribute,
        kAXFocusedUIElementAttribute,
        kAXValueAttribute,
        kAXPressAction,
        kAXMainWindowAttribute,
        kAXWindowsAttribute,
    )
except ImportError as e:
    raise A11yError(f"pyobjc ApplicationServices not available: {e}")


# AX role -> AT-SPI style role names (lowercase)
AX_ROLE_MAP = {
    "AXButton":       "button",
    "AXWebArea":      "document web",
    "AXMenuItem":     "menu item",
    "AXMenu":         "menu",
    "AXMenuBar":      "menu bar",
    "AXCheckBox":     "check box",
    "AXRadioButton":  "radio button",
    "AXTable":        "table",
    "AXTextArea":     "entry",
    "AXTextField":    "entry",
    "AXComboBox":     "combo box",
    "AXStaticText":   "text",
    "AXGroup":        "section",
    "AXSplitGroup":   "split panel",
    "AXScrollArea":   "scroll pane",
    "AXWindow":       "frame",
    "AXDialog":       "dialog",
    "AXList":         "list",
    "AXImage":        "image",
    "AXSlider":       "slider",
    "AXProgressIndicator": "progress bar",
    "AXTabGroup":     "page tab list",
    "AXRadioButton":  "radio button",
    "AXRow":          "table row",
    "AXColumn":       "table column",
    "AXToolbar":      "tool bar",
    "AXPopUpButton":  "combo box",
    "AXMenuButton":   "button",
    "AXGenericElement": "section",
    "AXApplication":  "application",
}


def _copy_attr(element, attr):
    try:
        ok, value = AXUIElementCopyAttributeValue(element, attr, None)
        if ok == 0:
            return value
    except Exception:
        pass
    return None


class AxNode:
    """AT-SPI-style duck-typed wrapper around an AXUIElement."""

    __slots__ = ("_e", "_role")

    def __init__(self, element):
        self._e = element
        role = _copy_attr(element, kAXRoleAttribute)
        self._role = role if isinstance(role, str) else ""

    @property
    def raw(self):
        return self._e

    def get_role_name(self):
        return AX_ROLE_MAP.get(self._role, (self._role or "unknown").lower())

    def get_name(self):
        v = _copy_attr(self._e, kAXTitleAttribute)
        if not v:
            v = _copy_attr(self._e, kAXDescriptionAttribute)
        if not v:
            v = _copy_attr(self._e, kAXValueAttribute)
        return v if isinstance(v, str) else ""

    def get_description(self):
        v = _copy_attr(self._e, kAXDescriptionAttribute)
        return v if isinstance(v, str) else ""

    def get_child_count(self):
        kids = _copy_attr(self._e, kAXChildrenAttribute)
        return len(kids) if kids else 0

    def get_child_at_index(self, index):
        kids = _copy_attr(self._e, kAXChildrenAttribute)
        if kids and 0 <= index < len(kids):
            return AxNode(kids[index])
        return None

    def get_n_actions(self):
        return 1

    def get_action_name(self, index):
        return "press"

    def do_action(self, index=0):
        try:
            return AXUIElementPerformAction(self._e, kAXPressAction) == 0
        except Exception:
            return False

    def get_component_iface(self):
        node = self

        class _ComponentShim:
            def grab_focus(self):
                node.grab_focus()

        return _ComponentShim()

    def grab_focus(self):
        try:
            return AXUIElementSetAttributeValue(self._e, kAXFocusedAttribute, True) == 0
        except Exception:
            return False

    def get_rect(self):
        pos = _copy_attr(self._e, "AXPosition")
        size = _copy_attr(self._e, "AXSize")
        try:
            return (int(pos.x), int(pos.y), int(size.width), int(size.height))
        except Exception:
            return (0, 0, 0, 0)

    def __repr__(self):
        try:
            return f"<AxNode '{self.get_name()}' role={self.get_role_name()}>"
        except Exception:
            return "<AxNode <?>>"


class AxBackend:
    platform = "ax"

    def __init__(self):
        self._pid = None
        import os
        self._pid = int(os.environ["JASP_PID"]) if os.environ.get("JASP_PID", "") else None

    def init(self):
        pass

    def _app_element(self):
        if self._pid is None:
            return None
        return AXUIElementCreateApplication(self._pid)

    # ── application discovery ────────────────────────────────────────

    def app_nodes(self):
        app = self._app_element()
        return [AxNode(app)] if app else []

    def find_jasp_app(self, timeout=30, main_window_names=None):
        if main_window_names is None:
            main_window_names = ("JASP",)
        app_elem = self._app_element()
        if not app_elem:
            return None, None
        app = AxNode(app_elem)
        deadline = time.time() + timeout
        while time.time() < deadline:
            win = _copy_attr(app_elem, kAXMainWindowAttribute)
            if win:
                node = AxNode(win)
                if node.get_name() in main_window_names:
                    return app, node
            time.sleep(1)
        return app, None

    def get_jasp_app(self):
        app_elem = self._app_element()
        return AxNode(app_elem) if app_elem else None

    def find_window_by_name(self, app, window_name, timeout=10, role_name=None):
        wl = window_name.lower()
        deadline = time.time() + timeout
        while time.time() < deadline:
            app_elem = app.raw if app is not None else self._app_element()
            if app_elem:
                windows = _copy_attr(app_elem, kAXWindowsAttribute) or []
                for w in windows:
                    node = AxNode(w)
                    if wl in node.get_name().lower():
                        return node
            time.sleep(0.5)
        return None

    def find_file_dialog(self, timeout=10):
        # Native macOS sheets belong to the app itself; AT-SPI-style foreign
        # dialog discovery does not apply. Return None (skip dialog flows).
        return None

    def grab_window_focus(self):
        app_elem = self._app_element()
        if not app_elem:
            return False
        win = _copy_attr(app_elem, kAXMainWindowAttribute)
        if win:
            return AXUIElementPerformAction(win, "AXRaise") == 0
        return False

    # ── keyboard synthesis ───────────────────────────────────────────
    # AX cannot synthesize global keyboard events; use Quartz CGEvent
    # (pyobjc) when needed. Kept minimal.

    def generate_key_event(self, keyval):
        try:
            import Quartz
            if 0x20 <= keyval < 0x7F:
                ch = chr(keyval).upper()
                code = Quartz.CGEventCreateKeyboardEvent(None, 0, True)
                Quartz.CGEventKeyboardSetUnicodeString(code, 1, ch)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, code)
                up = Quartz.CGEventCreateKeyboardEvent(None, 0, False)
                Quartz.CGEventKeyboardSetUnicodeString(up, 1, ch)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, up)
                return True
        except Exception:
            pass
        return False

    def ctrl_w(self):
        try:
            import Quartz
            for down in (True, False):
                ev = Quartz.CGEventCreateKeyboardEvent(None, 13, down)  # kVK_ANSI_W = 13
                if down:
                    Quartz.CGEventSetFlags(ev, Quartz.kCGEventFlagMaskCommand)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, ev)
        except Exception:
            pass

    def press_escape(self):
        return self.generate_key_event(0x1B)

    # ── node-level platform ops ──────────────────────────────────────

    def is_focused(self, node):
        v = _copy_attr(node.raw, kAXFocusedAttribute)
        return bool(v)

    def set_editable_text(self, node, text):
        try:
            ok = AXUIElementSetAttributeValue(node.raw, kAXValueAttribute, text)
            if ok == 0:
                return True
        except Exception:
            pass
        raise A11yError(f"set value failed on '{node.get_name()}'")

    def shutdown(self):
        pass
