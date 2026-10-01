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
    from CoreFoundation import kCFBooleanTrue
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
        # Do NOT use AXUIElementPerformAction here: on macOS 26 an AXPress
        # that opens one of JASP's popups can wedge the app's entire AX
        # bridge (kAXErrorNotImplemented forever, VoiceOver loses the app).
        # A synthesized click on the element performs the same UI action and
        # is immune. See Tests/qt-ax-repro/README.md.
        try:
            x, y, w, h = self.get_rect()
            if w <= 0 or h <= 0:
                return False
            import Quartz
            cx, cy = x + w / 2.0, y + h / 2.0
            move = Quartz.CGEventCreateMouseEvent(None, Quartz.kCGEventMouseMoved, (cx, cy), Quartz.kCGMouseButtonLeft)
            Quartz.CGEventPost(Quartz.kCGHIDEventTap, move)
            time.sleep(0.05)
            for down in (True, False):
                ev_type = Quartz.kCGEventLeftMouseDown if down else Quartz.kCGEventLeftMouseUp
                ev = Quartz.CGEventCreateMouseEvent(None, ev_type, (cx, cy), Quartz.kCGMouseButtonLeft)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, ev)
                time.sleep(0.05)
            return True
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
            from ApplicationServices import (
                AXValueGetValue,
                kAXValueCGPointType,
                kAXValueCGSizeType,
            )
            okp, p = AXValueGetValue(pos, kAXValueCGPointType, None)
            oks, s = AXValueGetValue(size, kAXValueCGSizeType, None)
            if okp and oks:
                return (int(p.x), int(p.y), int(s.width), int(s.height))
        except Exception:
            pass
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
        app = AXUIElementCreateApplication(self._pid)
        # Engage Qt's accessibility bridge the way VoiceOver does: without
        # this the app exposes an empty tree (just the window frame).
        for attr in ("AXManualAccessibility", "AXEnhancedUserInterface"):
            try:
                AXUIElementSetAttributeValue(app, attr, kCFBooleanTrue)
            except Exception:
                pass
        return app

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
        win = None
        while time.time() < deadline:
            w = _copy_attr(app_elem, kAXMainWindowAttribute)
            if w:
                name = (AxNode(w).get_name() or "").lower()
                if any(m.lower() in name for m in main_window_names):
                    win = w
                    break
            time.sleep(1)
        if win is None:
            return app, None
        # Engage the AX bridge the way an assistive client does: Qt builds
        # its QML tree lazily on the first attribute queries that reach the
        # QNSView (activateQtAccessibility). Querying the focused element
        # pokes that path; keep poking until children materialize.
        try:
            AXUIElementSetAttributeValue(app_elem, "AXManualAccessibility", kCFBooleanTrue)
        except Exception:
            pass
        deadline = time.time() + 10
        while time.time() < deadline:
            _copy_attr(app_elem, kAXFocusedUIElementAttribute)
            w = _copy_attr(app_elem, kAXMainWindowAttribute)
            if w:
                node = AxNode(w)
                if node.get_child_count() > 0:
                    return app, node
            time.sleep(0.5)
        return app, AxNode(win)

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
    # (pyobjc) when needed.

    # X-keysym (as used by the AT-SPI tests) -> macOS virtual key code
    _KEYSYM_TO_VK = {
        0xFF08: 51,  # BackSpace  kVK_Delete
        0xFF09: 48,  # Tab        kVK_Tab
        0xFF0D: 36,  # Return     kVK_Return
        0xFF1B: 53,  # Escape     kVK_Escape
        0xFF50: 115, # Home       kVK_Home
        0xFF51: 123, # Left       kVK_LeftArrow
        0xFF52: 126, # Up         kVK_UpArrow
        0xFF53: 124, # Right      kVK_RightArrow
        0xFF54: 125, # Down       kVK_DownArrow
        0xFF55: 116, # Prior      kVK_PageUp
        0xFF56: 121, # Next       kVK_PageDown
        0xFF57: 119, # End        kVK_End
        0xFFFF: 117, # Delete     kVK_ForwardDelete
        0xFFE1: 56,  # Shift_L    kVK_Shift
        0xFFE2: 60,  # Shift_R    kVK_RightShift
        0xFFE3: 59,  # Control_L  kVK_Control
        0xFFE4: 54,  # Control_R  kVK_RightControl
        0xFFE7: 63,  # Super_L    kVK_Function
        0xFFE9: 58,  # Alt_L      kVK_Option
        0xFFEA: 61,  # Alt_R      kVK_RightOption
    }

    def generate_key_event(self, keyval):
        try:
            import Quartz

            # X-keysym specials (Enter/Escape/arrows/modifiers...)
            vk = self._KEYSYM_TO_VK.get(keyval)
            if vk is not None:
                flags = 0
                if keyval in (0xFFE1, 0xFFE2):
                    flags = Quartz.kCGEventFlagMaskShift
                elif keyval in (0xFFE3, 0xFFE4):
                    flags = Quartz.kCGEventFlagMaskControl
                elif keyval in (0xFFE9, 0xFFEA):
                    flags = Quartz.kCGEventFlagMaskAlternate
                down = Quartz.CGEventCreateKeyboardEvent(None, vk, True)
                if flags:
                    Quartz.CGEventSetFlags(down, flags)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, down)
                up = Quartz.CGEventCreateKeyboardEvent(None, vk, False)
                if flags:
                    Quartz.CGEventSetFlags(up, flags)
                Quartz.CGEventPost(Quartz.kCGHIDEventTap, up)
                return True

            if 0x20 <= keyval < 0x7F or keyval == 0x1B:
                ch = chr(keyval)
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

    def is_checked(self, node):
        v = _copy_attr(node.raw, kAXValueAttribute)
        if isinstance(v, bool):
            return v
        if isinstance(v, (int, float)):
            return v != 0
        return None

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
