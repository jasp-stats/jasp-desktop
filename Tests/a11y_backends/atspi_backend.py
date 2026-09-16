#!/usr/bin/env python3
"""
AT-SPI2 backend (Linux) — wraps the existing pyatspi/GI usage so that
accessibility_common.py behaves exactly as it always has on Linux.
"""

from a11y_backends import A11yError

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
    raise A11yError(f"PyGObject not available: {e}")


KEY_CONTROL_L = 0xFFE3


class AtspiBackend:
    platform = "atspi"

    def __init__(self):
        self._inited = False

    def init(self):
        if not self._inited:
            Atspi.init()
            self._inited = True

    # ── application discovery ────────────────────────────────────────

    def app_nodes(self):
        """All application root nodes on the AT-SPI desktop."""
        out = []
        try:
            desktop = Atspi.get_desktop(0)
            for i in range(desktop.get_child_count()):
                out.append(desktop.get_child_at_index(i))
        except Exception:
            pass
        return out

    def find_jasp_app(self, timeout=30, main_window_names=None):
        """Wait for JASP; return (app, main_window). Same semantics as before."""
        if main_window_names is None:
            main_window_names = ("JASP",)
        import time
        self.init()
        app = None
        main_window = None
        for attempt in range(timeout):
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

    def get_jasp_app(self):
        """Fresh best-guess JASP application node (most children wins)."""
        try:
            best = None
            best_cc = -1
            for a in self.app_nodes():
                if "jasp" in a.get_name().lower():
                    cc = a.get_child_count()
                    if cc > best_cc:
                        best_cc = cc
                        best = a
            return best
        except Exception:
            pass
        return None

    def find_window_by_name(self, app, window_name, timeout=10, role_name=None):
        """Find a frame/window child of app by name with retry. app=None scans all apps."""
        import time
        wl = window_name.lower()
        roles = (role_name,) if role_name else ("frame", "window")
        for _ in range(timeout * 2):
            apps_to_search = self.app_nodes() if app is None else [app]
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
            time.sleep(0.5)
        return None

    def find_file_dialog(self, timeout=10):
        """Find any non-JASP file dialog / frame on the desktop."""
        import time
        for _ in range(timeout * 2):
            try:
                for a in self.app_nodes():
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
            time.sleep(0.5)
        return None

    def grab_window_focus(self):
        """Grab focus for the first JASP frame window."""
        try:
            for a in self.app_nodes():
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

    # ── keyboard synthesis ───────────────────────────────────────────

    def generate_key_event(self, keyval):
        try:
            Atspi.generate_keyboard_event(keyval, None, Atspi.KeySynthType.SYM)
            return True
        except Exception:
            return False

    def ctrl_w(self):
        Atspi.generate_keyboard_event(KEY_CONTROL_L, None, Atspi.KeySynthType.PRESS)
        time.sleep(0.02)
        Atspi.generate_keyboard_event(ord('w'), None, Atspi.KeySynthType.PRESS)
        time.sleep(0.02)
        Atspi.generate_keyboard_event(ord('w'), None, Atspi.KeySynthType.RELEASE)
        time.sleep(0.02)
        Atspi.generate_keyboard_event(KEY_CONTROL_L, None, Atspi.KeySynthType.RELEASE)

    def press_escape(self):
        try:
            Atspi.generate_keyboard_event(0xFF1B, None, Atspi.KeySynthType.SYM)
            return True
        except Exception:
            return False

    # ── node-level platform ops ──────────────────────────────────────

    def is_focused(self, node):
        try:
            return node.get_state_set().contains(Atspi.StateType.FOCUSED)
        except Exception:
            return False

    def set_editable_text(self, node, text):
        try:
            ei = node.get_editable_text_iface()
            ei.set_text_contents(text)
            return True
        except Exception as e:
            raise A11yError(f"set_text_contents failed on '{node.get_name()}': {e}")

    def shutdown(self):
        pass
