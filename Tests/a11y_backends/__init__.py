#!/usr/bin/env python3
"""
Platform backends for JASP accessibility tests.

Each backend exposes the same primitives that accessibility_common.py needs:

  platform                      "atspi" | "uia" | "ax"
  init()                        one-time backend setup
  app_nodes()                   list of application root nodes currently visible
  find_jasp_app(timeout, names) (app, main_window) with retries
  get_jasp_app()                fresh best-guess app node
  find_window_by_name(app, name, timeout, role_name)
  find_file_dialog(timeout)     a non-JASP/native file dialog node
  grab_window_focus()
  generate_key_event(keyval)    X-keysym style key synthesis
  is_focused(node)
  set_editable_text(node, text)
  shutdown()                    called at test end

Node objects are duck-typed to the AT-SPI API the tests already use:
  get_role_name(), get_name(), get_description(), get_child_count(),
  get_child_at_index(i), get_n_actions(), get_action_name(i), do_action(i),
  grab_focus(), get_component_iface() -> .grab_focus()

On Linux the backend returns raw AT-SPI objects, so existing tests behave
exactly as before. On Windows/macOS it returns wrapper nodes implementing
the same API over UIAutomation / the macOS AX API.
"""

import os
import sys

_BACKEND = None


def requested_backend():
    """Backend requested explicitly via env, or None for platform default."""
    return os.environ.get("JASP_A11Y_BACKEND", None) or None


def load_backend():
    """Load and return the backend instance (cached)."""
    global _BACKEND
    if _BACKEND is not None:
        return _BACKEND

    requested = requested_backend()
    platform = requested or {
        "win32": "uia",
        "darwin": "ax",
        "linux": "atspi",
    }.get(sys.platform, "atspi")

    if platform == "uia":
        from a11y_backends.uia_backend import UiaBackend
        _BACKEND = UiaBackend()
    elif platform == "ax":
        from a11y_backends.ax_backend import AxBackend
        _BACKEND = AxBackend()
    elif platform == "atspi":
        from a11y_backends.atspi_backend import AtspiBackend
        _BACKEND = AtspiBackend()
    else:
        raise RuntimeError(f"Unknown accessibility backend: {platform}")
    return _BACKEND


class A11yError(Exception):
    """Generic backend failure."""
    pass
