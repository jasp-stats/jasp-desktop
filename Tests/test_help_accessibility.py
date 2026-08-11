#!/usr/bin/env python3
"""
Test WebEngine accessibility by opening the Help window via AT-SPI navigation
and verifying content is exposed through the accessibility tree.
"""

import sys
import time
from accessibility_common import (
    find_by_name, find_by_desc,
    click_element, dump_tree, find_window_by_name,
    setup_jasp_app,
)


def main():
    app, main_window = setup_jasp_app(timeout=30)
    if not main_window:
        print("FATAL: Could not find JASP main window")
        sys.exit(1)
    print(f"Found main window: {main_window.get_child_count()} children")

    # Step 1: Click "Main menu" button
    print("Clicking 'Main menu' button...")
    hamburger_btn = find_by_name(main_window, "Main menu", "button", timeout=3)
    if not hamburger_btn:
        print("FATAL: 'Main menu' button not found")
        sys.exit(1)
    if not click_element(hamburger_btn):
        print("FATAL: Could not click 'Main menu'")
        sys.exit(1)
    time.sleep(2)

    # Step 2: Click "Preferences" button in FileMenu
    print("Looking for 'Preferences' button...")
    prefs_btn = find_by_name(app, "Preferences", "button", timeout=5)
    if not prefs_btn:
        print("FATAL: 'Preferences' button not found")
        sys.exit(1)
    if not click_element(prefs_btn):
        print("FATAL: Could not click 'Preferences'")
        sys.exit(1)
    time.sleep(2)

    # Step 3: Find Help button in Preferences page
    print("Looking for help info-button...")
    help_btn = find_by_name(app, "Help", "button", timeout=5)
    if not help_btn:
        print("  Trying by description...")
        help_btn = find_by_desc(app, "Show info about", timeout=3)
    if not help_btn:
        print("FATAL: Help button not found")
        sys.exit(1)
    print(f"  Found help button: {help_btn.get_name()!r}")
    if not click_element(help_btn):
        print("FATAL: Could not click help button")
        sys.exit(1)
    time.sleep(3)

    # Step 4: Find Help window frame
    print("Looking for Help window...")
    help_frame = find_window_by_name(None, "JASP Help", timeout=15)

    if not help_frame:
        print("FATAL: Help window not found")
        sys.exit(1)

    print(f"Help window found: {help_frame.get_child_count()} children")
    dump_tree(help_frame, max_depth=3)

    # Step 5: Verify document web
    doc = None
    for i in range(help_frame.get_child_count()):
        try:
            c = help_frame.get_child_at_index(i)
            if "document" in c.get_role_name().lower():
                doc = c
                break
        except Exception:
            pass

    if not doc:
        print("FAIL: No document web found in Help window")
        sys.exit(1)

    doc_root = doc
    if doc.get_child_count() == 1:
        try:
            inner = doc.get_child_at_index(0)
            if "document" in inner.get_role_name().lower():
                doc_root = inner
        except Exception:
            pass

    print(f"\nDocument web: name={doc_root.get_name()!r} role={doc_root.get_role_name()} children={doc_root.get_child_count()}")

    if doc_root.get_name() != "JASP Help":
        print(f"FAIL: Document web name is {doc_root.get_name()!r}, expected 'JASP Help'")
        sys.exit(1)

    if doc_root.get_child_count() == 0:
        print("FAIL: Document web has no accessible children")
        sys.exit(1)

    print("PASS: Document web has accessible children!")
    print("\nDocument web children:")
    dump_tree(doc_root, max_depth=2)

    text_found = False
    for i in range(doc_root.get_child_count()):
        try:
            c = doc_root.get_child_at_index(i)
            if "text" in c.get_role_name().lower() or "heading" in c.get_role_name().lower():
                text_found = True
                print(f"  Found text: {c.get_name()!r} role={c.get_role_name()}")
                break
        except Exception:
            pass

    if not text_found:
        print("FAIL: No text/heading elements in document web")
        sys.exit(1)

    print("\nALL CHECKS PASSED!")


if __name__ == "__main__":
    main()