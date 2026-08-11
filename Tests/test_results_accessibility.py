#!/usr/bin/env python3
"""
Check JASP results window accessibility for Sleep.jasp Descriptives output.
Assumes JASP is already running with Sleep.jasp loaded.
"""

import time
import sys
from accessibility_common import (
    find_document_web, find_all, find_all_by_role,
    setup_jasp_app,
)


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

    print(f"  Document: '{doc.get_name()}', {doc.get_child_count()} children")

    doc_elements = find_all(doc)

    desc_stats_found = False
    boxplots_found = False
    tables = []
    sections = []
    images = []

    for role, name, child in doc_elements:
        nl = name.lower()
        if "descriptive statistics" in nl:
            desc_stats_found = True
        if "boxplots" in nl or "box plot" in nl:
            boxplots_found = True
        if "table" in role.lower() and name:
            tables.append((role, name))
        if "section" in role.lower() and name:
            sections.append((role, name))
        if "image" in role.lower() or "graphic" in role.lower():
            images.append((role, name))

    print(f"  Total document elements: {len(doc_elements)}")
    print(f"\n  Sections ({len(sections)}):")
    for r, n in sections:
        print(f"    {r}: \"{n}\"")

    print(f"\n  Tables ({len(tables)}):")
    for r, n in tables:
        print(f"    {r}: \"{n}\"")

    print(f"\n  Images/Graphics ({len(images)}):")
    for r, n in images:
        print(f"    {r}: \"{n}\"")

    print(f"\n{'='*60}")
    print("RESULTS:")
    print(f"  'Descriptive Statistics' found: {desc_stats_found}")
    print(f"  'BoxPlots' found:            {boxplots_found}")

    if desc_stats_found and boxplots_found:
        print("  PASS - Both expected result elements accessible!")
        return 0
    else:
        print("  FAIL - Some expected elements missing")
        return 1


if __name__ == "__main__":
    sys.exit(main())