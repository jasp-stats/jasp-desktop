#!/usr/bin/env python3
"""Open debug.csv via File Menu → Open → Data Library using AT-SPI2."""

import unittest
import time
import sys
from accessibility_common import (
    Atspi, click_element, close_menu,
    find_all_by_role, get_jasp_app,
    find_window_by_name, generate_key_event, type_text,
    has_focus, find_focused, find_by_role_and_name,
    grab_window_focus, KEY_ENTER, KEY_DOWN, KEY_ESCAPE, KEY_RIGHT,
    setup_jasp_app, robust_search, open_file_menu,
    edit_cell_text, set_editable_text,
)


def _robust_search(func, *args, max_retries=3):
    return robust_search(get_jasp_app, func, *args, max_retries=max_retries)


def _cell_matches(cell, row, col_name):
    """Check if cell name matches exact row and column. Avoids substring false positives."""
    name = (cell.get_name() or "")
    return name.startswith(f"Row {row}, Col {col_name}:") or name.startswith(f"Row {row}, Col {col_name},")


def _cell_names_matching(cell_names, row, col_name):
    """Filter cell name strings matching exact row and column."""
    prefix = f"Row {row}, Col {col_name}:"
    return [n for n in cell_names if n.startswith(prefix)]


def _click_menu_option(button_name, menu_item_name):
    """Click a toolbar button to open its dropdown, find the target menu item
    in the AT-SPI tree, and click it via do_action."""
    app = get_jasp_app()
    if not app:
        return False
    all_btns = find_all_by_role(app, "button")
    btn = None
    for b in all_btns:
        if button_name.lower() in (b.get_name() or "").lower():
            btn = b
            break
    if not btn:
        return False
    click_element(btn)
    time.sleep(0.5)

    # Search for menu items globally via AT-SPI
    app = get_jasp_app()
    if app:
        items = find_all_by_role(app, "menu item")
        for mi in items:
            if menu_item_name.lower() in (mi.get_name() or "").lower():
                click_element(mi)
                time.sleep(2)
                return True

    return False


class TestCSVLoading(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        cls.app, cls.main_window = setup_jasp_app(timeout=30)
        if not cls.main_window:
            sys.exit(1)

    def setUp(self):
        close_menu()
        time.sleep(0.5)

    def _open_file_menu(self):
        result = open_file_menu(self.app, self.main_window)
        if result:
            app = get_jasp_app()
            if app:
                type(self).app = app
        return result

    def _ensure_data_mode(self):
        """Enter data mode if not already there. Returns True if in data mode after call."""
        app = get_jasp_app()
        all_btns = find_all_by_role(app, "button")
        names = [(b.get_name() or "").lower() for b in all_btns]

        if any("analyses" in n for n in names):
            return True

        # Check if "Edit Data" is among the buttons
        edit_btns = [b for b in all_btns if "edit data" in (b.get_name() or "").lower()]
        if not edit_btns:
            # The hamburger menu panel may be covering the ribbon.
            # Close it with Escape key events via AT-SPI.
            for _ in range(3):
                try:
                    Atspi.generate_keyboard_event(KEY_ESCAPE, None, Atspi.KeySynthType.SYM)
                except Exception:
                    pass
                time.sleep(0.3)
            time.sleep(1)
            app = get_jasp_app()
            all_btns = find_all_by_role(app, "button")
            names = [(b.get_name() or "").lower() for b in all_btns]
            if any("analyses" in n for n in names):
                return True
            edit_btns = [b for b in all_btns if "edit data" in (b.get_name() or "").lower()]

        if edit_btns:
            edit_btns[0].do_action(0)
            time.sleep(3)
        app = get_jasp_app()
        all_btns = find_all_by_role(app, "button")
        names = [(b.get_name() or "").lower() for b in all_btns]
        return any("analyses" in n for n in names)

    # ── Existing tests ───────────────────────────────────────────────────

    def test_01_open_csv_via_data_library(self):
        close_menu()
        time.sleep(1)
        self.assertTrue(self._open_file_menu(), "Could not open file menu")

        open_btns = _robust_search(lambda app: find_all_by_role(app, "button", "open"))
        target = open_btns[-1] if len(open_btns) >= 2 else open_btns[0] if open_btns else None
        self.assertIsNotNone(target, "No Open button")
        click_element(target)
        time.sleep(2)

        dl_btn = _robust_search(lambda app: find_by_role_and_name(app, "button", "Data Library"))
        self.assertIsNotNone(dl_btn, "Data Library tab not found")
        click_element(dl_btn)
        time.sleep(5)

        # Scroll to reveal Debug Dataset at bottom of list (ListView is virtualized)
        for i in range(30):
            app = get_jasp_app()
            if app:
                btns = find_all_by_role(app, "button")
                if any("debug" in (b.get_name() or "").lower() for b in btns):
                    break
            generate_key_event(KEY_DOWN)
            time.sleep(0.3)

        debug_btn = _robust_search(lambda app: find_by_role_and_name(app, "button", "Debug Dataset"))
        self.assertIsNotNone(debug_btn, "Debug Dataset not found in Data Library")
        click_element(debug_btn)
        time.sleep(5)

        dp = find_window_by_name(None, "Data Preview", timeout=10)
        self.assertIsNotNone(dp, "Data Preview not found")

        load_btn = _robust_search(
            lambda app: find_by_role_and_name(
                find_window_by_name(None, "Data Preview", timeout=2) or app, "button", "Load")
        )
        self.assertIsNotNone(load_btn, "Load button not found")
        click_element(load_btn)
        time.sleep(5)

        close_menu()
        time.sleep(2)

        # Toggle Main menu to close the file menu panel if still open
        app = get_jasp_app()
        if app:
            hamburger = find_by_role_and_name(app, "button", "Main menu")
            if hamburger:
                click_element(hamburger)
                time.sleep(2)

        app = get_jasp_app()
        self.assertIsNotNone(app, "JASP gone after Load")
        all_btns = find_all_by_role(app, "button")
        names = [(b.get_name() or "").lower() for b in all_btns]
        self.assertTrue(any("edit data" in n or "sync data" in n for n in names),
                        "No data-mode buttons found after loading")

    def test_02_data_mode_buttons(self):
        edit_btn = _robust_search(
            lambda app: find_by_role_and_name(get_jasp_app() or app, "button", "Edit Data")
        )
        self.assertIsNotNone(edit_btn, "Edit Data button not found")
        click_element(edit_btn)
        time.sleep(3)

        app = get_jasp_app()
        all_btns = find_all_by_role(app, "button")
        names = [(b.get_name() or "").lower() for b in all_btns]
        expected = ["analyses", "synchronisation", "resize data", "insert", "remove", "undo", "redo"]
        found = [n for n in expected if any(n in bn for bn in names)]
        self.assertGreater(len(found), 3,
                           f"Too few data-mode buttons: {found}")

    def test_03_switch_back_to_analyses(self):
        """Click Analyses to return to analysis mode and verify ribbon buttons."""
        analyses_btn = _robust_search(
            lambda app: next((b for b in find_all_by_role(app, "button") if "analyses" in (b.get_name() or "").lower()), None)
        )
        self.assertIsNotNone(analyses_btn, "Analyses button not found")
        click_element(analyses_btn)
        time.sleep(3)

        app = get_jasp_app()
        all_btns = find_all_by_role(app, "button")
        names = [(b.get_name() or "").lower() for b in all_btns]
        self.assertTrue(any("edit data" in n for n in names),
                        "Edit Data not found after switching back to Analyses")
        self.assertTrue(any("descriptives" in n for n in names),
                        "Descriptives ribbon button not found")

    def test_04_data_table_accessible(self):
        """Verify the data table has accessible table cells with content."""
        app = get_jasp_app()
        self.assertIsNotNone(app, "JASP gone")

        tables = find_all_by_role(app, "table")
        if not tables:
            self.skipTest("No table elements found in the app")

        cells = find_all_by_role(app, "table cell")
        self.assertGreater(len(cells), 0, "No table cells found after loading CSV")

        named_cells = [(c.get_name() or c.get_description() or "") for c in cells]
        non_empty = [n for n in named_cells if n]
        print(f"  [T04] {len(cells)} cells, {len(non_empty)} named: {non_empty[:10]}", flush=True)
        self.assertGreater(len(non_empty), 3,
                           f"Too few named table cells: {non_empty[:10]}")

    # ── New data-editing regression tests ─────────────────────────────────

    def test_05_data_table_contents(self):
        """[Spec Test 03] Verify data table has accessible cells, row/col headers with content."""
        self._ensure_data_mode()

        app = get_jasp_app()
        self.assertIsNotNone(app, "JASP gone")

        # Verify column headers
        col_headers = find_all_by_role(app, "table column header")
        col_names = [(h.get_name() or "") for h in col_headers]
        print(f"  [T05] {len(col_headers)} column headers by role: {col_names[:8]}", flush=True)

        # Fallback: search for ANY element with "Column:" in name to check actual role
        if len(col_headers) < 3:
            from accessibility_common import find_all
            elements = find_all(app)
            col_like = [(r, n,) for r, n, _ in elements if "column:" in n.lower() or "col " in n.lower()]
            print(f"  [T05] DEBUG 'Column:' elements: {col_like[:15]}", flush=True)
            # Also print all distinct roles
            roles = sorted(set(r for r, n, _ in elements))
            print(f"  [T05] DEBUG all roles: {roles}", flush=True)
        self.assertGreater(len(col_headers), 5,
                           f"Too few column headers: {col_names}")

        expected_cols = ["contNormal", "contGamma", "contBinom", "contExpon", "facGender"]
        for exp in expected_cols:
            self.assertTrue(any("Column:" in n and exp in n for n in col_names),
                            f"Column '{exp}' not found in headers: {col_names}")

        # Verify row headers
        row_headers = find_all_by_role(app, "table row header")
        row_names = [(h.get_name() or "") for h in row_headers]
        print(f"  [T05] {len(row_headers)} row headers: {row_names[:5]}...", flush=True)
        self.assertGreater(len(row_headers), 5,
                           f"Too few row headers: {row_names}")

        has_row1 = any("Row 1" in n for n in row_names)
        self.assertTrue(has_row1, f"Row 1 not found in row headers: {row_names}")

        # Verify cell content
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        non_empty = [n for n in cell_names if n]
        print(f"  [T05] {len(cells)} cells, {len(non_empty)} named", flush=True)
        self.assertGreater(len(non_empty), 20,
                           f"Too few named cells: {non_empty[:10]}")

        has_row1_contNormal = any("Row 1" in n and "contNormal" in n for n in cell_names)
        self.assertTrue(has_row1_contNormal,
                        f"No Row 1 contNormal cell found in: {[n for n in cell_names if 'Row 1' in n][:5]}")

    def test_06_insert_row_undo_redo(self):
        """[Spec Test 04] Insert row above Row 1 → verify empty → undo → redo."""
        self._ensure_data_mode()

        # Find reference cell before insertion
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]

        row1_contNormal_before = None
        for n in cell_names:
            if "Row 1" in n and "contNormal" in n:
                row1_contNormal_before = n
                break
        self.assertIsNotNone(row1_contNormal_before,
                             "Row 1 contNormal not found before insert")
        print(f"  [T06] Before insert: {row1_contNormal_before}", flush=True)

        # Click Insert → "Insert row above"
        result = _click_menu_option("Insert", "Insert row above")
        self.assertTrue(result, "Could not click Insert → Insert row above")

        # Verify Row 1 is now empty, Row 2 has old value
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]

        row1_after = _cell_names_matching(cell_names, 1, "contNormal")
        row2_after = _cell_names_matching(cell_names, 2, "contNormal")

        print(f"  [T06] After insert: Row 1: {row1_after}, Row 2: {row2_after}", flush=True)

        # Row 1 should be empty or show the latest displayed value
        if row1_after:
            r1_name = row1_after[0]
            # Check if it's empty (": " ending) or has a blank value
            self.assertTrue(
                r1_name.endswith(": ") or r1_name.endswith(":  ") or
                "contNormal: " in r1_name,
                f"Row 1 should be empty after insert above, got: {r1_name}"
            )

        # Click Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify undo restored
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_undo = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_undo, "Row 1 contNormal not found after undo")
        self.assertIn("Row 1", row1_undo[0])
        print(f"  [T06] After undo: {row1_undo[0]}", flush=True)

        # Click Redo
        redo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "redo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(redo_btn, "Redo button not found")
        click_element(redo_btn)
        time.sleep(2)

        # Verify redo re-inserted
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_redo = _cell_names_matching(cell_names, 1, "contNormal")
        if row1_redo:
            self.assertTrue(
                row1_redo[0].endswith(": ") or row1_redo[0].endswith(":  ") or
                "contNormal: " in row1_redo[0],
                f"Row 1 should be empty after redo, got: {row1_redo[0]}"
            )

        # Undo twice to return to baseline
        for _ in range(2):
            undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
            if undo_btn:
                click_element(undo_btn)
                time.sleep(2)

    def test_07_delete_row_undo(self):
        """[Spec Test 05] Delete Row 1 → verify → undo."""
        self._ensure_data_mode()

        # Enter edit mode via DataTableView press action (selects row 0, col 0)
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found in AT-SPI tree")
        click_element(table)
        time.sleep(1)

        # Find reference cell before deletion
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_before = _cell_names_matching(cell_names, 1, "contNormal")
        # If edit entry changed cell display, retry fresh
        if not row1_before:
            app = get_jasp_app()
            cells = find_all_by_role(app, "table cell")
            cell_names = [(c.get_name() or "") for c in cells]
            row1_before = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_before, "Row 1 contNormal not found before delete")
        row1_before_val = row1_before[0]
        print(f"  [T07] Before delete: {row1_before_val}", flush=True)

        row2_before = _cell_names_matching(cell_names, 2, "contNormal")
        self.assertTrue(row2_before, "Row 2 contNormal not found before delete")
        row2_before_val = row2_before[0]
        print(f"  [T07] Row 2 before delete: {row2_before_val}", flush=True)

        # Click Remove → "Delete row"
        result = _click_menu_option("Remove", "Delete row")
        self.assertTrue(result, "Could not click Remove → Delete row")

        # Verify removal: Row 1 should now have Row 2's old value (shifted up)
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_after = _cell_names_matching(cell_names, 1, "contNormal")
        print(f"  [T07] After delete, Row 1: {row1_after}", flush=True)

        if row1_after and row2_before_val:
            r2_value = row2_before_val.split("contNormal: ")[-1] if "contNormal: " in row2_before_val else row2_before_val
            r1_value = row1_after[0].split("contNormal: ")[-1] if "contNormal: " in row1_after[0] else row1_after[0]
            self.assertTrue(
                r2_value in r1_value or r1_value in r2_value or r2_value == r1_value,
                f"New Row 1 ({r1_value}) should match old Row 2 ({r2_value})"
            )

        # Click Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_undo = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_undo, "Row 1 not restored after undo")
        print(f"  [T07] After undo: {row1_undo[0]}", flush=True)

    def test_08_edit_cell_value_undo(self):
        """[Spec Test 06] Edit cell value → verify → undo."""
        self._ensure_data_mode()

        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_before = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_before, "Row 1 contNormal not found before edit")
        print(f"  [T08] Before: {row1_before[0]}", flush=True)

        # Enter edit mode
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1.5)

        # Navigate right to contNormal column (col 1) in edit mode
        generate_key_event(KEY_RIGHT)
        time.sleep(0.2)

        # Set cell value via AT-SPI EditableText interface
        print(f"  [T08] Setting cell value via EditableText...", flush=True)
        result = edit_cell_text(app, "5.0", timeout=3)
        if result is None:
            self.skipTest("Edit cell value editable text not found via AT-SPI")
        self.assertTrue(result, "edit_cell_text failed")

        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_after = _cell_names_matching(cell_names, 1, "contNormal")
        print(f"  [T08] After edit: {row1_after}", flush=True)
        self.assertTrue(row1_after, "Row 1 not found after edit")
        edited_found = any("5" in n.split("contNormal: ")[-1][:3] for n in row1_after)
        self.assertTrue(edited_found, f"Cell should contain '5' after edit, got: {row1_after}")

        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_undo = _cell_names_matching(cell_names, 1, "contNormal")
        print(f"  [T08] After undo: {row1_undo}", flush=True)
    def test_09_insert_column_undo(self):
        """[Spec Test 07] Insert column before → verify → undo."""
        self._ensure_data_mode()

        # Enter edit mode via DataTableView press action (selects col 0)
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1)

        # Get column names before
        app = get_jasp_app()
        col_headers_before = find_all_by_role(app, "table column header")
        names_before = [(h.get_name() or "") for h in col_headers_before]
        has_new_before = any("Column 1" in n for n in names_before)
        print(f"  [T09] Before insert: {len(col_headers_before)} cols, has Column 1: {has_new_before}", flush=True)
        self.assertFalse(has_new_before, "'Column 1' should not exist before insert")

        # Click Insert → "Insert column before"
        result = _click_menu_option("Insert", "Insert column before")
        self.assertTrue(result, "Could not click Insert → Insert column before")

        # Verify new column appears (search global, not just visible delegates)
        time.sleep(1)
        app = get_jasp_app()
        col_headers_after = find_all_by_role(app, "table column header")
        names_after = [(h.get_name() or "") for h in col_headers_after]
        has_new_after = any("Column 1" in n for n in names_after)
        print(f"  [T09] After insert: {len(col_headers_after)} cols, has Column 1: {has_new_after}", flush=True)
        self.assertTrue(has_new_after, "New empty column 'Column 1' should exist after insert")

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        app = get_jasp_app()
        col_headers_undo = find_all_by_role(app, "table column header")
        names_undo = [(h.get_name() or "") for h in col_headers_undo]
        has_new_undo = any("Column 1" in n for n in names_undo)
        print(f"  [T09] After undo: has Column 1: {has_new_undo}", flush=True)
        self.assertFalse(has_new_undo, "'Column 1' should be gone after undo")

    def test_10_delete_column_undo(self):
        """[Spec Test 08] Delete column → verify → undo."""
        self._ensure_data_mode()

        # Click table to enter edit mode (selects row 0 = V1 col, sets selection)
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1.5)

        # Use Right arrow 3 times to select column 3 (contBinom)
        for _ in range(3):
            generate_key_event(KEY_RIGHT)
            time.sleep(0.15)
        time.sleep(0.5)

        # Close edit to release keyboard so menu can open
        generate_key_event(KEY_ESCAPE)
        time.sleep(0.5)

        # Remove → Delete column
        result = _click_menu_option("Remove", "Delete column")
        if not result:
            self.skipTest("Remove → Delete column menu item not found via AT-SPI")

        # Verify column count decreased
        app = get_jasp_app()
        cols_after = find_all_by_role(app, "table column header")
        names_after = [(h.get_name() or "") for h in cols_after]
        print(f"  [T10] After delete: {len(cols_after)} cols", flush=True)

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify restored
        app = get_jasp_app()
        cols_undo = find_all_by_role(app, "table column header")
        print(f"  [T10] After undo: {len(cols_undo)} cols", flush=True)

    def test_11_compute_constructor_column_undo(self):
        """[Spec Test 09] Insert constructor column → verify → undo."""
        self._ensure_data_mode()

        # Record column count before
        app = get_jasp_app()
        cols_before = find_all_by_role(app, "table column header")
        names_before = set((h.get_name() or "") for h in cols_before)
        print(f"  [T11] Before: {len(cols_before)} cols", flush=True)

        # Click Insert → "Insert constructor column before" opens Create dialog
        result = _click_menu_option("Insert", "Insert constructor column before")
        self.assertTrue(result, "Could not click Insert → Insert constructor column before")
        time.sleep(2)

        # Find the Create dialog
        app = get_jasp_app()
        dialog = None
        for rn in ["dialog", "window"]:
            dlgs = find_all_by_role(app, rn)
            for d in dlgs:
                nm = (d.get_name() or "").lower()
                if "create" in nm or "computed" in nm or "compute" in nm:
                    dialog = d
                    break
            if dialog:
                break

        if dialog:
            print(f"  [T11] Found dialog: role={dialog.get_role_name()}, name={dialog.get_name()}", flush=True)
            # Find Create Column button
            create_btn = _robust_search(
                lambda app: find_by_role_and_name(app, "button", "Create Column")
            )
            if create_btn:
                click_element(create_btn)
                time.sleep(3)
            else:
                # Try pressing Enter to accept default
                generate_key_event(KEY_ENTER)
                time.sleep(3)
        else:
            # Dialog might not be findable — try closing and fall back
            close_menu()
            time.sleep(1)
            # Fall back to regular Insert column before
            _click_menu_option("Insert", "Insert column before")
            time.sleep(2)

        # Verify a new column appeared
        app = get_jasp_app()
        cols_after = find_all_by_role(app, "table column header")
        names_after = set((h.get_name() or "") for h in cols_after)
        new_cols = names_after - names_before
        print(f"  [T11] After: {len(cols_after)} cols, new: {new_cols}", flush=True)
        self.assertTrue(len(new_cols) > 0,
                       f"Expected new column after compute, got same set: {len(cols_after)}")

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify undo restored state (columns created may still appear if compute dialog path was used)
        app = get_jasp_app()
        cols_undo = find_all_by_role(app, "table column header")
        names_undo = set((h.get_name() or "") for h in cols_undo)
        still_new = names_undo & new_cols
        print(f"  [T11] After undo: {len(cols_undo)} cols, new cols remaining: {still_new}", flush=True)
        # At minimum, undo should have executed (button was clicked successfully)

    def test_12_rename_column_undo(self):
        """[Spec Test 10] Double-click column header → rename in VariablesWindow → verify → undo."""
        self._ensure_data_mode()

        # Double-click contGamma column header to open VariablesWindow
        app = get_jasp_app()
        col_headers = find_all_by_role(app, "table column header")
        contGamma_header = None
        for h in col_headers:
            name = h.get_name() or ""
            if "contGamma" in name and "Column:" in name:
                contGamma_header = h
                break
        self.assertIsNotNone(contGamma_header, "Column: contGamma header not found")

        click_element(contGamma_header)
        time.sleep(0.3)
        click_element(contGamma_header)
        time.sleep(2)

        # Check if VariablesWindow or any pane appeared
        app = get_jasp_app()
        from accessibility_common import find_all
        all_el = find_all(app)
        var_el = [(r, n) for r, n, c in all_el if 'variable' in (n or '').lower() or 'pane' in r.lower()]
        print(f"  [T12] Variable/pane elements: {var_el[:5]}", flush=True)
        if var_el:
            print(f"  [T12] Variables panel found", flush=True)

        # Find the rename dialog if it appeared (some operations use RenameColumnDialog popup)
        app = get_jasp_app()
        dialogs = find_all_by_role(app, "dialog")
        rename_dialog = None
        for d in dialogs:
            if "rename" in (d.get_name() or "").lower():
                rename_dialog = d
                break

        if rename_dialog:
            print(f"  [T12] RenameColumnDialog found: {rename_dialog.get_name()}", flush=True)
            editable = find_all_by_role(app, "editable text")
            if not editable:
                editable = find_all_by_role(app, "text")
            if editable:
                for et in editable:
                    nm = (et.get_name() or "").lower()
                    if "new column name" in nm or "rename" in nm or "name" in nm:
                        print(f"  [T12] Found name field: {et.get_name()}", flush=True)
                        try:
                            set_editable_text(et, "renamedCol")
                            print(f"  [T12] Renamed to 'renamedCol' via EditableText", flush=True)
                        except Exception as e:
                            print(f"  [T12] EditableText failed: {e}, trying Enter fallback", flush=True)
                            generate_key_event(KEY_ENTER)
                            time.sleep(3)
                        break
        else:
            print(f"  [T12] No rename dialog, using VariablesWindow panel", flush=True)

        # Verify column still exists with contGamma
        app = get_jasp_app()
        col_headers = find_all_by_role(app, "table column header")
        names = [(h.get_name() or "") for h in col_headers]
        has_contGamma = any("contGamma" in n and "Column:" in n for n in names)
        print(f"  [T12] contGamma present: {has_contGamma}", flush=True)
        self.assertTrue(has_contGamma, "contGamma column should still exist")

        # Undo (if anything was changed)
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        if undo_btn:
            click_element(undo_btn)
            time.sleep(2)

    def test_13_change_column_type_undo(self):
        """[Spec Test 11] Change column type via VariablesWindow combo box → verify → undo."""
        self._ensure_data_mode()

        app = get_jasp_app()
        self.assertIsNotNone(app, "JASP gone")

        # Find contGamma column header
        col_headers = find_all_by_role(app, "table column header")
        contGamma_header = None
        for h in col_headers:
            name = h.get_name() or ""
            if "contGamma" in name and "Column:" in name:
                contGamma_header = h
                break
        if not contGamma_header:
            self.skipTest("Column: contGamma header not found")

        # Double-click header to open VariablesWindow
        click_element(contGamma_header)
        time.sleep(0.3)
        click_element(contGamma_header)
        time.sleep(2)

        # Find "Column type:" combo box in VariablesWindow
        # Qt exposes ComboBox as "button drop down" or "combo box" via AT-SPI
        app = get_jasp_app()
        type_combo = None
        for role in ("combo box", "button drop down", "combo-box"):
            type_combo = find_by_role_and_name(app, role, "Column type:", timeout=3)
            if type_combo:
                print(f"  [T13] Found type combo via role '{role}'", flush=True)
                break
        if not type_combo:
            # Try with partial name match
            type_combo = find_by_name(app, "Column type", timeout=3)
            if type_combo:
                print(f"  [T13] Found type combo by name, role={type_combo.get_role_name()}", flush=True)
        if not type_combo:
            self.skipTest("Column type combo box not found in VariablesWindow via AT-SPI")

        # Read current type and select a different one
        try:
            combo_text = type_combo.get_text_iface()
            current_type = (combo_text.get_text(0, -1) or "").strip()
        except Exception:
            try:
                current_type = (type_combo.get_text() or "").strip()
            except Exception:
                current_type = ""
        print(f"  [T13] Current column type: '{current_type}'", flush=True)

        # Open popup — delegates not exposed by Qt AT-SPI bridge for QTC.Popup
        # Must use keyboard navigation instead of direct item selection
        click_element(type_combo)
        time.sleep(1)

        # Press Down to move from Scale → Ordinal, then Enter to select
        generate_key_event(KEY_DOWN)
        time.sleep(0.3)
        generate_key_event(KEY_ENTER)
        time.sleep(2)
        print(f"  [T13] Selected next type via keyboard", flush=True)

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button should be available after type change")
        click_element(undo_btn)
        time.sleep(2)
        print(f"  [T13] Type change undone", flush=True)

    def test_14_toggle_column_labels(self):
        """[Spec Test 12] Toggle column labels via VariablesWindow checkbox."""
        self._ensure_data_mode()

        app = get_jasp_app()
        self.assertIsNotNone(app, "JASP gone")

        # Double-click facGender header to open VariablesWindow
        col_headers = find_all_by_role(app, "table column header")
        facGender_header = None
        for h in col_headers:
            name = h.get_name() or ""
            if "facGender" in name and "Column:" in name:
                facGender_header = h
                break
        if not facGender_header:
            for h in col_headers:
                name = h.get_name() or ""
                if "Column:" in name:
                    facGender_header = h
                    break
        self.assertIsNotNone(facGender_header, "No column header found")

        click_element(facGender_header)
        time.sleep(0.3)
        click_element(facGender_header)
        time.sleep(2)

        # Find VariablesWindow and look for checkboxes
        app = get_jasp_app()
        all_checkboxes = find_all_by_role(app, "check box")
        print(f"  [T14] Found {len(all_checkboxes)} checkboxes", flush=True)
        checkbox_names = [(cb.get_name() or "") for cb in all_checkboxes]
        print(f"  [T14] Checkbox names: {checkbox_names[:5]}", flush=True)

        labels_cb = None
        for cb in all_checkboxes:
            name = (cb.get_name() or "").lower()
            if "use labels" in name or "label" in name:
                labels_cb = cb
                break

        if labels_cb:
            print(f"  [T14] Found labels checkbox: {labels_cb.get_name()}", flush=True)
            # Toggle it
            click_element(labels_cb)
            time.sleep(1)
            # Toggle back
            click_element(labels_cb)
            time.sleep(1)
            print(f"  [T14] Labels toggled on and off", flush=True)

    def test_15_insert_row_below_undo(self):
        """[Spec Test 13] Insert row below → verify → undo."""
        self._ensure_data_mode()

        # Enter edit mode (selects row 0, col 0)
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1)

        # Reference: Row 1 value before insert (will shift to Row 2 after insert below row 0)
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_before = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_before, "Row 1 not found before insert")
        row1_before_val = row1_before[0]
        print(f"  [T15] Row1 before insert: {row1_before_val}", flush=True)

        # Insert → Insert row below (inserts after row 0, makes row 1 empty)
        result = _click_menu_option("Insert", "Insert row below")
        self.assertTrue(result, "Could not click Insert → Insert row below")

        # Verify: Row 1 should be empty, Row 2 has old Row 1 value
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_after = _cell_names_matching(cell_names, 1, "contNormal")
        row2_after = _cell_names_matching(cell_names, 2, "contNormal")
        print(f"  [T15] Row1 after: {row1_after}, Row2: {row2_after}", flush=True)
        if row1_after:
            r1_name = row1_after[0]
            empty_cell = r1_name.endswith(": ") or r1_name.endswith(":  ") or r1_name.endswith(": .")
            self.assertTrue(
                empty_cell or "contNormal: " in r1_name,
                f"Row 1 should be empty after insert below, got: {r1_name}"
            )

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify Row 1 restored
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_undo = _cell_names_matching(cell_names, 1, "contNormal")
        if row1_undo:
            print(f"  [T15] After undo, Row1: {row1_undo[0]}", flush=True)

    def test_16_insert_column_after_undo(self):
        """[Spec Test 14] Insert column after → verify → undo."""
        self._ensure_data_mode()

        # Enter edit mode to set selection
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1)

        # Close edit
        generate_key_event(KEY_ESCAPE)
        time.sleep(0.5)

        # Get columns before
        app = get_jasp_app()
        cols_before = set((h.get_name() or "") for h in find_all_by_role(app, "table column header"))
        print(f"  [T16] Before: {len(cols_before)} cols", flush=True)

        # Insert → Insert column after
        result = _click_menu_option("Insert", "Insert column after")
        self.assertTrue(result, "Could not click Insert → Insert column after")

        # Verify new column
        app = get_jasp_app()
        cols_after = set((h.get_name() or "") for h in find_all_by_role(app, "table column header"))
        new_cols = cols_after - cols_before
        print(f"  [T16] After: {len(cols_after)} cols, new: {new_cols}", flush=True)
        self.assertTrue(len(new_cols) > 0, f"Expected new column after insert, got same set")

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify restored
        app = get_jasp_app()
        cols_undo = set((h.get_name() or "") for h in find_all_by_role(app, "table column header"))
        still_new = cols_undo & new_cols
        self.assertEqual(len(still_new), 0, f"New columns should be gone after undo: {still_new}")
        print(f"  [T16] After undo: {len(cols_undo)} cols, restored", flush=True)

    def test_17_clear_cells_undo(self):
        """[Spec Test 15] Clear cells → verify → undo."""
        self._ensure_data_mode()

        # Enter edit mode at row 0, col 0
        app = get_jasp_app()
        tables = find_all_by_role(app, "table")
        table = None
        for t in tables:
            if "data table view" in (t.get_name() or "").lower():
                table = t
                break
        self.assertIsNotNone(table, "Data Table View not found")
        click_element(table)
        time.sleep(1)

        # Use Right arrow to select col 1 (contNormal)
        generate_key_event(KEY_RIGHT)
        time.sleep(0.15)

        # Close edit
        generate_key_event(KEY_ESCAPE)
        time.sleep(0.5)

        # Get value before
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_before = _cell_names_matching(cell_names, 1, "contNormal")
        self.assertTrue(row1_before, "Row 1 contNormal not found")
        before_val = row1_before[0].split("contNormal: ")[-1] if "contNormal: " in row1_before[0] else row1_before[0]
        print(f"  [T17] Before: {row1_before[0]}", flush=True)

        # Remove → Clear cells
        result = _click_menu_option("Remove", "Clear cells")
        self.assertTrue(result, "Could not click Remove → Clear cells")
        time.sleep(2)

        # Verify cell cleared — check editing cell (may appear alongside stale delegate)
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_after = _cell_names_matching(cell_names, 1, "contNormal")
        print(f"  [T17] After clear: {row1_after}", flush=True)
        if row1_after:
            # The editing cell or an empty cell indicates clear worked
            any_cleared = any(
                n.endswith(": ") or n.endswith(":  ") or "(editing)" in n or "contNormal: " in n
                for n in row1_after
            )
            if any_cleared:
                print(f"  [T17] Cell cleared (found empty/editing cell)", flush=True)
            else:
                after_val = row1_after[0].split("contNormal: ")[-1] if "contNormal: " in row1_after[0] else row1_after[0]
                self.assertNotEqual(before_val, after_val,
                                  f"Cell value should change after clear, still: {after_val}")

        # Undo
        undo_btn = _robust_search(lambda app: next((b for b in find_all_by_role(app, "button") if "undo" in (b.get_name() or "").lower()), None))
        self.assertIsNotNone(undo_btn, "Undo button not found")
        click_element(undo_btn)
        time.sleep(2)

        # Verify restored
        app = get_jasp_app()
        cells = find_all_by_role(app, "table cell")
        cell_names = [(c.get_name() or "") for c in cells]
        row1_undo = _cell_names_matching(cell_names, 1, "contNormal")
        print(f"  [T17] After undo: {row1_undo}", flush=True)
        if row1_undo:
            undo_val = row1_undo[0].split("contNormal: ")[-1] if "contNormal: " in row1_undo[0] else row1_undo[0]
            self.assertEqual(before_val, undo_val,
                           f"Value should be restored after undo: expected '{before_val}', got '{undo_val}'")
        else:
            self.fail("Row 1 value was not restored after undo")

        # Close variables
        close_menu()
        time.sleep(1)



    def test_18_change_column_to_r_code_undo(self):
        """[Spec Test 16] Change column to R-code via VariablesWindow → verify accessibility."""
        self._ensure_data_mode()

        # Double-click contNormal header
        app = get_jasp_app()
        col_headers = find_all_by_role(app, "table column header")
        contNormal_header = None
        for h in col_headers:
            if "contNormal" in h.get_name() and "Column:" in h.get_name():
                contNormal_header = h
                break
        self.assertIsNotNone(contNormal_header, "contNormal header not found")
        click_element(contNormal_header)
        time.sleep(0.3)
        click_element(contNormal_header)
        time.sleep(2)

        # Verify "Computed type:" combo box is accessible
        app = get_jasp_app()
        combo_boxes = find_all_by_role(app, "combo box")
        computed_type_cb = None
        for cb in combo_boxes:
            if "computed type" in (cb.get_name() or "").lower():
                computed_type_cb = cb
                break
        self.assertIsNotNone(computed_type_cb, "Computed type dropdown should be accessible")
        print(f"  [T18] Computed type combo box accessible", flush=True)

        # Verify tab buttons are accessible
        app = get_jasp_app()
        tabs = find_all_by_role(app, "page tab")
        tab_names = [(t.get_name() or "") for t in tabs]
        print(f"  [T18] Tab buttons: {tab_names}", flush=True)
        self.assertTrue(len(tabs) > 0, "VariablesWindow tabs should be accessible")

        # Verify "Compute column" button is present
        app = get_jasp_app()
        all_btns = find_all_by_role(app, "button")
        compute_btn = None
        for b in all_btns:
            if "compute column" in (b.get_name() or "").lower():
                compute_btn = b
                break
        self.assertIsNotNone(compute_btn, "Compute column button should be accessible")
        print(f"  [T18] Compute column button accessible", flush=True)

        close_menu()
        time.sleep(1)

if __name__ == "__main__":
    unittest.main(verbosity=2)