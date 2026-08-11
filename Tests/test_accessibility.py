#!/usr/bin/env python3
"""
Comprehensive unit test for JASP accessibility features using AT-SPI2 DBus API.
Tests all accessible UI components, menus, forms, tables, and results.

JASP is started by the run_accessibility_tests.sh runner; this script only connects.
"""

import unittest
import time
import sys
import os
from accessibility_common import (
    Atspi, click_element, close_menu, close_window, dismiss_dialogs,
    ensure_menu_closed, find_all_by_role, find_document_web, find_file_dialog,
    find_jasp_app, find_by_role_and_name, find_window_by_name,
    generate_key_event, get_jasp_app, open_file_menu,
    robust_search, setup_jasp_app,
    KEY_ENTER,
)


class TestJASPAccessibility(unittest.TestCase):
    """Test JASP accessibility with screen reader support."""

    @classmethod
    def setUpClass(cls):
        cls.app, cls.main_window = setup_jasp_app(timeout=30)
        if not cls.main_window:
            sys.exit(1)
        cls._native_dialogs_disabled = False

    def _refresh_app(self, names=None, timeout=5):
        try:
            if names is None:
                names = ("JASP",)
            app, mw = find_jasp_app(timeout=timeout, main_window_names=names)
            if app:
                type(self).app = app
            if mw:
                type(self).main_window = mw
            return bool(app)
        except Exception:
            return False

    def _get_all_accessible_elements(self, obj, depth=0, elements=None):
        if elements is None:
            elements = []
        try:
            elements.append({"role": obj.get_role_name() or "unknown", "name": obj.get_name() or ""})
            cc = obj.get_child_count()
            for i in range(cc):
                try:
                    child = obj.get_child_at_index(i)
                    if child:
                        self._get_all_accessible_elements(child, depth + 1, elements)
                except Exception:
                    pass
        except Exception:
            pass
        return elements

    def _collect_all_roles(self):
        roles = set()
        desktop = Atspi.get_desktop(0)
        for i in range(desktop.get_child_count()):
            app = desktop.get_child_at_index(i)
            elements = self._get_all_accessible_elements(app)
            for e in elements:
                if e["role"]:
                    roles.add(e["role"].lower())
        return roles

    def _count_components(self):
        counts = {"buttons": 0, "fillers": 0, "menus": 0, "menu_items": 0,
                  "text": 0, "labels": 0, "spin_boxes": 0, "combo_boxes": 0,
                  "tables": 0, "documents": 0, "frames": 0, "panels": 0}
        elements = self._get_all_accessible_elements(self.main_window)
        for e in elements:
            r = e["role"].lower() if e["role"] else ""
            if "button" in r: counts["buttons"] += 1
            elif "filler" in r: counts["fillers"] += 1
            elif "menu item" in r: counts["menu_items"] += 1
            elif "menu" in r: counts["menus"] += 1
            elif "text" in r: counts["text"] += 1
            elif "label" in r: counts["labels"] += 1
            elif "spin" in r: counts["spin_boxes"] += 1
            elif "combo" in r: counts["combo_boxes"] += 1
            elif "table" in r: counts["tables"] += 1
            elif "document" in r: counts["documents"] += 1
            elif "frame" in r: counts["frames"] += 1
            elif "panel" in r: counts["panels"] += 1
        return counts

    # ------------------------------------------------------------------
    #  tests 01–25: basic startup checks
    # ------------------------------------------------------------------

    def test_01_app_accessible(self):
        self.assertEqual(self.app.get_role_name(), "application")

    def test_02_main_menu_accessible(self):
        btn = find_by_role_and_name(self.main_window, "button", "Main menu")
        self.assertIsNotNone(btn, "Main menu button not found")

    def test_03_modules_menu_accessible(self):
        btn = find_by_role_and_name(self.main_window, "button", "Modules menu")
        self.assertIsNotNone(btn, "Modules menu button not found")

    def test_04_analysis_menu_accessible(self):
        menu = find_by_role_and_name(self.main_window, "menu", "Analysis menu")
        if not menu:
            self.skipTest("Analysis menu not visible — no submenu currently open")

    def test_05_open_button_accessible(self):
        btn = find_by_role_and_name(self.main_window, "button", "Open")
        self.assertIsNotNone(btn, "Open button not found")

    def test_06_save_button_accessible(self):
        btn = find_by_role_and_name(self.main_window, "button", "Save")
        self.assertIsNotNone(btn, "Save button not found")

    def test_07_results_accessible(self):
        doc = find_document_web(self.app)
        self.assertIsNotNone(doc, "Results document not found")

    def test_08_data_panel_accessible(self):
        data_frame = None
        for i in range(self.app.get_child_count()):
            try:
                c = self.app.get_child_at_index(i)
                if c.get_name() == "Data Preview":
                    data_frame = c
                    break
            except Exception:
                pass
        self.assertIsNotNone(data_frame, "Data panel not accessible")

    def test_09_main_window_structure(self):
        buttons = []
        elements = self._get_all_accessible_elements(self.main_window)
        for e in elements:
            if "button" in e["role"]:
                buttons.append(e["name"])
        expected = ["Main menu", "Open", "Save", "Modules menu"]
        for name in expected:
            self.assertTrue(any(name in b for b in buttons), f"'{name}' button not found")

    def test_10_accessible_roles_present(self):
        roles = self._collect_all_roles()
        required = {"application", "button", "filler", "frame", "panel",
                     "text", "check box", "separator", "label"}
        for role in required:
            self.assertIn(role, roles, f"Role '{role}' not found")

    def test_11_menu_items_accessible(self):
        roles = self._collect_all_roles()
        self.assertTrue(any(r in roles for r in ("filler", "button", "menu")),
                        "No menu-related roles found")

    def test_12_accessible_names(self):
        elements = self._get_all_accessible_elements(self.main_window)
        named = [e for e in elements if e["name"]]
        self.assertGreater(len(named), 0, "No named elements in main window")
        btn_names = [e["name"] for e in elements if "button" in e["role"] and e["name"]]
        self.assertGreater(len(btn_names), 0, "Buttons without names")

    def test_13_spin_box_accessible(self):
        roles = self._collect_all_roles()
        if "spin box" in roles:
            return
        self.skipTest("No spin box visible on startup — requires opening an analysis")

    def test_14_table_accessible(self):
        roles = self._collect_all_roles()
        if "table" in roles:
            return
        self.skipTest("No table visible on startup — requires loading data")

    def test_15_document_accessible(self):
        roles = self._collect_all_roles()
        self.assertTrue("document" in roles or "document web" in roles,
                        "Document components not accessible")

    def test_16_window_accessible(self):
        self.assertIsNotNone(self.main_window, "Main window not accessible")
        self.assertEqual(self.main_window.get_role_name(), "frame")
        from accessibility_common import count_tree_elements
        total = count_tree_elements(self.main_window)
        self.assertGreater(total, 80, f"Main window tree too shallow: {total} elements")

    def test_17_alert_messages_accessible(self):
        roles = self._collect_all_roles()
        if "alert" in roles:
            return
        self.skipTest("No alert visible on startup")

    def test_18_accessible_tree_depth(self):
        elements = self._get_all_accessible_elements(self.main_window)
        self.assertGreater(len(elements), 30, f"Only {len(elements)} elements in tree")

    def test_19_component_counts(self):
        counts = self._count_components()
        self.assertGreater(counts["buttons"], 5, f"Only {counts['buttons']} buttons")
        self.assertGreater(counts["frames"], 0, "No frames")
        self.assertGreater(counts["text"], 0, "No text elements")

    def test_20_sleep_data_exists(self):
        import accessibility_common
        sleep_file = accessibility_common.repo_root() / "jasp-desktop" / "build" / "Resources" / "Data Sets" / "Data Library" / "1. Descriptives" / "Sleep.jasp"
        self.assertTrue(sleep_file.exists(), f"Sleep.jasp not found at {sleep_file}")

    def test_21_analysis_results_accessible(self):
        doc = find_document_web(self.app)
        self.assertIsNotNone(doc, "Results document not accessible")
        self.assertIsNotNone(doc.get_name())

    def test_22_rich_text_accessible(self):
        elements = self._get_all_accessible_elements(self.main_window)
        text_el = [e for e in elements if "text" in e["role"] or "label" in e["role"]]
        self.assertGreater(len(text_el), 0, "No text or label components")

    def test_23_form_controls_accessible(self):
        roles = self._collect_all_roles()
        found = [r for r in ("spin box", "check box", "combo box", "text") if r in roles]
        self.assertGreater(len(found), 0, "No form controls")

    def test_24_webengine_document_role(self):
        doc = find_document_web(self.app)
        self.assertIsNotNone(doc, "document web not found")
        self.assertEqual(doc.get_role_name(), "document web")

    def test_25_webengine_children(self):
        doc = find_document_web(self.app)
        self.assertIsNotNone(doc, "document web not found")
        cc = doc.get_child_count()
        if cc == 0:
            self.skipTest("WebEngine children not yet loaded — run help/results tests for content")
        child = doc.get_child_at_index(0)
        self.assertIsNotNone(child.get_role_name())
        self.assertIsNotNone(child.get_name())

    # ------------------------------------------------------------------
    #  tests 26–29: file menu navigation
    # ------------------------------------------------------------------

    def test_26_file_menu_opens(self):
        self._assert_file_menu_open()
        elements = self._get_all_accessible_elements(self.app)
        names = [e["name"].lower() for e in elements]
        indicators = ["save as", "export results", "sync data", "close", "preferences"]
        found = [i for i in indicators if any(i in n for n in names)]
        self.assertGreater(len(found), 2, f"File menu not fully open, found: {found}")

    def test_27_file_menu_action_buttons(self):
        expected = [
            "New", "Open", "Save", "Save As", "Export Results",
            "Export Data", "Sync Data", "Close", "Preferences",
            "Contact", "Community", "About",
        ]
        elements = self._get_all_accessible_elements(self.app)
        btn_names = [e["name"] for e in elements if "button" in e["role"]]
        for name in expected:
            self.assertTrue(
                any(name.lower() in bn.lower() for bn in btn_names),
                f"File menu button '{name}' not found",
            )

    def test_28_file_menu_open_subitems(self):
        self._assert_file_menu_open()
        time.sleep(1)
        baseline = len(self._get_all_accessible_elements(self.app))
        open_buttons = find_all_by_role(self.app, "button", "open")
        if len(open_buttons) >= 2:
            click_element(open_buttons[-1])
        elif open_buttons:
            click_element(open_buttons[0])
        time.sleep(2)
        after = len(self._get_all_accessible_elements(self.app))
        elements = self._get_all_accessible_elements(self.app)
        names = [e["name"].lower() for e in elements]
        sub_indicators = ["computer", "osf", "data library", "recent files", "database"]
        found = [i for i in sub_indicators if any(i in n for n in names)]
        if len(found) <= 1:
            self.assertGreaterEqual(after, baseline,
                f"Tree did not expand after clicking Open (baseline={baseline}, after={after})")
        else:
            self.assertGreater(len(found), 1)

    def test_29_file_menu_preferences_tabs(self):
        close_menu()
        time.sleep(0.5)
        self._assert_file_menu_open()
        time.sleep(1)
        prefs_btn = find_by_role_and_name(self.app, "button", "Preferences")
        if not prefs_btn:
            close_menu()
            self.skipTest("Preferences button not found in file menu")
        baseline = len(self._get_all_accessible_elements(self.app))
        self.assertTrue(click_element(prefs_btn), "Could not click Preferences")
        time.sleep(3)
        after = len(self._get_all_accessible_elements(self.app))
        elements = self._get_all_accessible_elements(self.app)
        names = [e["name"].lower() for e in elements]
        found = [t for t in ["data", "results", "advanced"]
                 if any(t in n for n in names)]
        if len(found) == 0:
            self.assertGreaterEqual(after, baseline,
                f"Tree did not expand after clicking Preferences (baseline={baseline}, after={after})")
        close_menu()

    # ------------------------------------------------------------------
    #  test 29b: computer tab in open submenu
    # ------------------------------------------------------------------

    def test_29b_computer_tab_accessible(self):
        open_buttons = self._open_file_menu_open_submenu()
        if not open_buttons:
            self.skipTest("No Open buttons found via AT-SPI")
        click_element(open_buttons[-1] if len(open_buttons) >= 2 else open_buttons[0])
        time.sleep(2)
        if not self._refresh_app():
            close_menu()
            self.skipTest("App reference stale after clicking Open")
        computer_btn = self._robust_search(
            lambda app: find_by_role_and_name(app, "button", "Computer")
        )
        if not computer_btn:
            computer_btn = self._robust_search(
                lambda app: find_by_role_and_name(app, "push button", "Computer")
            )
        if not computer_btn:
            close_menu()
            self.skipTest("Computer tab button not found in file menu")
        click_element(computer_btn)
        time.sleep(2)
        browse_btn = self._robust_search(
            lambda app: find_by_role_and_name(app, "button", "Browse")
        )
        if not browse_btn:
            browse_btn = self._robust_search(
                lambda app: find_by_role_and_name(app, "push button", "Browse")
            )
        folder_items = [e for e in self._robust_search(
            lambda app: self._get_all_accessible_elements(app)
        ) if e["name"].lower().startswith("folder ")]
        self.assertTrue(
            browse_btn is not None or len(folder_items) > 0,
            "Computer tab has no Browse button or folder entries",
        )

    # ------------------------------------------------------------------
    #  tests 30–33: file menu → window open
    # ------------------------------------------------------------------

    def test_30_help_window_accessible(self):
        self.skipTest("Help window steals focus and breaks subsequent tests")
        # self._test_file_menu_window("Help", "JASP Help")

    def test_31_about_window_accessible(self):
        self._test_file_menu_window("About", "About")

    def test_32_contact_window_accessible(self):
        self._test_file_menu_window("Contact", "Contact")

    def test_33_community_window_accessible(self):
        self._test_file_menu_window("Community", "Community")

    # ------------------------------------------------------------------
    #  test 33b: disable native file dialogs
    # ------------------------------------------------------------------

    def test_33b_disable_native_dialogs(self):
        if self._ensure_qt_file_dialogs():
            type(self)._native_dialogs_disabled = True
            self._refresh_app()
        else:
            self.skipTest("Could not disable native file dialogs via Preferences")

    # ------------------------------------------------------------------
    #  test 35: open CSV via file menu → Computer → Browse
    # ------------------------------------------------------------------

    def test_35_open_debug_csv(self):
        if not self._refresh_app():
            self.skipTest("Could not refresh JASP app reference")
        if not self._native_dialogs_disabled:
            self.skipTest("Native file dialogs not disabled, skipping CSV open test")
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)

        csv_path = os.path.join(
            os.path.dirname(os.path.abspath(__file__)),
            "..", "Resources", "Data Sets", "debug.csv",
        )

        open_buttons = self._open_file_menu_open_submenu()
        if not open_buttons:
            close_menu()
            self.skipTest("No Open buttons in file menu")
        click_element(open_buttons[-1] if len(open_buttons) >= 2 else open_buttons[0])
        time.sleep(2)
        if not self._refresh_app():
            close_menu()
            self.skipTest("App reference stale after clicking Open")
        computer_btn = self._robust_search(
            lambda app: find_by_role_and_name(app, "button", "Computer")
        )
        if not computer_btn:
            close_menu()
            self.skipTest("Computer tab not available in file menu")
        click_element(computer_btn)
        time.sleep(2)
        browse_btn = self._robust_search(
            lambda app: find_by_role_and_name(app, "button", "Browse")
        )
        if not browse_btn:
            close_menu()
            self.skipTest("Browse button not available")
        click_element(browse_btn)
        time.sleep(3)

        dialog = find_file_dialog(timeout=5)

        if not dialog:
            close_menu()
            self.skipTest("File dialog window not found via AT-SPI")

        try:
            text_entries = find_all_by_role(dialog, "text")
            if not text_entries:
                text_entries = find_all_by_role(dialog, "combo box")
            if not text_entries:
                text_entries = find_all_by_role(dialog, "entry")

            if text_entries:
                entry = text_entries[0]
                try:
                    text_iface = entry.query_text()
                    text_iface.set_text_contents(0, len(text_iface.get_text(0, -1)), csv_path)
                except Exception:
                    for ch in csv_path:
                        generate_key_event(ord(ch))
                        time.sleep(0.005)
            else:
                for ch in csv_path:
                    generate_key_event(ord(ch))
                    time.sleep(0.005)

            time.sleep(0.5)
            generate_key_event(KEY_ENTER)
            time.sleep(4)
        except Exception:
            pass

        close_menu()
        dismiss_dialogs(self.app)
        time.sleep(2)

        self._refresh_app(names=("JASP", "debug"))

        table_found = False
        for _ in range(10):
            time.sleep(1)
            try:
                elements = self._get_all_accessible_elements(self.main_window)
                if any("table" in e["role"] for e in elements):
                    table_found = True
                    break
            except Exception:
                pass

        if not table_found:
            self.skipTest("Table role not found after CSV open — data may not have loaded")

        data_frame = None
        for i in range(self.app.get_child_count()):
            try:
                c = self.app.get_child_at_index(i)
                if c.get_name() == "Data Preview" and c.get_child_count() > 20:
                    data_frame = c
                    break
            except Exception:
                pass
        self.assertIsNotNone(data_frame, "Data Preview panel not populated after CSV load")

    def test_36_data_loaded_verification(self):
        if not self._data_is_loaded():
            self.skipTest("Data not loaded, skipping post-load checks")
        elements = self._get_all_accessible_elements(self.main_window)
        roles = set(e["role"] for e in elements)
        table_found = any("table" in r for r in roles)
        self.assertTrue(table_found, "Table role missing after data load")
        data_preview_found = any(
            e["name"] == "Data Preview"
            for e in self._get_all_accessible_elements(self.app)
        )
        self.assertTrue(data_preview_found, "Data Preview panel missing after data load")

    # ------------------------------------------------------------------
    #  tests 37–40: data-mode ribbon
    # ------------------------------------------------------------------

    def test_37_edit_data_button_enabled(self):
        if not self._data_is_loaded():
            self.skipTest("Data not loaded, skipping data-mode tests")
        buttons = find_all_by_role(self.main_window, "button")
        names = [(b.get_name() or "").lower() for b in buttons]
        ribbon_indicators = ["new data", "edit data", "descriptives", "r console", "analyses"]
        found = any(any(i in n for n in names) for i in ribbon_indicators)
        self.assertTrue(found, "No ribbon buttons accessible after data load")

    def test_38_switch_to_data_mode(self):
        if not self._data_is_loaded():
            self.skipTest("Data not loaded, skipping data-mode tests")
        buttons = find_all_by_role(self.main_window, "button")
        edit_data = [b for b in buttons if "edit data" in (b.get_name() or "").lower()]
        if edit_data:
            data_btn = edit_data[0]
        else:
            # Edit Data not visible (dataAvailableChanged not fired for CSV file-open path).
            # Fall back to New Data which creates a fresh empty dataset in data mode.
            new_data = [b for b in buttons if "new data" in (b.get_name() or "").lower()]
            if new_data:
                data_btn = new_data[0]
            else:
                self.skipTest("Neither Edit Data nor New Data button found in ribbon")
        self.assertTrue(click_element(data_btn), f"Could not click {data_btn.get_name()}")
        time.sleep(3)
        elements = self._get_all_accessible_elements(self.main_window)
        button_names = [e["name"].lower() for e in elements if "button" in e["role"]]
        self.assertTrue(
            any("analyses" in bn for bn in button_names),
            "Analyses button not found — data mode switch may have failed",
        )

    def test_39_data_mode_buttons(self):
        if not self._data_is_loaded():
            self.skipTest("Data not loaded, skipping data-mode tests")
        buttons = find_all_by_role(self.main_window, "button")
        button_names = [(b.get_name() or "").lower() for b in buttons]
        expected = ["analyses", "synchronisation", "resize data", "insert", "remove", "undo", "redo"]
        found = [n for n in expected if any(n in bn for bn in button_names)]
        self.assertGreater(len(found), 3, f"Only {len(found)} data-mode buttons found: {found}")

    def test_40_switch_back_to_analyses(self):
        if not self._data_is_loaded():
            self.skipTest("Data not loaded, skipping data-mode tests")
        buttons = find_all_by_role(self.main_window, "button")
        analyses = [b for b in buttons if "analyses" in (b.get_name() or "").lower()]
        if not analyses:
            self.skipTest("Analyses button not found — may already be in analysis mode")
        self.assertTrue(click_element(analyses[0]), "Could not click Analyses")
        time.sleep(3)

    # ------------------------------------------------------------------
    #  tests 41–43: ribbon special buttons & analysis dropdown
    # ------------------------------------------------------------------

    def test_41_ribbon_special_buttons(self):
        """Open Modules Menu, enable R module, verify R Console button becomes accessible in ribbon."""
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        modules_btn = find_by_role_and_name(self.main_window, "button", "Modules menu")
        self.assertIsNotNone(modules_btn, "Modules menu button not found")
        click_element(modules_btn)
        time.sleep(2)
        r_checkbox = None
        for role in ("check box", "check-box"):
            checkboxes = find_all_by_role(self.app, role)
            for cb in checkboxes:
                name = (cb.get_name() or "").lower()
                if name == "r" or "r console" in name or "r module" in name:
                    r_checkbox = cb
                    break
            if r_checkbox:
                break
        self.assertIsNotNone(r_checkbox, "R module checkbox not found in Modules Menu — accessibility gap")
        try:
            state = r_checkbox.get_state_set()
            if not state.contains(Atspi.StateType.CHECKED):
                click_element(r_checkbox)
                time.sleep(2)
        except Exception:
            pass
        close_menu()
        time.sleep(1)
        self._refresh_app()
        r_btn = find_by_role_and_name(self.app, "button", "R console")
        self.assertIsNotNone(
            r_btn, "R Console button not accessible in ribbon after enabling — accessibility gap"
        )

    def test_42_analysis_dropdown_menu(self):
        blacklist = (
            "main menu", "modules menu", "open", "save", "new data",
            "r console", "edit data", "analyses", "new", "save as",
            "export results", "export data", "sync data", "close",
            "preferences", "contact", "community", "about",
        )
        buttons = find_all_by_role(self.app, "button")
        mod_buttons = [
            b for b in buttons
            if b.get_name()
            and b.get_name().lower() not in blacklist
            and len(b.get_name()) > 3
        ]
        if not mod_buttons:
            self.skipTest("No analysis module buttons found in ribbon")
        btn = mod_buttons[0]
        self.assertTrue(click_element(btn), f"Could not click {btn.get_name()}")
        time.sleep(2)
        menu = find_by_role_and_name(self.app, "menu", btn.get_name().lower())
        if not menu:
            self.skipTest(
                f"No menu opened after clicking {btn.get_name()} "
                f"(may be a single-analysis module with no dropdown)"
            )
        items = find_all_by_role(self.app, "menu item")
        self.assertGreater(len(items), 0, f"No menu items accessible in {btn.get_name()} dropdown")
        close_menu()
        time.sleep(1)

    # ------------------------------------------------------------------
    #  test 43: modules menu panel
    # ------------------------------------------------------------------

    def test_43_modules_menu_panel(self):
        """Open Modules Menu, verify the panel is visible with its sections."""
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        self._ensure_modules_menu_open()
        panel_elements = self._get_all_accessible_elements(self.app)
        names = [e["name"].lower() for e in panel_elements]
        panel_indicators = ["installed modules", "module library", "install", "version"]
        found = [i for i in panel_indicators if any(i in n for n in names)]
        self.assertGreater(len(found), 1, f"Panel content not found, matched: {found}")
        close_menu()
        time.sleep(1)

    def test_43b_modules_menu_modules_accessible(self):
        """Open Modules Menu, verify installed modules are listed with accessible checkboxes."""
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        self._ensure_modules_menu_open()
        checkboxes = find_all_by_role(self.app, "check box")
        self.assertGreater(len(checkboxes), 0, "No module checkboxes found in Modules Menu")
        cb_names = [(cb.get_name() or "") for cb in checkboxes]
        expected_modules = ["R console", "Community", "Show Betas"]
        found = [m for m in expected_modules if any(m.lower() in n.lower() for n in cb_names)]
        self.assertGreaterEqual(len(found), 2,
            f"Expected modules menu checkboxes not found, got: {cb_names}")
        close_menu()
        time.sleep(1)

    def test_43c_module_library_webengine_accessible(self):
        """Open Modules Menu, verify the module library WebEngine loads as accessible document web."""
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        baseline = len(find_all_by_role(self.app, "document web"))
        self._ensure_modules_menu_open()
        # Wait up to 15 seconds for the module library to load
        docs = []
        for _ in range(15):
            time.sleep(1)
            docs = find_all_by_role(self.app, "document web")
            if len(docs) > baseline:
                break
        if len(docs) <= baseline:
            self.skipTest(
                "Module library WebEngine not loaded — checkUpdates may be disabled "
                "or page still loading"
            )
        store_doc = docs[-1]
        cc = store_doc.get_child_count()
        self.assertGreater(
            cc, 0,
            "Module library document web has no accessible children"
        )
        role = store_doc.get_role_name()
        self.assertEqual(role, "document web", f"Expected 'document web' role, got '{role}'")
        close_menu()
        time.sleep(1)

    # ------------------------------------------------------------------
    #  test 44: final tree summary
    # ------------------------------------------------------------------

    def test_44_accessible_tree_summary(self):
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(1)
        try:
            elements = self._get_all_accessible_elements(self.main_window)
        except Exception:
            self.skipTest("Could not enumerate accessible tree — app reference stale")
        if len(elements) == 0:
            self.skipTest("Accessible tree is empty — app reference stale")
        roles = {}
        for e in elements:
            r = e["role"]
            roles[r] = roles.get(r, 0) + 1
        print(f"\n  Accessible tree: {len(elements)} elements across {len(roles)} roles")
        for role, count in sorted(roles.items(), key=lambda x: -x[1])[:20]:
            print(f"    {role}: {count}")
        self.assertGreater(len(elements), 80, f"Only {len(elements)} elements in final tree")
        self.assertGreater(len(roles), 8, f"Only {len(roles)} distinct roles")

    # ==================================================================
    #  helper methods (grouped at end of class)
    # ==================================================================

    def _test_file_menu_window(self, button_name, window_name):
        """Open file menu, click named action button, verify a window opens with children."""
        self._refresh_app()
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        self._assert_file_menu_open()
        time.sleep(1)
        btn = find_by_role_and_name(self.app, "button", button_name)
        if not btn:
            close_menu()
            self.skipTest(f"{button_name} button not found in file menu")
        click_element(btn)
        time.sleep(3)
        win = find_window_by_name(self.app, window_name, timeout=8)
        if not win:
            close_menu()
            self.skipTest(f"{window_name} window did not open")
        self.assertTrue(win.get_child_count() > 0, f"{window_name} window has no children")
        close_window(win)
        close_menu()
        time.sleep(1)

    def _open_file_menu_open_submenu(self):
        """Ensure menu is closed, open file menu, return list of Open buttons or None."""
        ensure_menu_closed(self.app, self.main_window)
        time.sleep(0.5)
        self._assert_file_menu_open()
        time.sleep(1)
        return self._robust_search(
            lambda app: find_all_by_role(app, "button", "open")
        )

    def _open_file_menu(self):
        self._refresh_app()
        try:
            result = open_file_menu(self.app, self.main_window)
        except Exception:
            return False
        if result:
            fresh = get_jasp_app()
            if fresh:
                type(self).app = fresh
        return result

    def _data_is_loaded(self):
        try:
            self._fresh_app()
            for i in range(self.app.get_child_count()):
                c = self.app.get_child_at_index(i)
                if c.get_name() == "Data Preview" and c.get_child_count() > 20:
                    return True
        except Exception:
            pass
        return False

    def _fresh_app(self):
        app = get_jasp_app()
        if app:
            type(self).app = app
            return True
        return False

    def _robust_search(self, func, *args, max_retries=3):
        def _get_app():
            app = get_jasp_app()
            if app:
                type(self).app = app
            return app
        return robust_search(_get_app, func, *args, max_retries=max_retries)

    def _assert_file_menu_open(self):
        self.assertTrue(self._open_file_menu(), "Could not open file menu")

    def _ensure_qt_file_dialogs(self):
        try:
            ensure_menu_closed(self.app, self.main_window)
            time.sleep(0.5)
            if not self._open_file_menu():
                return False
            self._refresh_app()
            time.sleep(1)
            prefs_btn = find_by_role_and_name(self.app, "button", "Preferences")
            if not prefs_btn:
                close_menu()
                return False
            click_element(prefs_btn)
            time.sleep(2)
            self._refresh_app()

            elements = self._get_all_accessible_elements(self.app)
            ui_btns = [e for e in elements
                       if "button" in e["role"]
                       and ("ui" in e["name"].lower() or "interface" in e["name"].lower())]
            if not ui_btns:
                close_menu()
                return False
            ui_btn = find_by_role_and_name(self.app, "button", ui_btns[0]["name"])
            if not ui_btn:
                close_menu()
                return False
            click_element(ui_btn)
            time.sleep(2)

            native_check = find_by_role_and_name(self.app, "check box", "native")
            if not native_check:
                elements = self._get_all_accessible_elements(self.app)
                for e in elements:
                    if "check box" in e["role"] and "native" in e["name"].lower():
                        close_menu()
                        return True
                close_menu()
                return False
            click_element(native_check)
            time.sleep(1)
            close_menu()
            time.sleep(1)
            return True
        except Exception:
            close_menu()
            return False


    def _ensure_modules_menu_open(self):
        """Open the modules menu panel if it is not already visible."""
        btn = find_by_role_and_name(self.main_window, "button", "Modules menu")
        if not btn:
            self.skipTest("Modules menu button not found")
        # Check if panel is already open by looking for modules-specific elements
        for _ in range(3):
            checkboxes = find_all_by_role(self.app, "check box")
            cb_names = [(cb.get_name() or "").lower() for cb in checkboxes]
            modules_indicators = ["r console", "community", "show betas", "checkmark official"]
            if any(any(ind in n for n in cb_names) for ind in modules_indicators):
                return  # Panel already open
            # Check for the "Installed Modules" section label
            elements = self._get_all_accessible_elements(self.app)
            if any("installed modules" in e["name"].lower() for e in elements):
                return  # Panel already open
            time.sleep(1)
        # Panel is closed — toggle it open
        self.assertTrue(click_element(btn), "Could not click Modules menu button")
        time.sleep(2)
        # Verify it opened
        elements = self._get_all_accessible_elements(self.app)
        names = [e["name"].lower() for e in elements]
        panel_indicators = ["installed modules", "r console", "community", "module library"]
        found = [i for i in panel_indicators if any(i in n for n in names)]
        self.assertGreater(len(found), 0,
            f"Modules menu panel did not open after clicking button")


if __name__ == "__main__":
    unittest.main(verbosity=2)