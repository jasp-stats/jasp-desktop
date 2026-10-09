import QtTest
import QtQuick
import JASP.Controls

// Per-form dataset/filter selection (VariablesForm::dataSetSelectionOption): every form that
// declares an option name owns a selection whose value is a FILTER ID, stored in that option of
// the analysis (so it reaches the engine identically in desktop and syntax mode) and served to
// the form's own controls through the form's own VariableInfo provider.
// The forms are independent: selecting in one never changes what another shows, and selection
// code NEVER touches the analysis' own filter (that global belongs to the FilterMenuButton).
// Fixture (see Tests/testqml.cpp): dataset 1 (shown: TestInts/TestLetters/TestDoubles/TestNominal)
// plus "Second" (TestInts with different values + SecondOnly), with default filter + one extra.
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			id:						formA
			dataSetSelectionOption:	"dataSetA"

			AvailableVariablesList	{ name: "allVarsA";	id: allVarsA }
			AssignedVariablesList	{ name: "dependentA";	id: dependentA; depends: "dataSetA" }
		}

		VariablesForm
		{
			id:						formB
			dataSetSelectionOption:	"dataSetB"

			AvailableVariablesList	{ name: "allVarsB";	id: allVarsB }
			AssignedVariablesList	{ name: "dependentB";	id: dependentB; depends: "dataSetB" }
		}

		// No dataSetSelectionOption: the FilterMenuButton world, untouched by any of this.
		VariablesForm
		{
			id:			noSelectionForm

			AvailableVariablesList	{ name: "allVarsC" }
			AssignedVariablesList	{ name: "targetC"; id: targetC }

			// Visually parked in the NON-selecting form, but drawing from formA's dependentA:
			// its namespace must follow the source (formA's selection), not the visual parent.
			// This is what JASPControl::sourceVarInfo() buys over the parentItem walk.
			AssignedVariablesList	{ name: "srcDriven"; id: srcDriven; source: dependentA }
		}

		// A TextArea is a host too (R/Lavaan/JAGS editors autocomplete against their own dataset):
		TextArea
		{
			id:						rCodeArea
			name:					"rCodeBox"
			dataSetSelectionOption:	"rCodeSet"
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestDataSelectingVariablesForm"

		SignalSpy
		{
			id:				spyLoader
			target:			jaspForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function _entryFor(labelStart)
		{
			for (var entry of formA.filterSelectionValues)
				if (entry.label.indexOf(labelStart) === 0)
					return entry
			return null
		}

		function _columnNames(listView)
		{
			var names = []
			for (var i = 0; i < listView.count; i++)
				names.push(listView.model.data(listView.model.index(i, 0)))
			return names
		}

		// Declaring a dataSetSelectionOption is what makes an analysis multi-dataset aware
		// (both modes; no separate dataSetSelection flag any more).
		function test_option_marks_analysis_aware()
		{
			verify(jaspForm.analysis.multiDataSetAware, "a form declaring dataSetSelectionOption implies multiDataSetAware")
		}

		// The offered values are filter ids with 'DataSet - Filter' labels, equal on both forms.
		function test_filter_values_offered_per_form()
		{
			var entries = formA.filterSelectionValues
			verify(entries.length >= 3, "fixture should offer >= 3 filters across the 2 datasets, got " + entries.length)

			for (var entry of entries)
			{
				var filterId = parseInt(entry.value)
				verify(!isNaN(filterId) && filterId >= 0, "a value must be a filter id, got: " + entry.value)
				verify(entry.label.indexOf(" - ") > 0, "label should read 'DataSet - Filter', got: " + entry.label)
			}

			compare(formB.filterSelectionValues.length, entries.length)
		}

		// Before any interaction every form defaults to the workspace's current filter, each
		// through its own selection (the dummy analysis holds no filter itself: -1).
		function test_defaults_to_analysis_filter()
		{
			verify(formA.selectedFilterId >= 0, "a selecting form defaults to the current filter")
			compare(formB.selectedFilterId, formA.selectedFilterId, "both forms start on the same current filter")
		}

		// The regression this whole file guards: two forms, two filters, at the same time.
		// (Linked-by-shared-state selections fail here, and always have.)
		function test_forms_select_independently()
		{
			var analysisFilterId = jaspForm.analysis.filterId	//the dummy analysis' own filter, must STAY what it is
			var secondEntry		 = _entryFor("Second - ")
			var firstEntry		 = null

			for (var entry of formA.filterSelectionValues)
				if (entry.label.indexOf("Second - ") !== 0)
				{
					firstEntry = entry
					break
				}

			verify(secondEntry !== null && firstEntry !== null, "fixture must offer a filter of each dataset")
			var secondFilterId = parseInt(secondEntry.value)
			var firstFilterId  = parseInt(firstEntry.value)
			verify(secondFilterId !== firstFilterId, "the two datasets must be different filters")

			formA.selectedFilterId = firstFilterId
			formB.selectedFilterId = secondFilterId
			wait(50)

			compare(formA.selectedFilterId, firstFilterId,  "form A keeps its own selection")
			compare(formB.selectedFilterId, secondFilterId, "form B keeps its own selection")
			compare(jaspForm.analysis.filterId, analysisFilterId, "selection must never touch the analysis' filter")

			// The selections travel as options: both present, side by side, holding the filter ids.
			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.dataSetA), String(firstFilterId))
			compare(String(options.dataSetB), String(secondFilterId))

			// And each form's data view follows its own selection, not the other's:
			var colsA = _columnNames(allVarsA)
			var colsB = _columnNames(allVarsB)

			verify(colsA.length > 0 && colsB.length > 0, "both lists should be populated: A " + colsA + ", B " + colsB)
			verify(colsB.indexOf("SecondOnly") !== -1, "form B must show its own dataset's column, got: " + colsB)
			verify(colsA.indexOf("SecondOnly") === -1, "form A must not show dataset 'Second's column, got: " + colsA)
			verify(colsA.indexOf("TestLetters") !== -1, "form A must still show dataset 1's columns, got: " + colsA)
			// Same name in both worlds, no cross-contamination (the 'compare a variable with itself' trap):
			verify(colsB.indexOf("TestInts") !== -1 && colsA.indexOf("TestInts") !== -1,
				   "'TestInts' exists in both datasets and must be listed by both forms")
		}

		// An id that resolves to no filter is rejected: selection (and option) stay as they were.
		function test_bogus_filter_id_rejected()
		{
			var before = formA.selectedFilterId
			formA.selectedFilterId = 987654
			wait(10)

			compare(formA.selectedFilterId, before, "a bogus filter id must be rejected, not adopted")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.dataSetA), String(before), "the option must not be rewritten by a rejected selection")
		}

		// A form without dataSetSelectionOption gets no selection at all (non-aware analyses,
		// the FilterMenuButton world): no dropdown, no option, selection reads as unselected.
		function test_form_without_option_has_no_selection()
		{
			compare(noSelectionForm.selectionAvailable, false)
			compare(noSelectionForm.dataSetSelectionOption, "")
			compare(noSelectionForm.selectedFilterId, -1, "a form without option holds no selection")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			verify(options.dataSetC === undefined, "a form without option may not write a selection option")
		}

		// T12: the same selection mechanism on a TextArea host - its own option, its own filter,
		// fully independent of the VariablesForms next to it and of the analysis filter.
		function test_textarea_host_selects_independently()
		{
			verify(rCodeArea.selectionAvailable, "declaring the option gives the text area a selection")
			verify(rCodeArea.selectedFilterId >= 0, "it defaults to the current filter")
			verify(rCodeArea.ownVarInfo !== null, "the host owns its VariableInfo")

			var secondFilterId = parseInt(_entryFor("Second - ").value)
			rCodeArea.selectedFilterId = secondFilterId
			wait(20)

			compare(rCodeArea.selectedFilterId, secondFilterId, "the text area holds its own selection")
			verify(formA.selectedFilterId !== secondFilterId, "form A is untouched by the text area's choice")
			verify(formB.selectedFilterId !== formA.selectedFilterId, "the forms still hold their own two-way split too")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.rCodeSet), String(secondFilterId), "its selection travels as its own option")
			compare(String(options.dataSetA), String(formA.selectedFilterId), "the other options stay put")
		}

		// source-first resolution: a list that declares `source` inherits the variable namespace of
		// the control it feeds from, even when visually parked under a different, non-selecting
		// form. The visual-parent (parentItem) chain is only the fallback for controls with no source.
		function test_source_drives_namespace_across_forms()
		{
			var original = formA.selectedFilterId

			// srcDriven mirrors formA's CURRENT selection, proving the namespace came through
			// source -> dependentA -> formA rather than the non-selecting visual parent.
			compare(srcDriven.resolvedSelectionFilterIdForTest(), formA.selectedFilterId,
					"a source-driven list resolves through its source (formA), not its visual parent")

			var secondFilterId = parseInt(_entryFor("Second - ").value)
			formA.selectedFilterId = secondFilterId
			wait(50)

			compare(srcDriven.resolvedSelectionFilterIdForTest(), secondFilterId,
					"changing formA's selection follows into the source-driven list")
			verify(targetC.resolvedSelectionFilterIdForTest() !== secondFilterId,
					"a sibling list WITHOUT a source stays on the non-selecting form's own namespace")

			formA.selectedFilterId = original	//don't bleed into the next test's assumptions
			wait(20)
		}
	}
}
