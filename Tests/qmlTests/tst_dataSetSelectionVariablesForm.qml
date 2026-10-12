import QtTest
import QtQuick
import JASP.Controls

// Per-form dataset/filter selection (VariablesForm::dataSetSelectionOption): every form that
// declares an option name owns a selection whose value is a FILTER ID, stored in that option of
// the analysis (so it reaches the engine identically in desktop and syntax mode) and served to
// the form's own controls through the form's own VariableInfo provider.
// The forms are independent: selecting in one never changes what another shows, and selection
// code NEVER touches the analysis' own filter (that global belongs to the FilterMenuButton).
// Controls below a selecting host get their bindTo dependency on that host's FilterSelect derived
// from the varInfo resolution (JASPControl::selectionDependency), so a selection loaded from a file
// is in effect before anything that uses it stamps its .meta - no repair pass involved.
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

			// No depends: of its own: the dependency on this form's selection is derived from the
			// varInfo resolution (JASPControl::selectionDependency) and recorded by sortControls.
			AssignedVariablesList	{ name: "aDependent";	id: aDependent }

			// The hard case for the ordering: a plain bound option that has no dependencies what
			// so ever (an assigned list at least depends on its available one) and whose name sorts
			// before the selection option, so without the derived edge the sort puts IT first and
			// its .meta is stamped with whatever filter was current before the selection was bound.
			Switch					{ name: "aSwitch";	id: aSwitch }
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
			var formABefore		= formA.selectedFilterId
			rCodeArea.selectedFilterId = secondFilterId
			wait(20)

			compare(rCodeArea.selectedFilterId, secondFilterId, "the text area holds its own selection")
			compare(formA.selectedFilterId, formABefore, "form A is untouched by the text area's choice")
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

		// The ordering dependency on a dataset selection is never declared: it is derived from where
		// a control's variables actually resolve to (JASPControl::selectionDependency), which is the
		// only reason a host's FilterSelect is bound before everything it serves.
		function test_selection_dependency_is_derived()
		{
			verify(allVarsA.dependsOnForTest("dataSetA"),
					"an available list with no depends: of its own depends on its form's selection")
			verify(aDependent.dependsOnForTest("dataSetA"),
					"an assigned list with no depends: of its own depends on its form's selection")
			verify(aSwitch.dependsOnForTest("dataSetA"),
					"a plain option without any dependency of its own still depends on the selection it resolves through")
			verify(dependentA.dependsOnForTest("dataSetA"),
					"a list that declares depends: on its selection ends up with the same edge")
			verify(dependentB.dependsOnForTest("dataSetB"), "form B's list depends on B's selection")
			verify(!dependentA.dependsOnForTest("dataSetB"), "no cross-talk between the two forms")

			// the real witness that the edge comes from the resolution and not from the visual tree:
			// srcDriven sits in the NON-selecting form and declares no depends at all, yet its
			// variables come through source -> dependentA -> formA, so it waits for formA's selection
			verify(srcDriven.dependsOnForTest("dataSetA"),
					"a source-driven list depends on the selection its source resolves through")
			verify(!srcDriven.dependsOnForTest("dataSetB"), "...and on no other selection")

			// no host, no selection dependency (the analysis-wide VariableInfo is in place long
			// before any control binds, so there is nothing to wait for)
			verify(!targetC.dependsOnForTest("dataSetA") && !targetC.dependsOnForTest("dataSetB"),
					"a list in a form without selection depends on no selection at all")

			// a host is the owner of its selection, not a dependent of it
			verify(!formA.dependsOnForTest("dataSetA"), "the host itself is not its own dependency")
		}

		// The regression a provenance repair pass used to paper over: loading options whose selection
		// points at another dataset than the analysis filter is running on must stamp the UNCHANGED
		// values with that dataset from the very first bind. setOptions is the same path a .jasp
		// reload takes (store the loaded values, then setAnalysisUp rebinds every control).
		function test_saved_selection_stamps_values_on_first_bind()
		{
			var secondEntry = _entryFor("Second - ")
			var firstEntry  = null

			for (var entry of formA.filterSelectionValues)
				if (entry.label.indexOf("Second - ") !== 0)
				{
					firstEntry = entry
					break
				}

			verify(secondEntry !== null && firstEntry !== null, "fixture must offer a filter of each dataset")

			var savedFilterA = parseInt(secondEntry.value)
			var savedFilterB = parseInt(firstEntry.value)
			var originalA	 = formA.selectedFilterId
			var originalB	 = formB.selectedFilterId
			var bound		 = {}

			try
			{
				jaspForm.setOptions({
					dataSetA:	secondEntry.value,				//saved selection: dataset 'Second'
					dataSetB:	firstEntry.value,					//saved selection: the first dataset
					aSwitch:	true,								//no dependencies: only the derived edge orders it
					aDependent:	{ value: ["TestInts"], types: ["scale"] },	//unchanged from the file
					dependentA:	{ value: ["TestInts"], types: ["scale"] },
					dependentB:	{ value: ["TestInts"], types: ["scale"] }
				})
				wait(50)
				bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			} finally
			{
				//functions run alphabetically: put the selections back for the tests after this one
				formA.selectedFilterId = originalA
				formB.selectedFilterId = originalB
				wait(20)
			}

			var meta = bound[".meta"]

			compare(parseInt(String(bound.dataSetA)), savedFilterA, "form A took the loaded selection")
			verify(meta && meta.aSwitch && meta.aDependent && meta.dependentA && meta.dependentB,
					"the loaded options bound and stamped their meta")

			// "TestInts" lives in both datasets: only the stamp tells them apart, and a stamp taken
			// before dataSetA was bound would name the analysis' current filter instead.
			compare(meta.aSwitch.filterId, savedFilterA,
					"an option that sorts before its selection still gets the loaded selection's stamp")
			compare(meta.aDependent.filterId, savedFilterA,
					"a value whose name sorts before its selection option is stamped with that selection")
			compare(meta.dependentA.filterId, savedFilterA, "the one that declares depends: likewise")
			compare(meta.dependentB.filterId, savedFilterB, "form B's value is stamped with B's loaded filter")
			verify(meta.aSwitch.dataSetId !== meta.dependentB.dataSetId,
					"the two stamps name different datasets, not one analysis-wide stamp")
			verify(bound.aDependent.value.indexOf("TestInts") !== -1, "the value itself survived the bind")
		}
	}
}
