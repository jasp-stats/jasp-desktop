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
// plus "Second" (TestInts with different values + SecondOnly), with default filter + extras; the
// workspace is then REOPENED from the database, so every test runs against a reopened file and
// the eager filter materialisation of DataSet::dbLoad (mds-plan C2/EAGER) is what puts the stored
// named filter "ReopenMe" in the dropdown.
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

			// A scalar under a selecting host, for the provenance-gate tests (mds-plan C3/M5):
			// it carries no columns, so its .meta must stay free of dataSetId/filterId.
			Switch					{ name: "bSwitch";	id: bSwitch }
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

		//The FilterSelect (a DropDown) a selecting host embeds behind its Loader: find it by the
		//option name it registered, so a test can read its error/awaiting state. hasError is the
		//JASPControl marker (plain Items expose objectName as `name`, not this property).
		function _selectControlFor(root, optionName)
		{
			for (var i = 0; i < root.children.length; i++)
			{
				const kid = root.children[i]
				if (kid.name === optionName && kid.hasError !== undefined)
					return kid
				const found = _selectControlFor(kid, optionName)
				if (found)
					return found
			}
			return null
		}

		// The phantom-slice class (agent-2 M3, mds-plan C3/M5): switching a host to ANOTHER
		// FILTER OF THE SAME DATASET rewrites no value at all - the encoded column names stay
		// perfectly valid - so provenance can only stay honest through the restamp walk that
		// HostFilterSelection::applyFilter does on every selection change. Column values must
		// follow the selection; scalars and the selection option itself carry no provenance, so
		// the abandoned filter cannot linger in .meta (and from there in the engine's slice
		// queue, referencedDataSets() or the RPC dataSetIds report).
		function test_midsession_switch_restamps_column_values()
		{
			var secondFilters = []

			for (var entry of formB.filterSelectionValues)
				if (entry.label.indexOf("Second - ") === 0)
					secondFilters.push(entry)

			verify(secondFilters.length >= 2, "fixture must offer two filters of dataset 'Second', got " + secondFilters.length)

			var f1 = parseInt(secondFilters[0].value)
			var f2 = parseInt(secondFilters[1].value)
			verify(f1 !== f2, "the two 'Second' filters must be different filters")

			var originalB = formB.selectedFilterId

			try
			{
				formB.selectedFilterId = f1
				wait(20)

				jaspForm.setOptions({
					dependentB:	{ value: ["TestInts"], types: ["scale"] },	//TestInts exists in 'Second' too
					bSwitch:	true
				})
				wait(50)

				var bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				var meta = bound[".meta"]

				verify(meta && meta.dependentB, "the column value bound and stamped its meta")
				compare(meta.dependentB.filterId, f1, "the column value is stamped with the live selection")
				verify(meta.dependentB.shouldEncode === true, "the gate keeps the generic .meta of an encoded value")

				//the actual mid-session switch: same dataset, other filter, no value change -
				formB.selectedFilterId = f2
				wait(50)

				bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				meta = bound[".meta"]

				compare(formB.selectedFilterId, f2, "the host took the new selection")
				verify(String(bound.dependentB.value).indexOf("TestInts") !== -1, "the value survived the switch untouched")
				compare(meta.dependentB.filterId, f2, "the column value followed the host onto the new filter")

				//and no stamp anywhere in the form still names the abandoned filter - that is
				//what the engine would queue as a phantom slice:
				var stale = []
				for (var key of Object.keys(meta))
					if (meta[key] && meta[key].filterId !== undefined && meta[key].filterId === f1)
						stale.push(key)
				compare(stale.join(","), "", "no stale filterId left in any .meta of the switched form")

				//scalars and the selection option carry no provenance at all (C3/M5 gate):
				verify(meta.bSwitch === undefined, "a scalar under the same host carries no stamp: " + JSON.stringify(meta.bSwitch))
				verify(meta.dataSetB === undefined, "the selection option carries no stamp: its value IS the filter id")
			} finally
			{
				formB.selectedFilterId = originalB	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// Declaring a dataSetSelectionOption is what makes an analysis multi-dataset aware
		// (both modes; no separate dataSetSelection flag any more).
		function test_option_marks_analysis_aware()
		{
			verify(jaspForm.analysis.multiDataSetAware, "a form declaring dataSetSelectionOption implies multiDataSetAware")
		}

		// The fixture reopened the database (Tests/testqml.cpp), so this whole file runs against
		// a REOPENED file. 'ReopenMe' was stored, never selected - it reaches the dropdown only
		// because DataSet::dbLoad materialises every stored filter at open (mds-plan C2/EAGER;
		// before that it stayed a db row and was unselectable - agent1 m4).
		function test_reopen_selection_survives_never_run_analysis()
		{
			var reopenEntry = _entryFor("Second - ReopenMe")
			verify(reopenEntry !== null, "'ReopenMe' (stored, never selected) must be offered right after the reopen")

			var savedFilterId = parseInt(reopenEntry.value)
			var originalA		 = formA.selectedFilterId
			var originalB		 = formB.selectedFilterId

			try
			{
				//The same bind path a .jasp load takes; the dummy analysis is NEVER run, so no
				//engine pass materialises anything here - exactly the disabled-analysis case of
				//finding M1, where the old coercion destroyed the saved selection for good.
				jaspForm.setOptions({ dataSetA: reopenEntry.value })
				wait(50)

				var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				compare(String(options.dataSetA), String(savedFilterId), "the saved selection must survive the bind intact")
				compare(formA.selectedFilterId, savedFilterId, "the host must hold the saved filter")

				var select = _selectControlFor(formA, "dataSetA")
				verify(select !== null, "the selection control must exist")
				verify(!select.hasError, "binding a reopened filter must not raise 'Unknown option'")
				verify(!select.valueAwaitsModel, "and nothing may be left pending once it bound")

				var colsA = _columnNames(allVarsA)
				verify(colsA.indexOf("SecondOnly") !== -1, "form A must show the saved selection's dataset, got: " + colsA)
			} finally
			{
				formA.selectedFilterId = originalA	//functions run alphabetically: don't bleed
				formB.selectedFilterId = originalB
				wait(20)
			}
		}

		// Every dataset×filter pair of the reopened file is in the model, including the named
		// ones - the aware dropdown, the RPC dataset_info list and the workspace dropdown all
		// iterate these same live filters.
		function test_reopened_named_filter_is_offered()
		{
			var entries = formA.filterSelectionValues
			var reopened = _entryFor("Second - ReopenMe")
			verify(reopened !== null, "the stored named filter must be listed without any priming")

			var ids = entries.map(function (e) { return String(e.value) })
			verify(ids.indexOf(String(reopened.value)) !== -1, "its value is the stored filter id")

			for (var entry of entries)
				verify(entry.label.indexOf(" - ") > 0, "every entry labels its dataset and filter, got: " + entry.label)
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

		// Agent-2's scratch probe (tst_reopenCoerce.qml) promoted: a selection value naming no
		// filter at all (id 987654: no live filter, no db row - the deleted/corrupted case eager
		// loading cannot rescue) must NOT be coerced to the first dropdown entry. The option keeps
		// the saved value while it waits for a model that can serve it, the host keeps its own
		// selection, and the control reports the problem instead of rewriting the file's content
		// behind the user's back (mds-plan C2/BIND). Last test alphabetically; restores anyway.
		function test_unknown_saved_filter_id_waits_not_coerced()
		{
			var unknownId = 987654

			//Park formA on a SECOND-dataset filter first, so its selection differs from the first
			//dropdown entry (dataset 1's default). Otherwise a coercion to entry 0 would land on
			//the very id the host already held and the bug would hide.
			var secondEntry = _entryFor("Second - ")
			verify(secondEntry !== null, "fixture must offer a second-dataset filter")
			var distinctId = parseInt(secondEntry.value)

			formA.selectedFilterId = distinctId
			wait(20)
			var before = formA.selectedFilterId
			var firstEntry = null
			for (var entry of formA.filterSelectionValues)
			{
				firstEntry = entry
				break
			}
			verify(parseInt(firstEntry.value) !== unknownId, "fixture sanity: first entry is not the unknown id")
			verify(parseInt(firstEntry.value) !== before, "host must start OFF entry 0 so coercion is visible")

			var select = _selectControlFor(formA, "dataSetA")
			verify(select !== null, "the selection control must exist")

			try
			{
				jaspForm.setOptions({ dataSetA: String(unknownId) })
				wait(50)

				var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				compare(String(options.dataSetA), String(unknownId),
						"an unknown saved selection must not be rewritten to the first entry's id")
				compare(formA.selectedFilterId, before,
						"the host must not be pushed to another filter by an unknown value")
				verify(select.valueAwaitsModel, "the value must stay pending until a model could serve it")
				verify(select.hasError, "the control must report the unknown option (error state)")
				compare(select.currentValue, "", "and show nothing rather than a coerced entry")
			} finally
			{
				//rebind through the combo itself: it is the side holding the pending value (the
				//host never moved), so a plain host reselect would leave the option at 987654.
				select.currentValue = String(before)
				wait(20)
			}

			var restored = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(restored.dataSetA), String(before), "the option is back on a live filter after the restore")
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
			verify(meta && meta.aDependent && meta.dependentA && meta.dependentB,
					"the loaded options bound and stamped their meta")

			// "TestInts" lives in both datasets: only the stamp tells them apart, and a stamp taken
			// before dataSetA was bound would name the analysis' current filter instead.
			compare(meta.aDependent.filterId, savedFilterA,
					"a value whose name sorts before its selection option is stamped with that selection")
			compare(meta.dependentA.filterId, savedFilterA, "the one that declares depends: likewise")
			compare(meta.dependentB.filterId, savedFilterB, "form B's value is stamped with B's loaded filter")
			verify(meta.dependentA.dataSetId !== meta.dependentB.dataSetId,
					"the two stamps name different datasets, not one analysis-wide stamp")
			verify(bound.aDependent.value.indexOf("TestInts") !== -1, "the value itself survived the bind")

			// mds-plan C3/M5: provenance belongs on column-bearing values ONLY. aSwitch (a plain
			// Switch) never carries columns, so it must not appear in .meta at all - and neither
			// does the selection option: its value already IS the filter id. A stamp here would
			// seed the engine's slice queue with a filter no value of this host uses (phantom
			// slice) and could never be refreshed when the host moves on.
			verify(meta.aSwitch === undefined,
					"a scalar carries no provenance stamp, got: " + JSON.stringify(meta.aSwitch))
			verify(meta.bSwitch === undefined,
					"form B's scalar carries no provenance stamp either, got: " + JSON.stringify(meta.bSwitch))
			verify(meta.dataSetA === undefined && meta.dataSetB === undefined,
					"selection options need no stamp: their value is the filter id itself")
		}
	}
}
