import QtTest
import QtQuick
import JASP.Controls

// Per-LIST dataset/filter selection (VariablesListBase::dataSetSelectionOption, mds-plan C10/D2.2):
// every list that declares an option name owns a selection whose value is a FILTER ID, stored in
// that option of the analysis (so it reaches the engine identically in desktop and syntax mode)
// and served to its own controls through the list's own VariableInfo provider. A plain assigned
// list inherits the namespace of the available list its items come from (VariablesListBase::
// sourceVarInfo), so selecting on the available list drives the whole section - while any other
// list can hold a different selection at the same time. TextAreaBase keeps a selection of its own
// exactly like before.
// The hosts are independent: selecting in one never changes what another shows, and selection
// code NEVER touches the analysis' own filter (that global belongs to the FilterMenuButton).
// Controls below a selecting host get their bindTo dependency on that host's FilterSelect derived
// from the varInfo resolution (JASPControl::selectionDependency), so a selection loaded from a
// file is in effect before anything that uses it stamps its .meta - no repair pass involved.
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
			id:			formA

			// The selection lives on the available list (mds-plan C10): it drives allVarsA itself
			// and, through the inherited available-list namespace, the assigned lists below it.
			AvailableVariablesList	{ id: allVarsA;	name: "allVarsA";	dataSetSelectionOption:	"dataSetA" }
			AssignedVariablesList	{ id: dependentA;	name: "dependentA";	depends: "dataSetA" }

			// No depends: of its own: the dependency on this section's selection is derived from
			// the varInfo resolution (JASPControl::selectionDependency) and recorded by
			// sortControls - the hard case for the ordering, and it must still be stamped by the
			// selection loaded from a file before its value binds.
			AssignedVariablesList	{ id: aDependent;	name: "aDependent" }

			// A plain option with no columns: since the selection moved onto the lists it
			// resolves through nothing (the form is not a host any more), so it neither depends
			// on a selection nor carries a provenance stamp.
			Switch					{ id: aSwitch;		name: "aSwitch" }
		}

		VariablesForm
		{
			id:			formB

			AvailableVariablesList	{ id: allVarsB;	name: "allVarsB";	dataSetSelectionOption:	"dataSetB" }
			AssignedVariablesList	{ id: dependentB;	name: "dependentB";	depends: "dataSetB" }

			// A scalar under a NON-selecting form, for the provenance-gate tests (mds-plan
			// C3/M5): it carries no columns, so its .meta must stay free of dataSetId/filterId.
			Switch					{ id: bSwitch;		name: "bSwitch" }
		}

		// No dataSetSelectionOption anywhere: the FilterMenuButton world, untouched by any of this.
		VariablesForm
		{
			id:			noSelectionForm

			AvailableVariablesList	{ id: allVarsC;	name: "allVarsC" }
			AssignedVariablesList	{ id: targetC;		name: "targetC" }

			// Visually parked in the NON-selecting form, but drawing from formA's dependentA:
			// its namespace must follow the source (allVarsA's selection), not the visual parent.
			// This is what JASPControl::sourceVarInfo() buys over the parentItem walk.
			AssignedVariablesList	{ id: srcDriven;	name: "srcDriven";	source: dependentA }
		}

		// A TextArea is a host too (R/Lavaan/JAGS editors autocomplete against their own dataset):
		TextArea
		{
			id:						rCodeArea
			name:					"rCodeBox"
			dataSetSelectionOption:	"rCodeSet"
		}
	}

	// A host instantiated only during the registry test, mimicking a ComponentsList row (created
	// after form completion; the row's creation context resolves `form`, exactly what a plain
	// createObject from this document gets too via the root's `form` alias).
	Component
	{
		id:			lateHostComponent
		TextArea
		{
			name:					"lateRCode"
			dataSetSelectionOption:	"lateSet"
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestDataSelectingVariablesList"

		SignalSpy
		{
			id:				spyLoader
			target:			jaspForm
			signalName:		"formCompletedSignal"
		}

		SignalSpy
		{
			id:				spySyntax
			target:			jaspForm
			signalName:		"rSyntaxTextChanged"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function _entryFor(labelStart)
		{
			for (var entry of allVarsA.filterSelectionValues)
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

		//The FilterSelect (a DropDown) a selecting list embeds in its chrome: find it by the
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

			for (var entry of allVarsB.filterSelectionValues)
				if (entry.label.indexOf("Second - ") === 0)
					secondFilters.push(entry)

			verify(secondFilters.length >= 2, "fixture must offer two filters of dataset 'Second', got " + secondFilters.length)

			var f1 = parseInt(secondFilters[0].value)
			var f2 = parseInt(secondFilters[1].value)
			verify(f1 !== f2, "the two 'Second' filters must be different filters")

			var originalB = allVarsB.selectedFilterId

			try
			{
				allVarsB.selectedFilterId = f1
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

				//and the assigned list that resolves through this host is stamped likewise:
				//(dependentB declares depends: on the option; its namespace comes from allVarsB)

				//the actual mid-session switch: same dataset, other filter, no value change -
				allVarsB.selectedFilterId = f2
				wait(50)

				bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				meta = bound[".meta"]

				compare(allVarsB.selectedFilterId, f2, "the host took the new selection")
				verify(String(bound.dependentB.value).indexOf("TestInts") !== -1, "the value survived the switch untouched")
				compare(meta.dependentB.filterId, f2, "the column value followed the host onto the new filter")

				//and no stamp anywhere in the analysis still names the abandoned filter - that is
				//what the engine would queue as a phantom slice:
				var stale = []
				for (var key of Object.keys(meta))
					if (meta[key] && meta[key].filterId !== undefined && meta[key].filterId === f1)
						stale.push(key)
				compare(stale.join(","), "", "no stale filterId left in any .meta after the switched list restamped its controls")

				//scalars and the selection option carry no provenance at all (C3/M5 gate):
				verify(meta.bSwitch === undefined, "a scalar carries no stamp: " + JSON.stringify(meta.bSwitch))
				verify(meta.dataSetB === undefined, "the selection option carries no stamp: its value IS the filter id")
			} finally
			{
				allVarsB.selectedFilterId = originalB	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// Declaring a dataSetSelectionOption (on a list) is what makes an analysis multi-dataset
		// aware (both modes; no separate dataSetSelection flag any more).
		function test_option_marks_analysis_aware()
		{
			verify(jaspForm.analysis.multiDataSetAware, "a list declaring dataSetSelectionOption implies multiDataSetAware")
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
			var originalA		= allVarsA.selectedFilterId
			var originalB		= allVarsB.selectedFilterId

			try
			{
				//The same bind path a .jasp load takes; the dummy analysis is NEVER run, so no
				//engine pass materialises anything here - exactly the disabled-analysis case of
				//finding M1, where the old coercion destroyed the saved selection for good.
				jaspForm.setOptions({ dataSetA: reopenEntry.value })
				wait(50)

				var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				compare(String(options.dataSetA), String(savedFilterId), "the saved selection must survive the bind intact")
				compare(allVarsA.selectedFilterId, savedFilterId, "the host list must hold the saved filter")

				var select = _selectControlFor(allVarsA, "dataSetA")
				verify(select !== null, "the selection control must live in the list's chrome")
				verify(!select.hasError, "binding a reopened filter must not raise 'Unknown option'")
				verify(!select.valueAwaitsModel, "and nothing may be left pending once it bound")

				var colsA = _columnNames(allVarsA)
				verify(colsA.indexOf("SecondOnly") !== -1, "the selecting list must show its saved selection's dataset, got: " + colsA)
			} finally
			{
				allVarsA.selectedFilterId = originalA	//functions run alphabetically: don't bleed
				allVarsB.selectedFilterId = originalB
				wait(20)
			}
		}

		// Every dataset×filter pair of the reopened file is in the model, including the named
		// ones - the aware dropdown, the RPC dataset_info list and the workspace dropdown all
		// iterate these same live filters.
		function test_reopened_named_filter_is_offered()
		{
			var entries = allVarsA.filterSelectionValues
			var reopened = _entryFor("Second - ReopenMe")
			verify(reopened !== null, "the stored named filter must be listed without any priming")

			var ids = entries.map(function (e) { return String(e.value) })
			verify(ids.indexOf(String(reopened.value)) !== -1, "its value is the stored filter id")

			for (var entry of entries)
				verify(entry.label.indexOf(" - ") > 0, "every entry labels its dataset and filter, got: " + entry.label)
		}

		// The offered values are filter ids with 'DataSet - Filter' labels, equal on both lists.
		function test_filter_values_offered_per_list()
		{
			var entries = allVarsA.filterSelectionValues
			verify(entries.length >= 3, "fixture should offer >= 3 filters across the 2 datasets, got " + entries.length)

			for (var entry of entries)
			{
				var filterId = parseInt(entry.value)
				verify(!isNaN(filterId) && filterId >= 0, "a value must be a filter id, got: " + entry.value)
				verify(entry.label.indexOf(" - ") > 0, "label should read 'DataSet - Filter', got: " + entry.label)
			}

			compare(allVarsB.filterSelectionValues.length, entries.length)
		}

		// Before any interaction every selecting list defaults to the workspace's current filter,
		// each through its own selection (the dummy analysis holds no filter itself: -1).
		function test_defaults_to_analysis_filter()
		{
			verify(allVarsA.selectedFilterId >= 0, "a selecting list defaults to the current filter")
			compare(allVarsB.selectedFilterId, allVarsA.selectedFilterId, "both lists start on the same current filter")
		}

		// The regression this whole file guards: two lists, two filters, at the same time, in one
		// analysis. (Linked-by-shared-state selections fail here, and always have.)
		function test_lists_select_independently()
		{
			var analysisFilterId = jaspForm.analysis.filterId	//the dummy analysis' own filter, must STAY what it is
			var secondEntry		 = _entryFor("Second - ")
			var firstEntry		 = null

			for (var entry of allVarsA.filterSelectionValues)
				if (entry.label.indexOf("Second - ") !== 0)
				{
					firstEntry = entry
					break
				}

			verify(secondEntry !== null && firstEntry !== null, "fixture must offer a filter of each dataset")
			var secondFilterId = parseInt(secondEntry.value)
			var firstFilterId  = parseInt(firstEntry.value)
			verify(secondFilterId !== firstFilterId, "the two datasets must be different filters")

			allVarsA.selectedFilterId = firstFilterId
			allVarsB.selectedFilterId = secondFilterId
			wait(50)

			compare(allVarsA.selectedFilterId, firstFilterId,  "list A keeps its own selection")
			compare(allVarsB.selectedFilterId, secondFilterId, "list B keeps its own selection")
			compare(jaspForm.analysis.filterId, analysisFilterId, "selection must never touch the analysis' filter")

			// The selections travel as options: both present, side by side, holding the filter ids.
			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.dataSetA), String(firstFilterId))
			compare(String(options.dataSetB), String(secondFilterId))

			// And each list's data view follows its own selection, not the other's:
			var colsA = _columnNames(allVarsA)
			var colsB = _columnNames(allVarsB)

			verify(colsA.length > 0 && colsB.length > 0, "both lists should be populated: A " + colsA + ", B " + colsB)
			verify(colsB.indexOf("SecondOnly") !== -1, "list B must show its own dataset's column, got: " + colsB)
			verify(colsA.indexOf("SecondOnly") === -1, "list A must not show dataset 'Second's column, got: " + colsA)
			verify(colsA.indexOf("TestLetters") !== -1, "list A must still show dataset 1's columns, got: " + colsA)
			// Same name in both worlds, no cross-contamination (the 'compare a variable with itself' trap):
			verify(colsB.indexOf("TestInts") !== -1 && colsA.indexOf("TestInts") !== -1,
				   "'TestInts' exists in both datasets and must be listed by both selecting lists")
		}

		// An id that resolves to no filter is rejected: selection (and option) stay as they were.
		function test_bogus_filter_id_rejected()
		{
			var before = allVarsA.selectedFilterId
			allVarsA.selectedFilterId = 987654
			wait(10)

			compare(allVarsA.selectedFilterId, before, "a bogus filter id must be rejected, not adopted")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.dataSetA), String(before), "the option must not be rewritten by a rejected selection")
		}

		// A list without dataSetSelectionOption gets no selection at all (non-aware analyses, the
		// FilterMenuButton world): no dropdown, no option, selection reads as unselected. And the
		// form exposes NO selection API any more (mds-plan C10: stripped hard, no shims):
		function test_list_without_option_has_no_selection()
		{
			compare(allVarsC.selectionAvailable, false)
			compare(allVarsC.dataSetSelectionOption, "")
			compare(allVarsC.selectedFilterId, -1, "a list without option holds no selection")

			verify(formA.dataSetSelectionOption === undefined, "VariablesForm has no selection API any more")
			verify(formA.selectedFilterId === undefined, "not even read-only: the property is gone")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			verify(options.dataSetC === undefined, "a list without option may not write a selection option")
		}

		// T12: the same selection mechanism on a TextArea host - its own option, its own filter,
		// fully independent of the selecting lists next to it and of the analysis filter.
		function test_textarea_host_selects_independently()
		{
			verify(rCodeArea.selectionAvailable, "declaring the option gives the text area a selection")
			verify(rCodeArea.selectedFilterId >= 0, "it defaults to the current filter")
			verify(rCodeArea.ownVarInfo !== null, "the host owns its VariableInfo")

			var secondFilterId = parseInt(_entryFor("Second - ").value)
			var listABefore		= allVarsA.selectedFilterId
			rCodeArea.selectedFilterId = secondFilterId
			wait(20)

			compare(rCodeArea.selectedFilterId, secondFilterId, "the text area holds its own selection")
			compare(allVarsA.selectedFilterId, listABefore, "list A is untouched by the text area's choice")
			verify(allVarsB.selectedFilterId !== allVarsA.selectedFilterId, "the lists still hold their own two-way split too")

			var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(options.rCodeSet), String(secondFilterId), "its selection travels as its own option")
			compare(String(options.dataSetA), String(allVarsA.selectedFilterId), "the other options stay put")
		}

		// Agent-2's scratch probe (tst_reopenCoerce.qml) promoted: a selection value naming no
		// filter at all (id 987654: no live filter, no db row - the deleted/corrupted case eager
		// loading cannot rescue) must NOT be coerced to the first dropdown entry. The option keeps
		// the saved value while it waits for a model that can serve it, the host keeps its own
		// selection, and the control reports the problem instead of rewriting the file's content
		// behind the user's back (mds-plan C2/BIND). Restores anyway.
		function test_unknown_saved_filter_id_waits_not_coerced()
		{
			var unknownId = 987654

			//Park list A on a SECOND-dataset filter first, so its selection differs from the first
			//dropdown entry (dataset 1's default). Otherwise a coercion to entry 0 would land on
			//the very id the host already held and the bug would hide.
			var secondEntry = _entryFor("Second - ")
			verify(secondEntry !== null, "fixture must offer a second-dataset filter")
			var distinctId = parseInt(secondEntry.value)

			allVarsA.selectedFilterId = distinctId
			wait(20)
			var before = allVarsA.selectedFilterId
			var firstEntry = null
			for (var entry of allVarsA.filterSelectionValues)
			{
				firstEntry = entry
				break
			}
			verify(parseInt(firstEntry.value) !== unknownId, "fixture sanity: first entry is not the unknown id")
			verify(parseInt(firstEntry.value) !== before, "host must start OFF entry 0 so coercion is visible")

			var select = _selectControlFor(allVarsA, "dataSetA")
			verify(select !== null, "the selection control must exist")

			try
			{
				jaspForm.setOptions({ dataSetA: String(unknownId) })
				wait(50)

				var options = JSON.parse(jaspForm.analysis.boundValuesAsJson())
				compare(String(options.dataSetA), String(unknownId),
						"an unknown saved selection must not be rewritten to the first entry's id")
				compare(allVarsA.selectedFilterId, before,
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

		// source-first resolution: a list that declares `source` inherits the variable namespace
		// of the control it feeds from, even when visually parked under a different, non-selecting
		// form. The visual-parent (parentItem) chain is only the fallback for controls with no
		// source and no related selecting list.
		function test_source_drives_namespace_across_forms()
		{
			var original = allVarsA.selectedFilterId

			// srcDriven mirrors allVarsA's CURRENT selection, proving the namespace came through
			// source -> dependentA -> (related available) allVarsA rather than the non-selecting
			// visual parent.
			compare(srcDriven.resolvedSelectionFilterIdForTest(), allVarsA.selectedFilterId,
					"a source-driven list resolves through its source chain (allVarsA), not its visual parent")

			var secondFilterId = parseInt(_entryFor("Second - ").value)
			allVarsA.selectedFilterId = secondFilterId
			wait(50)

			compare(srcDriven.resolvedSelectionFilterIdForTest(), secondFilterId,
					"changing the host list's selection follows into the source-driven list")
			verify(targetC.resolvedSelectionFilterIdForTest() !== secondFilterId,
					"a sibling list WITHOUT a source (and whose available list selects nothing) stays off the host's namespace")

			allVarsA.selectedFilterId = original	//don't bleed into the next test's assumptions
			wait(20)
		}

		// The ordering dependency on a dataset selection is never declared: it is derived from
		// where a control's variables actually resolve to (JASPControl::selectionDependency),
		// which is the only reason each list's FilterSelect is bound before everything it serves.
		function test_selection_dependency_is_derived()
		{
			// aDependent declares no depends at all: its edge to dataSetA falls out of resolving
			// through its related available list (allVarsA, the host):
			verify(aDependent.dependsOnForTest("dataSetA"),
					"an assigned list inherits the ordering edge to the selection of the list its items come from")
			verify(dependentA.dependsOnForTest("dataSetA"),
					"a list that declares depends: on its selection ends up with the same edge")
			verify(dependentB.dependsOnForTest("dataSetB"), "form B's list depends on B's selection")
			verify(!dependentA.dependsOnForTest("dataSetB"), "no cross-talk between the two sections")

			// the witness that the edge comes from the resolution and not from the visual tree:
			// srcDriven sits in the NON-selecting form and declares no depends, yet its variables
			// come through source -> dependentA -> allVarsA, so it waits for that selection
			verify(srcDriven.dependsOnForTest("dataSetA"),
					"a source-driven list depends on the selection its source resolves through")
			verify(!srcDriven.dependsOnForTest("dataSetB"), "...and on no other selection")

			// no host anywhere, no selection dependency (the analysis-wide VariableInfo is in
			// place long before any control binds, so there is nothing to wait for)
			verify(!targetC.dependsOnForTest("dataSetA") && !targetC.dependsOnForTest("dataSetB"),
					"a list in a form without any selection depends on no selection at all")
			verify(!allVarsC.dependsOnForTest("dataSetA"),
					"neither does an available list that selects nothing itself")
			verify(!aSwitch.dependsOnForTest("dataSetA"),
					"a plain Switch resolves through no selection, so it has no derived edge (C10)")

			// a host is the OWNER of its selection, not a dependent of it
			verify(!allVarsA.dependsOnForTest("dataSetA"), "the host itself is not its own dependency")
			verify(!allVarsB.dependsOnForTest("dataSetB"), "likewise for the second host")
			verify(!rCodeArea.dependsOnForTest("rCodeSet"), "and for the text-area host")
		}

		// The regression a provenance repair pass used to paper over: loading options whose
		// selection points at another dataset than the analysis filter is running on must stamp
		// the UNCHANGED values with that dataset from the very first bind. setOptions is the same
		// path a .jasp reload takes (store the loaded values, then setAnalysisUp rebinds every
		// control). The bind order comes from the derived edges alone.
		function test_saved_selection_stamps_values_on_first_bind()
		{
			var secondEntry = _entryFor("Second - ")
			var firstEntry  = null

			for (var entry of allVarsA.filterSelectionValues)
				if (entry.label.indexOf("Second - ") !== 0)
				{
					firstEntry = entry
					break
				}

			verify(secondEntry !== null && firstEntry !== null, "fixture must offer a filter of each dataset")

			var savedFilterA = parseInt(secondEntry.value)
			var savedFilterB = parseInt(firstEntry.value)
			var originalA	 = allVarsA.selectedFilterId
			var originalB	 = allVarsB.selectedFilterId
			var bound		 = {}

			try
			{
				jaspForm.setOptions({
					dataSetA:	secondEntry.value,				//saved selection: dataset 'Second'
					dataSetB:	firstEntry.value,					//saved selection: the first dataset
					aSwitch:	true,
					aDependent:	{ value: ["TestInts"], types: ["scale"] },	//unchanged from the file
					dependentA:	{ value: ["TestInts"], types: ["scale"] },
					dependentB:	{ value: ["TestInts"], types: ["scale"] }
				})
				wait(50)
				bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			} finally
			{
				//functions run alphabetically: put the selections back for the tests after this one
				allVarsA.selectedFilterId = originalA
				allVarsB.selectedFilterId = originalB
				wait(20)
			}

			var meta = bound[".meta"]

			compare(parseInt(String(bound.dataSetA)), savedFilterA, "list A took the loaded selection")
			verify(meta && meta.aDependent && meta.dependentA && meta.dependentB,
					"the loaded options bound and stamped their meta")

			// "TestInts" lives in both datasets: only the stamp tells them apart, and a stamp taken
			// before dataSetA was bound would name the analysis' current filter instead.
			compare(meta.aDependent.filterId, savedFilterA,
					"a value with no depends: of its own is stamped through its list's selection")
			compare(meta.dependentA.filterId, savedFilterA, "the one that declares depends: likewise")
			compare(meta.dependentB.filterId, savedFilterB, "the second section's value is stamped with B's loaded filter")
			verify(meta.dependentA.dataSetId !== meta.dependentB.dataSetId,
					"the two stamps name different datasets, not one analysis-wide stamp")
			verify(bound.aDependent.value.indexOf("TestInts") !== -1, "the value itself survived the bind")

			// mds-plan C3/M5: provenance belongs on column-bearing values ONLY. The Switches never
			// carry columns, so they must not appear in .meta at all - and neither does a selection
			// option: its value already IS the filter id. A stamp here would seed the engine's
			// slice queue with a filter no value of this host uses (phantom slice) and could never
			// be refreshed when the host moves on.
			verify(meta.aSwitch === undefined,
					"a scalar carries no provenance stamp, got: " + JSON.stringify(meta.aSwitch))
			verify(meta.bSwitch === undefined,
					"the other scalar carries no provenance stamp either, got: " + JSON.stringify(meta.bSwitch))
			verify(meta.dataSetA === undefined && meta.dataSetB === undefined,
					"selection options need no stamp: their value is the filter id itself")
		}

		// mds-plan C4/RENDER + RESOLVER + LANG: the wrapper call shown in the form carries the
		// syntax surface (bare dataset title for a default filter, "title - filterName" for a
		// named one - never the bound filter id), re-renders when a selection changes, and
		// survives a language switch: re-resolving the shown text through the bridge's own
		// exact-match map must land on exactly the filter ids that are bound (round-trip).
		function test_shownCall_selection_roundtrip_survives_languagechange()
		{
			var namedEntry   = _entryFor("Second - ReopenMe")			// named filter of dataset 'Second'
			var secondEntry  = _entryFor("Second - ")					// the default filter comes first per dataset
			verify(namedEntry !== null && secondEntry !== null,
					"fixture must offer both the default and the named 'ReopenMe' filter of 'Second'")

			var originalA = allVarsA.selectedFilterId
			var originalB = allVarsB.selectedFilterId

			try
			{
				allVarsA.selectedFilterId = parseInt(namedEntry.value)		// named filter
				allVarsB.selectedFilterId = parseInt(secondEntry.value)		// default filter
				wait(50)

				compare(String(allVarsA.selectedFilterId), String(namedEntry.value), "list A holds the named filter id on the wire")

				var syntax = form.rSyntaxText
				var mA = syntax.match(/dataSetA\s*=\s*"([^"]*)"/)
				var mB = syntax.match(/dataSetB\s*=\s*"([^"]*)"/)
				verify(mA !== null && mB !== null, "the shown call must render both selection options, got: " + syntax)

				// named filter -> exactly its dropdown label (user strings all the way down);
				// default filter -> the BARE title, never the translatable "... - Default filter":
				compare(mA[1], "Second - ReopenMe", "a named selection renders as 'title - filterName'")
				compare(mB[1], "Second", "a default-filter selection renders as the bare dataset title")
				verify(syntax.indexOf("Default filter") === -1,
						"no translatable label fragment may enter the shown call")

				// the round trip: shown text -> exact-match resolver (the bridge's own lookup) ->
				// the very filter ids that are bound on the wire:
				compare(form.resolveDataSetSelectionText(mA[1]), String(allVarsA.selectedFilterId),
						"re-parsing the shown surface string lands on list A's bound filter id")
				compare(form.resolveDataSetSelectionText(mB[1]), String(allVarsB.selectedFilterId),
						"...and on list B's")

				// LANG: a language change re-emits the shown call (Analyses::languageChangedHandler
				// emits form()->languageChanged() on every form; the render always rebuilds from the
				// live workspace map), and the re-shown text still resolves to the same ids.
				var emittedBefore = spySyntax.count
				jaspForm.languageChanged()
				wait(50)
				verify(spySyntax.count > emittedBefore,
						"languageChanged must re-emit the shown wrapper call")

				var syntax2 = form.rSyntaxText
				var m2A = syntax2.match(/dataSetA\s*=\s*"([^"]*)"/)
				var m2B = syntax2.match(/dataSetB\s*=\s*"([^"]*)"/)
				verify(m2A !== null && m2B !== null, "the re-shown call still carries both selections")
				compare(form.resolveDataSetSelectionText(m2A[1]), String(allVarsA.selectedFilterId),
						"the round-trip invariant holds across the language switch (list A)")
				compare(form.resolveDataSetSelectionText(m2B[1]), String(allVarsB.selectedFilterId),
						"the round-trip invariant holds across the language switch (list B)")
			} finally
			{
				allVarsA.selectedFilterId = originalA
				allVarsB.selectedFilterId = originalB
				wait(20)
			}
		}

		// mds-plan C4/REGISTRY: the bridge reads the form's selection-option registry, so
		// dataSetSelectionOptionNames must hold exactly the live hosts' option names - the LIST
		// hosts and the TextArea host alike (mds-plan D2.2 moved the form hosts onto the lists;
		// the registry is host-type-agnostic). The old findChildren scan covered the statically
		// created kinds; the registry covers every lifecycle point, tested here for membership.
		function test_selectionOptionRegistry_membership()
		{
			var names = form.dataSetSelectionOptionNames()
			verify(names.indexOf("dataSetA") !== -1, "list A's option is registered, got: " + names)
			verify(names.indexOf("dataSetB") !== -1, "list B's option is registered, got: " + names)
			verify(names.indexOf("rCodeSet")  !== -1, "the TextArea host's option is registered, got: " + names)
			verify(names.indexOf("dependentA") === -1, "a plain list option is not a selection option, got: " + names)

			// The M9 case proper: a host instantiated AFTER form completion (what a ComponentsList
			// row does) must be in the registry right away - the bridge translates its option from
			// then on, no findChildren moment needed. The production dynamic path reaches the host
			// through JASPControl's formIsKnown signal (RowControls set the isDynamic context
			// property); a plain createObject from the test takes the static branch, so fire that
			// same signal here - it is the exact entry point handleFormIsKnown listens on.
			var late = lateHostComponent.createObject(jaspForm)
			verify(late !== null, "the late host was created")
			late.formIsKnown(jaspForm)
			wait(20)
			verify(form.dataSetSelectionOptionNames().indexOf("lateSet") !== -1,
					"a host created after form completion registers immediately (M9)")

			// mds-plan C5 (chunk-4 finding #1): destroying that host again is safe NOW. The plain
			// createObject took the STATIC registration branch (JASPControl::componentComplete ->
			// AnalysisForm::addControl), so the host also entered the form's control caches - which
			// had no unregister path, and the next bind pass (setOptions -> setAnalysisUp ->
			// _setUpModels/_setUp/bindTo over _controls and _dependsOrderedCtrls) dereferenced the
			// freed pointer: deterministic SEGV. The caches are pruned synchronously on destruction
			// now, so the whole create -> bind -> destroy -> bind cycle below must survive.
			jaspForm.setOptions({ aSwitch: true })	//bind pass with the late host still alive
			wait(50)

			late.destroy()							//deleteLater: the destroyed signal prunes on the event loop
			wait(20)
			verify(form.getControl("lateRCode") === null, "a destroyed control must not resolve from the form's control cache anymore")
			verify(form.dataSetSelectionOptionNames().indexOf("lateSet") === -1, "the selection registry must not hold the dead host either")

			jaspForm.setOptions({ aSwitch: false })	//the previously crashing pass, run over pruned caches
			wait(50)
			var bound = JSON.parse(jaspForm.analysis.boundValuesAsJson())
			compare(String(bound.aSwitch), "false", "the bind pass completed normally after the destruction")

			//and the freed option name is reusable: the pruned entry must not resurface as a bogus
			//"2 controls have the same name" collision:
			var revived = lateHostComponent.createObject(jaspForm)
			wait(20)
			verify(form.getControl("lateRCode") !== null, "a recreated control takes the freed name")
			verify(!revived.hasError, "the recreated control must not be flagged as a duplicate")
			revived.destroy()
			wait(20)
		}
	}
}
