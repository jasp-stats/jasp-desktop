import QtTest
import QtQuick
import JASP.Controls

// Per-LIST dataset/filter selection (VariablesListBase::dataSetSelectionOption, mds-plan C10/D2.2):
// every list that declares an option name owns a selection whose value is a FILTER ID, stored in
// that option of the analysis (so it reaches the engine identically in desktop and syntax mode)
// and served to its own controls through the list's own VariableInfo provider. A plain assigned
// list inherits the namespace of the available list its items come from (JASPListControl::
// resolvedSelectionHost following its relation chain), so selecting on the available list drives
// the whole section - while any other list can hold a different selection at the same time.
// TextAreaBase keeps a selection of its own exactly like before.
// The hosts are independent: selecting in one never changes what another shows, and selection
// code NEVER touches the analysis' own filter (that global belongs to the FilterMenuButton).
// Controls below a selecting host get their bindTo dependency on that host's FilterSelect derived
// from the varInfo resolution (JASPControl::selectionDependency), so a selection loaded from a
// file is in effect before anything that uses it takes its provenance (mds-plan D3.4: generated
// into the transient `.filterIds` sidecar at send time, read off the host) - no repair pass.
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
			// This is what the relation-chain resolution (JASPListControl::resolvedSelectionHost,
			// mds-plan D4.2) buys over a visual-parent lookup.
			AssignedVariablesList	{ id: srcDriven;	name: "srcDriven";	source: dependentA }
		}

		// mds-plan D6.2: a fourth host - an ASSIGNED list that hosts a selection of its own (the
		// second-source pattern the component zoo exercises; the module form does the same
		// live). Its option is the destination of the cross-host typed-copy test below.
		AssignedVariablesList		{ id: boundG;		name: "boundG";		dataSetSelectionOption:	"dataSetG" }

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

		SignalSpy
		{
			id:				spyDataSpec
			target:			jaspForm.analysis
			signalName:		"dataSpecChanged"
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

				var bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))	//mds-plan D3.4: the generated Engine tree (the mirror is deleted)
				var meta = bound[".meta"]
				var ids = bound[".filterIds"]

				verify(meta && meta.dependentB, "the column value bound and meta'd itself")
				verify(ids && Array.isArray(ids.dependentB), "the column value carries its sidecar provenance")
				compare(ids.dependentB[0], f1, "the column value is provenanced with the live selection (positional sidecar, C11)")
				verify(meta.dependentB.shouldEncode === true, "the gate keeps the generic .meta of an encoded value (C8: no ids in meta)")

				//and the assigned list that resolves through this host is stamped likewise:
				//(dependentB declares depends: on the option; its namespace comes from allVarsB)

				//the actual mid-session switch: same dataset, other filter, no value change -
				allVarsB.selectedFilterId = f2
				wait(50)

				bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
				meta = bound[".meta"]
				ids = bound[".filterIds"]

				compare(allVarsB.selectedFilterId, f2, "the host took the new selection")
				verify(String(bound.dependentB.value).indexOf("TestInts") !== -1, "the value survived the switch untouched")
				compare(ids.dependentB[0], f2, "the column value followed the host onto the new filter (sidecar)")

				//and no provenance anywhere in the analysis still names the abandoned filter - that
				//is what the engine would queue as a phantom slice:
				var seen = []
				;(function walk(node)
				{
					if (Array.isArray(node)) { for (const e of node) walk(e); return }
					if (node !== null && typeof node === "object") { for (const k of Object.keys(node)) walk(node[k]); return }
					if (typeof node === "number") seen.push(node)
				})(ids)
				verify(seen.indexOf(f1) === -1, "the abandoned filter id is gone from the whole sidecar: " + JSON.stringify(seen))

				//scalars and the selection option carry no provenance at all (C3/M5 gate):
				verify(meta.bSwitch === undefined && ids.bSwitch === undefined, "a scalar carries neither meta nor sidecar node")
				verify(meta.dataSetB === undefined && ids.dataSetB === undefined, "the selection option carries no provenance: its value IS the filter id")
			} finally
			{
				allVarsB.selectedFilterId = originalB	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// mds-plan C10/D2.4: the analysis itself is a VariableInfoProvider over the selection-host
		// registry (AnalysisBase::selectedSelectionFilters etc.) - one namespace while every host
		// happens to sit on the same filter, MERGED once they split. The merged union keeps name
		// collisions (a column behind two selected namespaces is two entries - the terms' filterId
		// disambiguates downstream), and the merged case honestly has no single encoder handle.
		function test_analysisProvider_aggregatesHostRegistry()
		{
			//All four hosts default to the workspace's current filter (test_defaults...): the
			//aggregate dedups by FILTER, not by host - one distinct filter, not merged.
			var ids = jaspForm.analysis.selectionNamespaceFilterIds()
			compare(ids.length, 1, "four hosts on one filter aggregate to one namespace, got: " + ids)
			verify(!jaspForm.analysis.selectionNamespaceIsMerged(), "one distinct filter is not merged")

			var originalA = allVarsA.selectedFilterId
			var originalB = allVarsB.selectedFilterId
			var originalTA = rCodeArea.selectedFilterId
			var originalG = boundG.selectedFilterId

			try
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

				allVarsA.selectedFilterId = parseInt(secondEntry.value)	//dataset 'Second'
				allVarsB.selectedFilterId = parseInt(firstEntry.value)	//dataset 1
				wait(20)

				verify(jaspForm.analysis.selectionNamespaceIsMerged(),
						"lists on two datasets make the analysis namespace merged")
				ids = jaspForm.analysis.selectionNamespaceFilterIds()
				verify(ids.indexOf(parseInt(secondEntry.value)) !== -1 && ids.indexOf(parseInt(firstEntry.value)) !== -1,
						"both selected filters are in the aggregate, got: " + ids)

				//The union view: 'TestInts' lives in BOTH datasets, so it must appear once per
				//source filter - both visible, distinct (C9 disambiguates downstream).
				var names = jaspForm.analysis.selectionNamespaceVariableNames()
				var testInts = names.filter(function (n) { return n === "TestInts" }).length
				compare(testInts, 2, "'TestInts' must be visible once per selected filter, got: " + names)

				//...and the merge collapses by itself when the hosts reunite (per-use resolution,
				//nothing cached): ALL four hosts (the text area and the assigned-list host included)
				//onto one filter.
				rCodeArea.selectedFilterId = parseInt(firstEntry.value)
				allVarsB.selectedFilterId = parseInt(firstEntry.value)
				allVarsA.selectedFilterId = parseInt(firstEntry.value)
				boundG.selectedFilterId = parseInt(firstEntry.value)
				wait(20)
				verify(!jaspForm.analysis.selectionNamespaceIsMerged(),
						"all hosts back on one filter: single namespace again")
			} finally
			{
				allVarsA.selectedFilterId = originalA	//functions run alphabetically: don't bleed
				allVarsB.selectedFilterId = originalB
				rCodeArea.selectedFilterId = originalTA
				boundG.selectedFilterId = originalG
				wait(20)
			}
		}

		// mds-plan D2.5 (analysis side of the switch): a host selection change reaches the
		// ANALYSIS - the aware option-write path (dropdown mirroring the host) plus the explicit
		// provider notification (HostFilterSelection::applyFilter -> AnalysisForm ->
		// AnalysisBase::handleSelectionNamespaceMaybeChanged) mean dataSpecChanged re-emits on a
		// mid-session switch, so title/RPC/dataSetIds consumers re-read the live dataset set.
		function test_hostSwitch_reemitsDataSpec()
		{
			var secondEntry = _entryFor("Second - ")
			verify(secondEntry !== null, "fixture must offer a second-dataset filter")
			var secondFilterId = parseInt(secondEntry.value)
			var originalB = allVarsB.selectedFilterId

			try
			{
				var before = spyDataSpec.count
				allVarsB.selectedFilterId = secondFilterId
				wait(50)
				verify(spyDataSpec.count > before,
						"switching one list's host selection re-emits dataSpecChanged on the analysis")
				compare(String(JSON.parse(jaspForm.createOptionsJsonAsJson(0)).dataSetB), String(secondFilterId),
						"the selection option follows the host (the engine-side slice key travels as the option)")
			} finally
			{
				allVarsB.selectedFilterId = originalB	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// mds-plan D2.5 (list side of the switch): the host restamps the TERMS of its own bound
		// controls - host-inclusive, source-following - writing the typed filterId (C9), and the
		// walk stops at other hosts' subtrees. Same-dataset switch (no value rewrite at all) is
		// the hard case: only the restamp walk keeps typed state and .meta moving together.
		// The bind-time stamps double as the pinned DESTINATION rule of mds-plan D2.1: a term
		// bound into a control is stamped with the filter THAT control resolves through (its own
		// host, else the source chain, else -1), never with the filter the value happened to come
		// from in the file.
		function test_hostSwitch_typedRestamp_staysWithinHost()
		{
			var secondEntry = _entryFor("Second - ")
			var reopenEntry = _entryFor("Second - ReopenMe")
			var firstEntry  = null
			for (var entry of allVarsA.filterSelectionValues)
				if (entry.label.indexOf("Second - ") !== 0)
				{
					firstEntry = entry
					break
				}
			verify(secondEntry !== null && reopenEntry !== null && firstEntry !== null,
					"fixture must offer two filters of 'Second' and one of the first dataset")

			var fSecond	= parseInt(secondEntry.value)
			var fReopen	= parseInt(reopenEntry.value)
			var fFirst	= parseInt(firstEntry.value)
			var originalA = allVarsA.selectedFilterId
			var originalB = allVarsB.selectedFilterId

			try
			{
				//Two hosts, two DATASETS at once; then bind the same column name into both
				//sections, the source-driven list below host A, and the hostless list:
				allVarsA.selectedFilterId = fSecond
				allVarsB.selectedFilterId = fFirst
				wait(20)
				jaspForm.setOptions({
					dependentA:	{ value: ["TestInts"], types: ["scale"] },
					aDependent:	{ value: ["TestInts"], types: ["scale"] },
					srcDriven:	{ value: ["TestInts"], types: ["scale"] },
					dependentB:	{ value: ["TestInts"], types: ["scale"] },
					targetC:	{ value: ["TestInts"], types: ["scale"] }
				})
				wait(50)

				//bind-time destination stamps: A-section through host A's selection, B-section
				//through host B's, the source-driven one through A's chain (it lives under the
				//NON-selecting form!), and the hostless one unqualified:
				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + fSecond,
						"host A's assigned list stamps its terms at bind through host A")
				compare(aDependent.model.termProvenanceForTest().join(","), "TestInts@" + fSecond,
						"the no-depends assigned list likewise (inherits through its related available list)")
				compare(srcDriven.model.termProvenanceForTest().join(","), "TestInts@" + fSecond,
						"a source-driven list stamps through its SOURCE's host, not its visual parent")
				compare(dependentB.model.termProvenanceForTest().join(","), "TestInts@" + fFirst,
						"host B's list stamps through host B - same column name, different typed filter")
				compare(targetC.model.termProvenanceForTest().join(","), "TestInts@-1",
						"a list with no selection host keeps its terms unqualified (C13 legacy)")

				//The mid-session switch, same dataset, OTHER filter - nothing is re-entered and no
				//value changes; the walk must move typed AND meta provenance together, and ONLY
				//below the switched host:
				allVarsA.selectedFilterId = fReopen
				wait(50)

				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + fReopen,
						"switched host re-stamps its own bound list's terms (typed filterId)")
				compare(aDependent.model.termProvenanceForTest().join(","), "TestInts@" + fReopen,
						"the served no-depends list follows")
				compare(srcDriven.model.termProvenanceForTest().join(","), "TestInts@" + fReopen,
						"the source-driven list (visually parked elsewhere) follows too - its names come from there")
				compare(dependentB.model.termProvenanceForTest().join(","), "TestInts@" + fFirst,
						"THE REGRESSION: host B's terms are untouched by host A's switch")
				compare(targetC.model.termProvenanceForTest().join(","), "TestInts@-1",
						"and the hostless list never acquires a stamp from anyone's switch")

				//the generated sidecar follows the typed terms exactly (they are one state now -
				//mds-plan D3.4: the sidecar IS the move, generated from the terms per send):
				var ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
				compare(ids.dependentA[0], fReopen, "the generated sidecar followed host A")
				compare(ids.dependentB[0], fFirst, "the sidecar of host B untouched")
			} finally
			{
				allVarsA.selectedFilterId = originalA	//there and back: the walk restamps again
				allVarsB.selectedFilterId = originalB
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

				var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

 		// mds-plan D6.2: CROSS-HOST bindTo. A typed copy - the generated Storage tree of a list
 		// sitting on ANOTHER host's selection, per-term objects carrying the source filterId (the
 		// D5.1 file shape is exactly the carrier a save/reload or a control-to-control copy moves
 		// around) - bound into boundG, whose OWN selection points at a different dataset. The
 		// destination rules must NOT requalify it: the intake guard (ListModel::_setAllowedType,
 		// D5.1) stamps only UNQUALIFIED terms, and the restamp walk (D3.5 owns-the-stamp) moves
 		// only terms the host owns. So the copy keeps the SOURCE filterId through the bind, is
 		// provenanced to the source in the generated sidecar (the engine encodes it through the
 		// source's view), and survives a switch of the DESTINATION host unchanged.
 		function test_crossHostBindTo_typedCopyKeepsSourceStamp()
 		{
 			var secondEntry = _entryFor("Second - ")
 			var reopenEntry = _entryFor("Second - ReopenMe")
 			var firstEntry  = null
 			for (var entry of allVarsA.filterSelectionValues)
 				if (entry.label.indexOf("Second - ") !== 0)
 				{
 					firstEntry = entry
 					break
 				}
 			verify(secondEntry !== null && reopenEntry !== null && firstEntry !== null,
 					"fixture must offer two filters of 'Second' and one of the first dataset")

 			var sourceFilter = parseInt(secondEntry.value)	//host A's selection: the copy comes from here
 			var destFilter   = parseInt(firstEntry.value)		//boundG's own selection: different dataset
 			var destAlt      = parseInt(reopenEntry.value)	//a later switch of the DESTINATION host
 			verify(sourceFilter !== destFilter && destAlt !== sourceFilter, "three distinct filters for the three roles")

 			var originalA = allVarsA.selectedFilterId
 			var originalG = boundG.selectedFilterId

 			try
 			{
 				//source side: dependentA under host A (a 'Second' filter) gets a column
 				allVarsA.selectedFilterId = sourceFilter
 				boundG.selectedFilterId   = destFilter
 				wait(20)
 				jaspForm.setOptions({
 					dependentA: { value: ["TestInts"], types: ["scale"] }
 				})
 				wait(50)
 				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"the source list's term is stamped through host A")

 				//the typed copy: generated Storage tree carries the source filterId per term
 				var storage = JSON.parse(jaspForm.createOptionsJsonAsJson(1))	//format 1 = Storage
 				verify(storage.dependentA && storage.dependentA.value && storage.dependentA.value.length === 1,
 						"storage shape present for dependentA: " + JSON.stringify(storage.dependentA))
 				compare(storage.dependentA.value[0].filterId, sourceFilter,
 						"the D5.1 storage term object carries the source filterId (the copy's carrier)")

 				//bind the copy into boundG - its own host sits on the OTHER dataset:
 				jaspForm.setOptions({ boundG: storage.dependentA })
 				wait(50)

 				compare(boundG.selectedFilterId, destFilter, "boundG still holds its OWN selection")
 				compare(boundG.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"THE FLAGSHIP: the typed copy keeps the SOURCE filterId - the destination host must not requalify it (D5.1 intake guard, only unqualified terms get stamped)")

 				//the generated sidecar hands the copy to the engine with the SOURCE id (encoding
 				//goes through the source filter's VIEW, D3.3 per-term walk):
 				var ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
 				compare(ids.boundG[0], sourceFilter, "the generated sidecar provenances the copy with the SOURCE filter")
 				verify(ids.boundG[0] !== destFilter, "...which is NOT boundG's own selection")

 				//D3.5 owns-the-stamp: switching the DESTINATION host restamps only what boundG owns
 				//(its old filter or unqualified) - the foreign copy stays with its source:
 				boundG.selectedFilterId = destAlt
 				wait(50)
 				compare(boundG.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"the destination host's switch leaves the foreign-stamped copy alone (owns-the-stamp)")
 				ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
 				compare(ids.boundG[0], sourceFilter, "and the sidecar still names the SOURCE filter")

 				//sanity of the walk itself: a term bound AFTER the destination switch is owned by
 				//the destination and stamps through it (the guard stamps -1 terms only):
 				jaspForm.setOptions({ boundG: { value: ["SecondOnly"], types: ["scale"] } })
 				wait(50)
 				compare(boundG.model.termProvenanceForTest().join(","), "SecondOnly@" + destAlt,
 						"an UNQUALIFIED bind into the same host still takes the destination stamp (the guard condition stays filterId < 0)")
 			} finally
 			{
 				jaspForm.setOptions({ dependentA: { value: [], types: [] }, boundG: { value: [], types: [] } })
 				boundG.selectedFilterId = originalG	//functions run alphabetically: don't bleed
 				allVarsA.selectedFilterId = originalA	//(selections LAST: applyFilter re-writes the option after the cleanup bind)
 				wait(20)
 			}
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
			var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

			var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

			var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

			var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

				var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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

			var restored = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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
				bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
			} finally
			{
				//functions run alphabetically: put the selections back for the tests after this one
				allVarsA.selectedFilterId = originalA
				allVarsB.selectedFilterId = originalB
				wait(20)
			}

			var meta = bound[".meta"]
			var ids = bound[".filterIds"]

			compare(parseInt(String(bound.dataSetA)), savedFilterA, "list A took the loaded selection")
			verify(meta && meta.aDependent && meta.dependentA && meta.dependentB,
					"the loaded options bound and carried their (id-free, C8) meta")

			// "TestInts" lives in both datasets: only the provenance tells them apart, and an
			// id taken before dataSetA was bound would name the analysis' current filter instead
			// (mds-plan D3.4: the transient sidecar carries what the mirror used to stamp).
			verify(ids && Array.isArray(ids.aDependent) && Array.isArray(ids.dependentA) && Array.isArray(ids.dependentB),
					"the loaded column values carry sidecar provenance")
			compare(ids.aDependent[0], savedFilterA,
					"a value with no depends: of its own is provenanced through its list's selection")
			compare(ids.dependentA[0], savedFilterA, "the one that declares depends: likewise")
			compare(ids.dependentB[0], savedFilterB, "the second section's value is provenanced with B's loaded filter")
			//different datasets, not one analysis-wide stamp: each value is provenanced with the
			//filter ITS host loaded (A on a 'Second'-dataset filter, B on the first dataset's),
			//and those two saved filters are distinct ids (mds-plan D3.4: the sidecar carries
			//what the mirror's dataSetId/filterId stamp pair said; filter ids imply their
			//dataset, so differing ids from dataset-distinct entries ARE the evidence - no
			//label grammar parsed, C4).
			verify(savedFilterA !== savedFilterB, "fixture: the two saved selections name different filters")
			verify(ids.dependentA[0] !== ids.dependentB[0],
					"the two provenances name different filters (A through the 'Second' host, B through the first dataset's), not one analysis-wide id")
			verify(bound.aDependent.value.indexOf("TestInts") !== -1, "the value itself survived the bind")

			// mds-plan C3/M5: provenance belongs on column-bearing values ONLY. The Switches never
			// carry columns, so they must not appear in .meta at all - and neither does a selection
			// option: its value already IS the filter id. A stamp here would seed the engine's
			// slice queue with a filter no value of this host uses (phantom slice) and could never
			// be refreshed when the host moves on.
			verify(meta.aSwitch === undefined && ids.aSwitch === undefined,
					"a scalar carries no meta and no sidecar node, got: " + JSON.stringify(meta.aSwitch))
			verify(meta.bSwitch === undefined && ids.bSwitch === undefined,
					"the other scalar carries none either, got: " + JSON.stringify(meta.bSwitch))
			verify(meta.dataSetA === undefined && meta.dataSetB === undefined
					&& ids.dataSetA === undefined && ids.dataSetB === undefined,
					"selection options need no provenance: their value is the filter id itself")
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

		// mds-plan D2.3: the form's registry exposes the live selection HOSTS (count + list), the
		// selecting lists and the TextArea alike, and it tracks their lifecycle eagerly. The
		// per-host bind-order promise (each host's FilterSelect before the controls it serves)
		// is pinned in test_selection_dependency_is_derived; here: registry integrity - the
		// control registered for a host's option IS that host's own dropdown.
		function test_selectionHostRegistry_introspection()
		{
			compare(form.selectionHostCount(), 4, "the three selecting lists and the text area are the hosts")

			var hosts = form.selectionHosts()
			verify(hosts.indexOf(allVarsA) !== -1 && hosts.indexOf(allVarsB) !== -1 && hosts.indexOf(rCodeArea) !== -1
					&& hosts.indexOf(boundG) !== -1,
					"the live hosts are exactly the selecting ones, got: " + hosts)
			verify(hosts.indexOf(allVarsC) === -1 && hosts.indexOf(targetC) === -1 && hosts.indexOf(dependentA) === -1,
					"controls without a selection are not hosts")

			//registry integrity: getControl(option) resolves to the host's OWN FilterSelect, the
			//control selectionDependency() orders the served controls against:
			var selectA = _selectControlFor(allVarsA, "dataSetA")
			verify(selectA !== null, "list A's dropdown lives in its chrome")
			verify(form.getControl("dataSetA") === selectA,
					"the registered option control IS the host's own dropdown")

			//count is lifecycle-tracked: a late host joins immediately, its destruction removes
			//it EAGERLY (on destroyed, not read-time pruning), and the real hosts survive:
			var late = lateHostComponent.createObject(jaspForm)
			late.formIsKnown(jaspForm)
			wait(20)
			compare(form.selectionHostCount(), 5, "the late host registered")
			verify(form.selectionHosts().indexOf(late) !== -1, "and it is in the host list")
			late.destroy()
			wait(20)
			compare(form.selectionHostCount(), 4, "a destroyed host unregisters with the object")
			verify(form.selectionHosts().indexOf(allVarsA) !== -1 && form.selectionHosts().length === 4,
					"and the real hosts survived the churn")
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
			var bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
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
