import QtTest
import QtQuick
import JASP.Controls

// Per-LIST dataset/filter selection (VariablesListBase::dataSetSelectionOption):
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
// file is in effect before anything that uses it takes its provenance (generated
// into the transient `.filterIds` sidecar at send time, read off the host) - no repair pass.
// Fixture (see Tests/testqml.cpp): dataset 1 (shown: TestInts/TestLetters/TestDoubles/TestNominal)
// plus "Second" (TestInts with different values + SecondOnly), with default filter + extras; the
// workspace is then REOPENED from the database, so every test runs against a reopened file and
// the eager filter materialisation of DataSet::dbLoad is what puts the stored
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

			// The selection lives on the available list: it drives allVarsA itself
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

			// A scalar under a NON-selecting form, for the provenance-gate tests:
			// It carries no columns, so its .meta must stay free of dataSetId/filterId.
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
			//) buys over a visual-parent lookup.
			AssignedVariablesList	{ id: srcDriven;	name: "srcDriven";	source: dependentA }
		}

		// a fourth host - an ASSIGNED list that hosts a selection of its own (the
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

	// an R-code TextArea host for the qualified-reference encode test
	// below (same late-host pattern as lateHostComponent, created and destroyed per test).
	Component
	{
		id:			qualifiedCodeHostComponent
		TextArea
		{
			name:					"qualifiedCodeBox"
			textType:				JASPControl.TextTypeRcode
			dataSetSelectionOption:	"qualifiedCodeSet"
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

		// The phantom-slice class: switching a host to ANOTHER
		// FILTER OF THE SAME DATASET rewrites no value at all - the encoded column names stay
		// perfectly valid - so provenance can only stay honest through the participation-keyed
		// reconcile that HostFilterSelection::applyFilter drives on every selection change
		// (availableTermsReset carries filterIdToUpdate = the OLD selection; participating
		// references are replaced by the delivered universe term). Column values must follow
		// the selection; scalars and the selection option itself carry no provenance, so the
		// abandoned filter cannot linger in .meta (and from there in the engine's slice queue,
		// referencedDataSets() or the RPC dataSetIds report).
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

				var bound = JSON.parse(jaspForm.createOptionsJsonAsJson(0))	// the generated Engine tree (the mirror is deleted)
				var meta = bound[".meta"]
				var ids = bound[".filterIds"]

				verify(meta && meta.dependentB, "the column value bound and meta'd itself")
				verify(ids && Array.isArray(ids.dependentB), "the column value carries its sidecar provenance")
				compare(ids.dependentB[0], f1, "the column value is provenanced with the live selection (positional sidecar)")
				verify(meta.dependentB.shouldEncode === true, "the gate keeps the generic .meta of an encoded value (no ids in meta)")

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

				//scalars and the selection option carry no provenance at all:
				verify(meta.bSwitch === undefined && ids.bSwitch === undefined, "a scalar carries neither meta nor sidecar node")
				verify(meta.dataSetB === undefined && ids.dataSetB === undefined, "the selection option carries no provenance: its value IS the filter id")
			} finally
			{
				allVarsB.selectedFilterId = originalB	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// the analysis itself is a VariableInfoProvider over the selection-host
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
				//source filter - both visible, distinct (the filterId disambiguates downstream).
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

		// (analysis side of the switch): a host selection change reaches the
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

		// (ORIGIN-AUTHORITATIVE, -1 governance): stamps are born ONLY at origins -
		// the pool universe's birth stamp - and a bind is not an origin: a bare bound value stays
		// -1 forever, and its engine provenance follows its host-bound fallback chain per send (the generated
		// sidecar re-reads the host each time). A term that CARRIES an origin stamp - a value
		// saved in a file after it was selected through its pool, reaching a reload in the
		// Storage shape (exactly how this test delivers it) - is a reference into that provider
		// and FOLLOWS it when the pool's selection moves: the signal-driven reconcile REPLACES it
		// by the pool's delivered universe term (the successor of the deleted restamp walk; no
		// "new id" travels anywhere, the replacement rides the delivered term). The switch of one
		// host moves only the terms whose origin is that provider - another host's stamps and
		// every -1 term are other providers' references and never move.
		function test_hostSwitch_originStampedTermsFollow_bareStayMinusOne()
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
				//Two hosts, two DATASETS at once; bind the same column name everywhere - the
				//A-section list gets an ORIGIN-stamped term (selected through pool A while it
				//served fSecond), the rest get bare values:
				allVarsA.selectedFilterId = fSecond
				allVarsB.selectedFilterId = fFirst
				wait(20)
				jaspForm.setOptions({
					dependentA:	{ value: [{ name: "TestInts", types: ["scale"], filterId: fSecond }] },
					aDependent:	{ value: ["TestInts"], types: ["scale"] },
					srcDriven:	{ value: ["TestInts"], types: ["scale"] },
					dependentB:	{ value: [{ name: "TestInts", types: ["scale"], filterId: fFirst }] },
					targetC:	{ value: ["TestInts"], types: ["scale"] }
				})
				wait(50)

				//bind-time typed state: a stamped term keeps its origin through the bind (the
				//intake is stamp-free), a bare bind stays unqualified - and
				//NOTHING is destination-stamped any more:
				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + fSecond,
						"the origin-stamped term keeps its provenance through the bind (stamps are born at origins only)")
				compare(dependentB.model.termProvenanceForTest().join(","), "TestInts@" + fFirst,
						"host B's term keeps ITS origin - same column name, different provider")
				compare(aDependent.model.termProvenanceForTest().join(","), "TestInts@-1",
						"a bare bind stays -1 (a reference into the provider WITHOUT host selection, governed by the host-bound fallback chain)")
				compare(srcDriven.model.termProvenanceForTest().join(","), "TestInts@-1",
						"the source-driven list's bare bind sits unqualified until its source edge re-syncs (the setup-bound resync does not run)")
				compare(targetC.model.termProvenanceForTest().join(","), "TestInts@-1",
						"the hostless list never acquires a stamp from a bind (legacy)")

				//the generated sidecar provenances every value through its list's own resolution -
				//for -1 terms that is the host-bound fallback chain, read per send, no typed state involved:
				var ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
				compare(ids.dependentA[0], fSecond, "the stamped term rides its own id")
				compare(ids.dependentB[0], fFirst, "host B's stamped term rides its own id")
				compare(ids.aDependent[0], fSecond, "the bare term's sidecar backfills host A's chain (per send)")
				compare(ids.srcDriven[0], fSecond, "the source-driven one through its SOURCE's host, not its visual parent")

				//The mid-session switch, same dataset, OTHER filter: the pool re-points and
				//delivers its rebuilt (birth-stamped) universe; the reconcile replaces the
				//references INTO that provider and touches nothing else:
				allVarsA.selectedFilterId = fReopen
				wait(50)

				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + fReopen,
						"the origin-stamped reference FOLLOWED its provider - replaced by the delivered universe term")
				compare(aDependent.model.termProvenanceForTest().join(","), "TestInts@-1",
						"-1 governance: the bare term's typed state never moves")
				compare(srcDriven.model.termProvenanceForTest().join(","), "TestInts@" + fReopen,
						"the source-driven list FOLLOWS its source (dependentA's replaced term rides the mirror - its names come from there)")
				compare(dependentB.model.termProvenanceForTest().join(","), "TestInts@" + fFirst,
						"THE REGRESSION: host B's origin-stamped term is untouched by host A's switch")
				compare(targetC.model.termProvenanceForTest().join(","), "TestInts@-1",
						"and the hostless list never acquires a stamp from anyone's switch")

				//engine-facing provenance: the stamped terms carry their moved/kept ids, the bare
				//ones follow their hosts through the chain - each side its own namespace:
				ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
				compare(ids.dependentA[0], fReopen, "the followed term's sidecar names the NEW selection")
				compare(ids.aDependent[0], fReopen, "the bare term's sidecar followed host A through the chain")
				compare(ids.srcDriven[0], fReopen, "the source-driven bare term's sidecar too")
				compare(ids.dependentB[0], fFirst, "host B's sidecar untouched")
			} finally
			{
				allVarsA.selectedFilterId = originalA
				allVarsB.selectedFilterId = originalB
				wait(20)
			}
		}

		// Declaring a dataSetSelectionOption (on a list) is what makes an analysis multi-dataset
		// aware (both modes) - and the ONLY declaration there is: Description.qml has no
		// multiDataSetAware property any more, the Analysis finds out from the loaded form.
		function test_option_marks_analysis_aware()
		{
			verify(jaspForm.analysis.multiDataSetAware, "a list declaring dataSetSelectionOption implies multiDataSetAware")
		}

		// The fixture reopened the database (Tests/testqml.cpp), so this whole file runs against
		// a REOPENED file. 'ReopenMe' was stored, never selected - it reaches the dropdown only
		// because DataSet::dbLoad materialises every stored filter at open (;
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
				//the old coercion bug, which destroyed the saved selection for good.
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

 		// CROSS-HOST bindTo. A typed copy - the generated Storage tree of a list
 		// sitting on ANOTHER host's selection, per-term objects carrying the source filterId (the
 		// file shape is exactly the carrier a save/reload or a control-to-control copy moves
 		// around) - bound into boundG, whose OWN selection points at a different dataset. The
 		// destination rules must NOT requalify it: the intake guard (ListModel::_setAllowedType)
 		// stamps only UNQUALIFIED terms, and the signal-driven reconcile (the origin-
 		// authoritative successor of the deleted restamp walk) moves only references INTO the
 		// provider that changed. So the copy keeps the SOURCE filterId through the bind, is
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
 				//source side: dependentA under host A (a 'Second' filter) gets a column that
 				//CARRIES its origin - bound in the Storage shape, exactly how a term selected
 				//through pool A reaches a reload (a bare bind would stay -1: stamps are born at
 				//origins only, never at intake)
 				allVarsA.selectedFilterId = sourceFilter
 				boundG.selectedFilterId   = destFilter
 				wait(20)
 				jaspForm.setOptions({
 					dependentA: { value: [{ name: "TestInts", types: ["scale"], filterId: sourceFilter }] }
 				})
 				wait(50)
 				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"the origin-stamped term keeps its provenance through the bind (the intake is stamp-free)")

 				//the typed copy: generated Storage tree carries the source filterId per term
 				var storage = JSON.parse(jaspForm.createOptionsJsonAsJson(1))	//format 1 = Storage
 				verify(storage.dependentA && storage.dependentA.value && storage.dependentA.value.length === 1,
 						"storage shape present for dependentA: " + JSON.stringify(storage.dependentA))
 				compare(storage.dependentA.value[0].filterId, sourceFilter,
 						"the storage term object carries the source filterId (the copy's carrier)")

 				//bind the copy into boundG - its own host sits on the OTHER dataset:
 				jaspForm.setOptions({ boundG: storage.dependentA })
 				wait(50)

 				compare(boundG.selectedFilterId, destFilter, "boundG still holds its OWN selection")
 				compare(boundG.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"THE FLAGSHIP: the typed copy keeps the SOURCE filterId - the destination host must not requalify it (the intake guard, only unqualified terms get stamped)")

 				//the generated sidecar hands the copy to the engine with the SOURCE id (encoding
 				//goes through the source filter's VIEW, the per-term walk):
 				var ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
 				compare(ids.boundG[0], sourceFilter, "the generated sidecar provenances the copy with the SOURCE filter")
 				verify(ids.boundG[0] !== destFilter, "...which is NOT boundG's own selection")

 				// (origin-authoritative participation): switching the DESTINATION host
 				//moves only the references INTO the provider it left - the foreign copy (a different
 				//origin) is not one and stays with its source:
 				boundG.selectedFilterId = destAlt
 				wait(50)
 				compare(boundG.model.termProvenanceForTest().join(","), "TestInts@" + sourceFilter,
 						"the destination host's switch leaves the foreign-stamped copy alone (owns-the-stamp)")
 				ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
 				compare(ids.boundG[0], sourceFilter, "and the sidecar still names the SOURCE filter")

 				//sanity of the -1 governance: a term bound AFTER the destination switch stays
 				//UNQUALIFIED (a bind is not an origin) - its engine provenance follows the
 				//destination host through the host-bound fallback chain, in the sidecar, per send:
 				jaspForm.setOptions({ boundG: { value: ["SecondOnly"], types: ["scale"] } })
 				wait(50)
 				compare(boundG.model.termProvenanceForTest().join(","), "SecondOnly@-1",
 						"an UNQUALIFIED bind stays -1 (no destination stamping; the bind replaces the list's value)")
 				ids = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[".filterIds"]
 				compare(ids.boundG[0], destAlt, "and the bare term's sidecar backfills the destination host (per send)")
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
		// form exposes NO selection API any more (stripped hard, no shims):
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

		// a textarea's code text replays through the encoder on every
		// commit. A qualified surface form in the code must resolve and become the
		// REFERENCED slice's token - never mangled into the context namespace by the bare-name
		// pass, never handed to R's parser - while bare names keep encoding under the context's
		// own namespace (unchanged). An unresolvable reference is loud and typed,
		// but non-blocking: the message shows in the text area and the text stays raw.
		function test_textarea_qualified_reference_encodes_into_referenced_slice()
		{
			var host = qualifiedCodeHostComponent.createObject(jaspForm)
			verify(host !== null, "the R-code host was created")
			host.formIsKnown(jaspForm)
			//The plain createObject took the static branch (componentComplete -> addControl), so
			//the host sits in the form's _controls cache - but _dependsOrderedCtrls (what
			//createOptionsJson walks) is only rebuilt by a setAnalysisUp pass, i.e. a bind pass.
			//That is the same entry the other tests use; without it the host's option would not
			//be generated yet.
			jaspForm.setOptions({})
			wait(20)

			try
			{
				//The host's default selection is the shown dataset's current filter (dataset 1's
				//default) - that is the CONTEXT namespace for bare names; the qualified
				//reference points at "Second"'s default-filter slice.
				host.text = 'x <- TestInts + "Second"."SecondOnly"'
				host.applyRequest()
				wait(50)

				var options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
				var model = String(options.qualifiedCodeBox.model)

				var tokens = model.match(/JASPColumn_D\d+_F\d+_\d+_Encoded/g)
				verify(tokens !== null && tokens.length === 2,
						"both references encode as tokens, got: " + model)
				verify(tokens[0] !== tokens[1],
						"the bare name (context namespace) and the qualified reference (Second's slice) form DIFFERENT tokens: " + tokens)

				compare(String(options.qualifiedCodeBox.modelOriginal),
						'x <- TestInts + "Second"."SecondOnly"',
						"the original text stays raw (rCode stays raw in storage)")

				//An unresolvable reference: loud, typed, non-blocking - the text is left
				//unencoded (the bare pass must not mangle it) and the error shows.
				host.text = 'y <- "Nope"."SecondOnly"'
				host.applyRequest()
				wait(50)

				options = JSON.parse(jaspForm.createOptionsJsonAsJson(0))
				compare(String(options.qualifiedCodeBox.model), 'y <- "Nope"."SecondOnly"',
						"an unresolvable reference is left raw, not mangled into context tokens")
				verify(host.hasScriptError, "the text area reports the error state")
				verify(String(host.infoText).indexOf("'Nope'") !== -1,
						"the typed message names the missing slice, got: " + host.infoText)
			}
			finally
			{
				host.destroy()
				wait(20)
			}
		}

		// RSyntax term emission qualifies terms stamped OUTSIDE the emission
		// context. The context is the analysis' own filter (the namespace bare names resolve in
		// at run time, the self-reference rule); a term whose origin is a FOREIGN host (list B
		// on Second's default filter - the stamp was born when it was selected through that pool
		// and reaches the bind in the Storage shape, the file carrier) must render as the
		// qualified minimal form ("Dataset"."Column") in the generated R syntax - never mangled
		// into the context namespace, never carrying a bare ".type" suffix next to its own
		// ("type") segment. Unstamped/own-context terms stay byte-identical legacy (C++-tested).
		function test_rsyntax_emits_qualified_minimal_for_outward_terms()
		{
			var secondEntry = _entryFor("Second - ")
			verify(secondEntry !== null, "fixture must offer Second's default filter")
			var secondId = parseInt(secondEntry.value)
			var originalB = allVarsB.selectedFilterId

			try
			{
				allVarsB.selectedFilterId = secondId
				wait(20)
				jaspForm.setOptions({ dependentB: { value: [{ name: "SecondOnly", types: ["scale"], filterId: secondId }] } })
				wait(50)

				//the term carries the foreign origin's provenance (kept by the stamp-free intake)...
				compare(dependentB.model.termProvenanceForTest().join(","), "SecondOnly@" + secondId,
						"the term keeps its foreign-origin stamp through the bind (no destination stamping)")

				//...and the emission qualifies it: the dataset-title minimal form, no bare
				//".scale" suffix (the qualified form owns its ("scale") segment).
				var syntax = form.rSyntaxText
				verify(syntax.indexOf('"Second"."SecondOnly"') !== -1,
						"the outward term emits the qualified minimal form, got: " + syntax)
				verify(syntax.indexOf("SecondOnly.scale") === -1,
						"no bare .type suffix is appended to a qualified form, got: " + syntax)
			}
			finally
			{
				allVarsB.selectedFilterId = originalB
				jaspForm.setOptions({ dependentB: { value: [], types: [] } })
				wait(20)
			}
		}

		// Agent-2's scratch probe (tst_reopenCoerce.qml) promoted: a selection value naming no
		// filter at all (id 987654: no live filter, no db row - the deleted/corrupted case eager
		// loading cannot rescue) must NOT be coerced to the first dropdown entry. The option keeps
		// the saved value while it waits for a model that can serve it, the host keeps its own
		// selection, and the control reports the problem instead of rewriting the file's content
		// behind the user's back. Restores anyway.
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
					"a plain Switch resolves through no selection, so it has no derived edge")

			// a host is the OWNER of its selection, not a dependent of it
			verify(!allVarsA.dependsOnForTest("dataSetA"), "the host itself is not its own dependency")
			verify(!allVarsB.dependsOnForTest("dataSetB"), "likewise for the second host")
			verify(!rCodeArea.dependsOnForTest("rCodeSet"), "and for the text-area host")
		}

		// The provenance a .jasp reload serves at the very first bind: setOptions is the same
		// path a reload takes (store the loaded values, then setAnalysisUp rebinds every
		// control), and the bind order comes from the derived edges alone.
		// (ORIGIN-AUTHORITATIVE): a bare loaded value is UNQUALIFIED - it stays -1 through the
		// bind (a bind is not an origin; the intake is stamp-free) - and its engine provenance
		// is served per send by the generated sidecar reading each list's OWN loaded selection
		// through the host-bound fallback chain. A value that carries an origin stamp in the file (here: the
		// hostless list's term, bound in the Storage shape) keeps it verbatim.
		function test_first_bind_bareStaysMinusOne_sidecarFollowsLoadedSelections()
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
					dependentB:	{ value: ["TestInts"], types: ["scale"] },
					targetC:	{ value: [{ name: "TestInts", types: ["scale"], filterId: savedFilterB }] }
				})
				wait(50)

				//the bind-time typed state: bare stays -1 everywhere, the stamped term keeps its
				//origin - no bind-time stamping in either direction:
				compare(aDependent.model.termProvenanceForTest().join(","), "TestInts@-1",
						"a bare loaded value stays unqualified through the first bind (-1 governance)")
				compare(dependentA.model.termProvenanceForTest().join(","), "TestInts@-1",
						"likewise for the list that declares depends")
				compare(dependentB.model.termProvenanceForTest().join(","), "TestInts@-1",
						"the second section's bare value too")
				compare(targetC.model.termProvenanceForTest().join(","), "TestInts@" + savedFilterB,
						"a STAMPED value keeps its origin through the first bind (stamps survive; born at origins only)")
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
					"the loaded options bound and carried their (id-free) meta")

			// "TestInts" lives in both datasets: only the provenance tells them apart. The values
			// are -1 (bare) - the provenance is read PER SEND off each list's own
			// loaded selection through the host-bound fallback chain (the transient sidecar).
			verify(ids && Array.isArray(ids.aDependent) && Array.isArray(ids.dependentA) && Array.isArray(ids.dependentB),
					"the loaded column values carry sidecar provenance")
			compare(ids.aDependent[0], savedFilterA,
					"a value with no depends: of its own is provenanced through its list's selection")
			compare(ids.dependentA[0], savedFilterA, "the one that declares depends: likewise")
			compare(ids.dependentB[0], savedFilterB, "the second section's value is provenanced with B's loaded filter")
			compare(ids.targetC[0], savedFilterB, "the hostless list's STAMPED term rides its own id, no chain needed")
			//different datasets, not one analysis-wide stamp: each value is provenanced with the
			//filter ITS host loaded (A on a 'Second'-dataset filter, B on the first dataset's),
			//and those two saved filters are distinct ids (the sidecar carries
			//what the mirror's dataSetId/filterId stamp pair used to say; filter ids imply their
			//dataset, so differing ids from dataset-distinct entries ARE the evidence - no
			//label grammar parsed).
			verify(savedFilterA !== savedFilterB, "fixture: the two saved selections name different filters")
			verify(ids.dependentA[0] !== ids.dependentB[0],
					"the two provenances name different filters (A through the 'Second' host, B through the first dataset's), not one analysis-wide id")
			verify(bound.aDependent.value.indexOf("TestInts") !== -1, "the value itself survived the bind")

			// provenance belongs on column-bearing values ONLY. The Switches never
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

		// (ORIGIN-AUTHORITATIVE prune): the staleness prune of a pool participates
		// only references INTO that pool's own provider (stamp == the pool's universe stamp,
		// ListModel::termFollowsProviderChange). A foreign-stamped reference - another selection's
		// typed copy, reaching the list the way a file delivers it (the Storage shape) - is not
		// this pool's namespace: even when its name is absent from this pool's universe entirely
		// it must survive the prune with its provenance intact, and it must NOT hide this pool's
		// own same-name column (the exclusion stays strict identity; the pre-post name-based
		// prune killed the reference the moment its name was absent here - the upstream leak
		// this heals).
		function test_foreignStampedReference_survives_poolPrune()
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

			var fSecond	= parseInt(secondEntry.value)
			var fFirst	= parseInt(firstEntry.value)
			var originalA = allVarsA.selectedFilterId

			try
			{
				allVarsA.selectedFilterId = fFirst	//dataset 1: its universe has TestInts but no SecondOnly
				wait(20)
				jaspForm.setOptions({
					dependentA: { value: [
						{ name: "TestInts",   types: ["scale"], filterId: fSecond },	//same name as a pool column, foreign origin
						{ name: "SecondOnly", types: ["scale"], filterId: fSecond }	//absent from this universe entirely
					] }
				})
				wait(50)

				compare(dependentA.model.termProvenanceForTest().join(","),
						"TestInts@" + fSecond + ",SecondOnly@" + fSecond,
						"both foreign references survive the prune with their own provenance (not pruned, not re-stamped)")
				verify(allVarsA.model.termProvenanceForTest().indexOf("TestInts@" + fFirst) !== -1,
						"the pool still lists its own TestInts@" + fFirst + " - the foreign same-name reference does not hide it")
				verify(allVarsA.model.termProvenanceForTest().indexOf("SecondOnly@") === -1,
						"and the pool serves no phantom of the foreign column either")
			} finally
			{
				jaspForm.setOptions({ dependentA: { value: [], types: [] } })
				allVarsA.selectedFilterId = originalA	//functions run alphabetically: don't bleed
				wait(20)
			}
		}

		// + RESOLVER + LANG: the wrapper call shown in the form carries the
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

		// the form's registry exposes the live selection HOSTS (count + list), the
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

		// the bridge reads the form's selection-option registry, so
		// dataSetSelectionOptionNames must hold exactly the live hosts' option names - the LIST
		// hosts and the TextArea host alike (moved the form hosts onto the lists;
		// the registry is host-type-agnostic). The old findChildren scan covered the statically
		// created kinds; the registry covers every lifecycle point, tested here for membership.
		function test_selectionOptionRegistry_membership()
		{
			var names = form.dataSetSelectionOptionNames()
			verify(names.indexOf("dataSetA") !== -1, "list A's option is registered, got: " + names)
			verify(names.indexOf("dataSetB") !== -1, "list B's option is registered, got: " + names)
			verify(names.indexOf("rCodeSet")  !== -1, "the TextArea host's option is registered, got: " + names)
			verify(names.indexOf("dependentA") === -1, "a plain list option is not a selection option, got: " + names)

			// The late-host case proper: a host instantiated AFTER form completion (what a ComponentsList
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
					"a host created after form completion registers immediately")

			// (chunk-4 finding #1): destroying that host again is safe NOW. The plain
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
