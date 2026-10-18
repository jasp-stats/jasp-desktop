import QtTest
import QtQuick
import QtQuick.Controls	as QTC
import JASP.Controls

//NOTE: QtQuick.Controls is imported ALIASED on purpose: JASP.Controls registers its own
//`TextArea` (an option control), and the unaliased import would shadow the plain
//QtQuick.Controls TextArea that the production R editors (FilterWindow.qml, ComputeColumnWindow.qml)
//actually insert into - and that QualifiedColumnPicker/Validator target.

// mds-plan D7.1/D7.2: the editor-side surface of the qualified column-reference grammar.
// The three R editors (FilterWindow's filterEdit, ComputeColumnWindow's computeColumnEdit,
// ComputeDataSetPanel's codeEdit) share two components:
//
//   - QualifiedColumnPicker: outward-reference insertion fed by
//     Workspace::qualifiedColumnCandidates(contextFilterId). The context is the editor's
//     bare-name fallback namespace per plan §4 (the filter editor: the EDITED filter, fixed -
//     there is no fallback-selection UI; the compute editors: the selected compute/input
//     filter). Insertion puts the rendered surface form in VERBATIM at the cursor - stored
//     code stays raw (column.cpp:547), encoding happens only at run, engine-side (D7.3).
//   - QualifiedColumnValidator: debounced live validation = the D7.3 scanner's first typed
//     error (Workspace::qualifiedRefErrorInCode). Non-blocking: bare names are never errors
//     here, and an unresolvable reference warns inline while the run itself still fails
//     loudly at the engine (D7.5's surface).
//
// The components are driven directly against the harness fixture (Tests/testqml.cpp:
// dataset 1 + "Second", REOPENED from the database so every filter id is a final stored
// rowid) with the same context resolution the editors use
// (Workspace::filterIdByName(...)). "ReopenMe" (a named filter of "Second") plays the
// filter editor's EDITED filter, and "Second"'s default filter plays the compute editors'
// SELECTED INPUT FILTER - the plan §4 case where the fallback namespace belongs to ANOTHER
// dataset. The picker-cut/output-cut owner refinement (D7.0a NOTE): the picker may be
// scoped to own-dataset columns, but what it emits for an OUTWARD pick is always the full
// qualified surface form - asserted verbatim below. This file only READS the workspace, so
// it cannot disturb the tests that alphabetically surround it.
Item
{
	width:	800
	height:	600

	//The filter editor's context (D7.1): the edited filter's id.
	readonly property int filterContextId:
	{
		if(!workspace)
			return -1

		for(const candidate of workspace.qualifiedColumnCandidates(-1))
			if(candidate.dataSetTitle === "Second" && candidate.filterName === "ReopenMe")
				return candidate.filterId

		return -1
	}

	//The compute editors' context analogue (D7.2): the SELECTED INPUT FILTER - "Second"'s
	//default filter here, another dataset's filter, exactly the
	//"computed dataset | selected input filter" row of plan §4.
	readonly property int inputContextId:
	{
		if(!workspace)
			return -1

		for(const candidate of workspace.qualifiedColumnCandidates(-1))
			if(candidate.dataSetTitle === "Second" && candidate.filterName === "DEFAULT_FILTER")
				return candidate.filterId

		return -1
	}

	//NOTE: the components' `workspace` property must NOT be bound as `workspace: workspace`:
	//the element's own property shadows the context property in that binding, leaving it
	//null forever. Route the context property through this alias instead.
	readonly property var	ws:				workspace

	QTC.TextArea
	{
		id:				editor
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	parent.right
		height:			120
		wrapMode:		QTC.TextArea.WrapAtWordBoundaryOrAnywhere
	}

	QTC.TextArea
	{
		id:				validatedEditor
		anchors.top:	editor.bottom
		anchors.left:	parent.left
		anchors.right:	parent.right
		height:			120
		wrapMode:		QTC.TextArea.WrapAtWordBoundaryOrAnywhere
	}

	QualifiedColumnPicker
	{
		id:					filterEditorPicker
		target:				editor
		workspace:			ws
		contextFilterId:	filterContextId
	}

	QualifiedColumnPicker
	{
		id:					inputFilterPicker
		target:				editor
		workspace:			ws
		contextFilterId:	inputContextId
	}

	QualifiedColumnValidator
	{
		id:					validator
		workspace:			ws
		target:				validatedEditor
		debounceInterval:	20
	}

	function findCandidate(candidates, dataSetTitle, filterName, columnName)
	{
		for(const candidate of candidates)
			if((!dataSetTitle || candidate.dataSetTitle === dataSetTitle)
				&& (!filterName  || candidate.filterName  === filterName)
				&& (!columnName  || candidate.columnName  === columnName))
				return candidate

		return null
	}

	TestCase
	{
		name: "QualifiedColumnRefs"

		//The catalog against the FILTER EDITOR's context (D7.1): only the edited filter's own
		//columns are bare (that namespace is the bare-name fallback BY DEFINITION, C13 - no
		//fallback-selection UI), every other slice comes out as the minimal qualified surface
		//form - including the OTHER filters of the edited filter's own dataset (bare means the
		//edited filter's namespace, not "own dataset").
		function test_candidates_filter_context_bare_and_outward_qualified()
		{
			verify(workspace)
			verify(filterContextId >= 0)

			const candidates = workspace.qualifiedColumnCandidates(filterContextId)
			verify(candidates.length > 0)

			for(const candidate of candidates)
				compare(candidate.isBare, candidate.filterId === filterContextId)

			//The edited filter's own columns, VERBATIM what insertion puts into the editor:
			const own = findCandidate(candidates, "Second", "ReopenMe", "SecondOnly")
			verify(own)
			compare(own.code, "SecondOnly")
			compare(own.isBare, true)

			//Another filter of the SAME dataset (its default filter) is an outward ref:
			const secondDefault = findCandidate(candidates, "Second", "DEFAULT_FILTER", "TestInts")
			verify(secondDefault)
			compare(secondDefault.code, '"Second"."TestInts"')		//default filter: dataset-title form

			//A different dataset's filter, unique name: the 2-segment form. ("TestInts" exists
			//in BOTH datasets, so the dataset title is needed to point at the other one.)
			const dataSet1Title = candidates.find((candidate) => candidate.dataSetTitle !== "Second").dataSetTitle
			const dataSet1Default = findCandidate(candidates, dataSet1Title, "DEFAULT_FILTER", "TestInts")
			verify(dataSet1Default)
			compare(dataSet1Default.code, '"' + dataSet1Title + '"."TestInts"')
		}

		//The catalog against the COMPUTE EDITORS' context (D7.2): the selected input filter is
		//THE bare-name fallback namespace - even though it belongs to ANOTHER dataset. Its
		//columns come out bare; everything else (including the editor's own dataset) qualified.
		function test_candidates_input_filter_context_is_the_fallback_namespace()
		{
			verify(inputContextId >= 0)
			verify(inputContextId !== filterContextId)	//the fixture's input filter is not the edited filter

			const candidates = workspace.qualifiedColumnCandidates(inputContextId)

			for(const candidate of candidates)
				compare(candidate.isBare, candidate.filterId === inputContextId)

			//Second's columns are bare under this context (they resolve unqualified at run time
			//through the selected input filter, D7.3's runComputeDataSet):
			const secondInts = findCandidate(candidates, "Second", "DEFAULT_FILTER", "TestInts")
			verify(secondInts)
			compare(secondInts.code, "TestInts")
			compare(secondInts.isBare, true)

			//And dataset 1's columns are now the OUTWARD ones:
			const dataSet1 = findCandidate(candidates, null, "DEFAULT_FILTER", "TestLetters")
			verify(dataSet1)
			compare(dataSet1.code, '"' + dataSet1.dataSetTitle + '"."TestLetters"')
		}

		//Insertion: the rendered surface form goes in VERBATIM at the cursor and the caret
		//stays behind it. An OUTWARD pick is always the full qualified surface form - the
		//picker cut (v1 scope) limits what is OFFERED, never what is EMITTED (D7.0a owner
		//refinement: picker cut != output cut).
		function test_insertion_puts_rendered_form_verbatim_at_cursor()
		{
			filterEditorPicker.openForInsertion()
			try
			{
				verify(filterEditorPicker.opened)
				verify(filterEditorPicker.candidates.length > 0)

				editor.text = "contGamma > "
				editor.cursorPosition = editor.text.length

				const outward = findCandidate(filterEditorPicker.candidates, "Second", "DEFAULT_FILTER", "TestInts")
				verify(outward)
				verify(outward.code.indexOf('"Second".') === 0)	//outward pick: qualified, never bare
				compare(outward.code, outward.code.trim())		//no padding around the surface form

				verify(filterEditorPicker.insertCandidate(outward))
				compare(editor.text, "contGamma > " + outward.code)
				compare(editor.cursorPosition, editor.text.length)

				//The context's own columns insert as the plain bare name (today's own-dataset
				//code stays byte-identical, backward compatibility):
				editor.text = ""
				editor.cursorPosition = 0
				const bare = findCandidate(filterEditorPicker.candidates, "Second", "ReopenMe", "SecondOnly")
				verify(bare)
				compare(bare.code, "SecondOnly")
				verify(filterEditorPicker.insertCandidate(bare))
				compare(editor.text, "SecondOnly")
			}
			finally
			{
				filterEditorPicker.close()
			}
		}

		//Autocomplete: the search field filters the catalog and Enter inserts the selected hit
		//(the type-to-filter path of the editors' "insert reference" button).
		function test_picker_search_and_enter_inserts_selected()
		{
			inputFilterPicker.openForInsertion()
			try
			{
				verify(inputFilterPicker.opened)

				const searchField = inputFilterPicker.contentItem.children.find(
										(child) => child.objectName === "qualifiedColumnPickerSearchField")
				verify(searchField)

				editor.text = ""
				editor.cursorPosition = 0

				//Searching by filter name narrows the catalog to that slice's columns; every
				//hit carries the needle, and the FIRST hit inserts through Enter:
				searchField.text = "ReopenMe"
				verify(inputFilterPicker.visibleCandidates.length >= 1)
				for(const candidate of inputFilterPicker.visibleCandidates)
					verify(candidate.label.toLowerCase().indexOf("reopenme") >= 0
						|| candidate.code.toLowerCase().indexOf("reopenme") >= 0)

				inputFilterPicker.contentItem.children.find((child) => child.objectName === "qualifiedColumnRefsList").currentIndex = 0
				searchField.insertSelected()
				compare(editor.text, '"ReopenMe"."SecondOnly"')	//outward pick: the qualified surface form, verbatim
				compare(inputFilterPicker.opened, false)		//insertion closes the popup
			}
			finally
			{
				inputFilterPicker.close()
			}
		}

		//Validation: clean code and bare names are silent; an unresolvable reference reports
		//the FIRST typed error, debounced (D7.3's scanner, editor-side).
		function test_validator_clean_bare_and_debounced_first_error()
		{
			compare(validator.error, "")	//initial (empty) code is clean

			validatedEditor.text = "contGamma > 3 & TestInts < 10"
			compare(validator.error, "")	//debounce has not fired yet (no event loop turn)

			wait(validator.debounceInterval * 4)
			compare(validator.error, "")	//bare names are never errors: their fallback is the
											//editor's context, codified at the editor (plan §4)

			validatedEditor.text = 'contGamma > mean("Second"."TestInts")'
			wait(validator.debounceInterval * 4)
			compare(validator.error, "")	//a resolvable outward reference is clean

			//Two unresolvable refs: only the FIRST is reported (the scanner's firstError hook,
			//the same message the engine fails the run with):
			validatedEditor.text = 'contGamma > mean("Zzz1"."x") + mean("Zzz2"."y")'
			wait(validator.debounceInterval * 4)
			verify(validator.error.indexOf("Zzz1") >= 0)
			verify(validator.error.indexOf("Zzz2") < 0)

			//Suspends without disconnecting:
			validator.active = false		//onActiveChanged validates immediately: error clears
			compare(validator.error, "")
			validatedEditor.text = 'mean("Zzz3"."x")'	//inactive: no debounce restart
			wait(validator.debounceInterval * 4)
			compare(validator.error, "")
			validator.active = true			//re-activating validates the current text
			verify(validator.error.indexOf("Zzz3") >= 0)

			validatedEditor.text = ""
			wait(validator.debounceInterval * 4)	//restore: leave nothing dirty behind
			compare(validator.error, "")
		}
	}
}
