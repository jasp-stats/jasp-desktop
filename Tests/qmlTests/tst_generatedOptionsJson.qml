import QtTest
import QtQuick
import JASP
import JASP.Controls

// mds-plan D3.1 - the Engine-json EQUIVALENCE HARNESS, part 1: the NON-aware form.
//
// Phase 3 inverts the options pipeline: the controls' typed state is the source of truth and the
// two JSON shapes (Engine request / Storage file) are GENERATED on demand
// (AnalysisForm::createOptionsJson). The _boundValues mirror still feeds every run/save path until
// the consumer flip (D3.3/D3.5), so this pair of files pins the cutover contract: for a
// representative control matrix, `createOptionsJson(Engine)` must equal the mirror EXCEPT for the
// two intentional deltas -
//   1. the generated tree carries the transient `.filterIds` sidecar (per-term positional arrays
//      over term lists, scalar ids elsewhere; stripped at the engine boundary in D3.3), and
//   2. the generated `.meta` holds everything createMeta() stamps MINUS dataSetId/filterId (C8 -
//      provenance rides the sidecar and the terms instead).
// The Storage shape (D5.1 preview) and the aware-side provenance assertions live in
// tst_generatedOptionsJsonAware.qml (one Form per test document: the controls resolve their
// AnalysisForm through the root item's `form` property, so a second Form would register its
// controls on the wrong analysis).
//
// This file covers the FilterMenuButton world: NO dataSetSelectionOption anywhere ⟹ nothing is
// ever stamped ⟹ the generated Engine json must be byte-identical to the mirror INCLUDING the
// meta (unstamped values generate filterId-less, exactly as today - mds-plan D3.2) and must carry
// no `.filterIds` tree at all.
//
// Fixture (Tests/testqml.cpp): dataset 1 with TestInts/TestLetters/TestDoubles/TestNominal, plus
// the reopened "Second" dataset in the workspace (unused here except as noise).
//
// Beware the QML TestCase alphabetical-order trap: every test re-binds the form via setOptions at
// the start (which resets the whole tree through bindTo), so tests stay self-contained.
Item
{
	width:	1000
	height:	1600

	property alias form: plainForm

	Form
	{
		id:		plainForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsP;	name: "allVarsP" }
			AssignedVariablesList	{ id: depP;		name: "depP" }
			AssignedVariablesList	{ id: fixedP;	name: "fixedP";	allowedColumns: ["nominal", "ordinal"] }
			AssignedVariablesList
			{
				id:				rowsP
				name:			"rowsP"
				rowComponent:	CheckBox { name: "keepP" }
			}
		}

		// interaction terms: value rows are component arrays under the optionKey discipline
		AssignedVariablesList	{ id: interP;	name: "interP";	source: allVarsP;	listViewType: JASP.Interaction }

		// factor levels: the encodeThis meta family (extras feed, mds-plan C6)
		FactorLevelList			{ id: facP;		name: "factorLevelsP";	factorName: "FacP";	levelName: "LevP" }

		// variable combobox: the {value, types} single-term contract
		DropDown				{ id: comboP;	name: "comboP";		source: allVarsP; addEmptyValue: true }

		// filtered tableview: the loadFilteredData meta family
		TableView
		{
			id:			tableP
			name:		"filteredP"
			modelType:	"FilteredDataEntryModel"
			source:		"depP"
			colName:	"auditResultP"
			filter:		"TRUE"
		}

		// textareas: plain (string value) and Rcode (derived-object value, commit-on-apply)
		TextArea		{ id: textP;	name: "plainTextP" }
		TextArea
		{
			id:			codeP
			name:		"rCodeP"
			textType:	JASPControl.TextTypeRcode
		}
		// JAGS: derived {modelOriginal, model, columns, parameters} object value
		JAGSTextArea	{ id: jagsP;	name: "jagsP" }

		// components list: row objects with bound child controls
		ComponentsList
		{
			id:				compP
			name:			"compListP"
			minimumItems:	1
			rowComponent:	Row
			{
				TextField	{ name: "hypP" }
				CheckBox	{ name: "incP" }
			}
		}

		// scalars: the never-encoded, never-stamped families (mds-plan C3/M5)
		Switch				{ id: swP;		name: "swP" }
		CheckBox			{ id: cbP;		name: "cbP" }
		IntegerField		{ id: intP;		name: "intP" }
		DoubleField			{ id: dblP;		name: "dblP" }
		// a string-valued formula is the family that stamps isRCode today (TextInputBase::bindTo)
		FormulaField		{ id: formulaP;	name: "formulaP" }
		RadioButtonGroup	{ id: radP;		name: "radP"
			RadioButton		{ name: "firstP" }
			RadioButton		{ name: "secondP" }
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestGeneratedOptionsJson"

		SignalSpy
		{
			id:				spyCompleted
			target:			plainForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyCompleted.wait(3000)
			compare(spyCompleted.count, 1, "The form should have completed")
			verify(plainForm.initialized(), "the dummy analysis must have completed the setup pass (NO_DESKTOP_MODE)")
			verify(!plainForm.analysis.multiDataSetAware, "this form stays in the non-aware world")
		}

		// ---- helpers (shared shape with tst_generatedOptionsJsonAware.qml) ---------------------

		function isObj(v)
		{
			return v !== null && typeof v === "object"
		}

		// deep comparison collecting (up to `cap`) path-carrying differences
		// (JSON trees, key order agnostic)
		function diffInto(a, b, path, out, cap)
		{
			if (out.length >= cap)
				return out
			if (a === b)
				return out
			if (!isObj(a) || !isObj(b))
			{
				out.push(path + ": " + JSON.stringify(a) + " != " + JSON.stringify(b))
				return out
			}
			if (Array.isArray(a) !== Array.isArray(b))
			{
				out.push(path + ": array/object mismatch " + JSON.stringify(a) + " != " + JSON.stringify(b))
				return out
			}
			if (Array.isArray(a) && a.length !== b.length)
				out.push(path + ": length " + a.length + " != " + b.length)
			const ka = Object.keys(a), kb = Object.keys(b)
			for (const k of ka)
				if (!(k in b))
					out.push(path + "." + k + ": only in first (" + JSON.stringify(a[k]) + ")")
			for (const k of kb)
				if (!(k in a))
					out.push(path + "." + k + ": only in second (" + JSON.stringify(b[k]) + ")")
			for (const k of ka)
				if (k in b)
					diffInto(a[k], b[k], path + "." + k, out, cap)
			return out
		}

		function deepDiff(a, b, path)
		{
			return diffInto(a, b, path, [], 12).join(" | ")
		}

		// recursive removal of a member key (arrays are walked in place)
		function stripKey(tree, key)
		{
			if (Array.isArray(tree))
			{
				for (const elt of tree)
					stripKey(elt, key)
				return tree
			}
			if (isObj(tree))
			{
				delete tree[key]
				for (const k of Object.keys(tree))
					stripKey(tree[k], key)
			}
			return tree
		}

		// The tolerances of the D3.1 contract, applied ONLY inside the ".meta" subtree (a
		// dataSetId leaking into the VALUES would therefore not be hidden):
		function stripMetaProvenance(tree)
		{
			if (tree && tree[".meta"] !== undefined)
			{
				stripKey(tree[".meta"], "dataSetId")
				stripKey(tree[".meta"], "filterId")
			}
			return tree
		}

		function mirrorOf()
		{
			return JSON.parse(plainForm.analysis.boundValuesAsJson())
		}

		function generatedOf(storage)
		{
			return JSON.parse(plainForm.createOptionsJsonAsJson(storage ? 1 : 0))
		}

		// the core D3.1 assertion: generator == mirror modulo the intentional deltas; returns the
		// generated tree (sidecar put back) for follow-up assertions
		function assertEngineEqualsMirror(stage)
		{
			wait(100)   // let queued termsChanged/setBoundValue writes settle

			const mirror	= mirrorOf()
			const generated	= generatedOf(false)

			verify(mirror[".filterIds"] === undefined, stage + ": the mirror never carries .filterIds")

			const sidecar = generated[".filterIds"]
			delete generated[".filterIds"]

			stripMetaProvenance(mirror)
			stripMetaProvenance(generated)

			const d = deepDiff(mirror, generated, "")
			if (d !== "")
				console.log("EQUIVALENCE-DIFF[" + stage + "] mirror=" + JSON.stringify(mirror) + " generated=" + JSON.stringify(generated) + " diffs=" + d)
			compare(d, "",
				stage + ": createOptionsJson(Engine) differs from the _boundValues mirror beyond the intentional deltas")

			generated[".filterIds"] = sidecar
			return generated
		}

		// ---- the non-aware matrix ----------------------------------------------------------------

		function test_scalars_fields_and_radios_match_mirror()
		{
			plainForm.setOptions({
				swP: true, cbP: true, intP: 7, dblP: 2.5,
				plainTextP: "hello", radP: "secondP",
				depP: { value: ["TestInts"], types: ["scale"] }   //gives the filtered tableview its source, so its model writes the live shape (see NOTE in the header test)
			})

			const gen = assertEngineEqualsMirror("plain scalars")
			verify(gen[".filterIds"] === undefined,
				"a non-aware form generates NO .filterIds sidecar at all (unstamped values are filterId-less, exactly as today)")
			const meta = gen[".meta"] || {}
			verify(meta.swP === undefined && meta.cbP === undefined && meta.intP === undefined
				&& meta.dblP === undefined && meta.radP === undefined,
				"scalars keep generating no meta (C3/M5): " + JSON.stringify(meta))
		}

		function test_term_lists_rows_and_interactions_match_mirror()
		{
			// the lists' real optionKey members are "components" (interaction) and "variable"
			// (assigned with rowComponent) - read them live so the bound rows parse as terms
			const keyInter = interP.optionKey
			const keyRows	= rowsP.optionKey
			const rowObj1 = {}
			rowObj1[keyInter] = ["TestInts", "TestLetters"]
			const rowObj2 = {}
			rowObj2[keyRows] = "TestInts"
			rowObj2.keepP = true

			plainForm.setOptions({
				depP:	{ value: ["TestInts", "TestDoubles"], types: ["scale", "scale"] },
				fixedP:	{ value: ["TestLetters"],			types: ["nominal"] },
				interP:	{ value: [rowObj1], types: [["scale", "nominal"]], optionKey: keyInter },
				rowsP:	{ value: [rowObj2], types: ["scale"], optionKey: keyRows }
			})

			const gen = assertEngineEqualsMirror("plain term lists")
			verify(gen[".filterIds"] === undefined, "non-aware: still no sidecar (interaction/row/single-row lists included)")

			// the {value, types, optionKey} contract itself must come through:
			compare(JSON.stringify(gen.depP.value), JSON.stringify(["TestInts", "TestDoubles"]), "value array survives generation")
			verify(gen.interP.optionKey === keyInter && gen.rowsP.optionKey === keyRows, "optionKey discipline kept")
			verify(JSON.stringify(gen.interP.value).indexOf("TestInts") !== -1
				&& JSON.stringify(gen.interP.value).indexOf("TestLetters") !== -1, "both interaction components survive")
			verify(gen.rowsP.value.length === 1 && gen.rowsP.value[0].keepP === true
				&& JSON.stringify(gen.rowsP.value[0]).indexOf("TestInts") !== -1,
				"row-control children aggregate into the row object at the same path the mirror uses: " + JSON.stringify(gen.rowsP.value))

			// term-list meta: shouldEncode gates the engine-side encode and must survive minus provenance
			const meta = gen[".meta"] || {}
			verify(meta.depP && meta.depP.shouldEncode === true, "the generated meta keeps shouldEncode for encoded values")
			verify(meta.depP.dataSetId === undefined && meta.depP.filterId === undefined,
				"the generated meta carries NO provenance keys (C8)")
		}

		function test_factorlevels_combo_textarea_and_table_match_mirror()
		{
			plainForm.setOptions({
				depP:			{ value: ["TestInts"], types: ["scale"] },
				factorLevelsP:	[{ name: "FacP 1", levels: ["LevP 1", "LevP 2"] }],
				comboP:			{ value: "TestLetters", types: "nominal" },
				plainTextP:		"r ~ code",
				rCodeP:			{ modelOriginal: "summary(TestInts)" },
				jagsP:			{ modelOriginal: "x ~ dnorm(0, 1)", model: "x ~ dnorm(0, 1)", columns: [], parameters: ["x"] },
				formulaP:		"2 * TestInts",
				compListP:		[{ value: "h1", hypP: "row one", incP: true }]
			})
			wait(150)   // the Rcode syntax check and the row-control binds write through the model

			const gen = assertEngineEqualsMirror("plain rich families")
			verify(gen[".filterIds"] === undefined, "non-aware: no sidecar for the rich families either")

			const meta = gen[".meta"] || {}
			verify(Array.isArray(meta.factorLevelsP && meta.factorLevelsP.encodeThis)
				&& meta.factorLevelsP.encodeThis.indexOf("FacP 1") !== -1,
				"the generated meta keeps the factorLevelList's encodeThis array (C6 extras feed): " + JSON.stringify(meta.factorLevelsP))
			verify(meta.filteredP && meta.filteredP.loadFilteredData && meta.filteredP.loadFilteredData.column === "auditResultP",
				"the generated meta keeps loadFilteredData {filter, column} for the filtered tableview: " + JSON.stringify(meta.filteredP))
			// isRCode: TextInputBase::bindTo stamps it for a string-valued FormulaField. (The
			// R-code TextArea does NOT get an isRCode meta even in the mirror today - the rlang
			// companion overrides checkSyntax without calling setIsRCode; the equivalence check
			// above pins that whatever the mirror holds, the generator reproduces. Flagged for
			// the D3.4 handoff as a createMeta quirk the generator inherits, not introduces.)
			verify(meta.formulaP && meta.formulaP.isRCode === true,
				"the generated meta keeps isRCode for the string-valued formula field: " + JSON.stringify(meta.formulaP))
			verify(meta.rCodeP === undefined && mirrorOf()[".meta"].rCodeP === undefined,
				"R-code textarea stays meta-free on BOTH sides (today's rlang behaviour, pinned by the equivalence check)")
			verify(meta.comboP && meta.comboP.shouldEncode === true, "the variable combobox generates its shouldEncode meta")
			verify(gen.jagsP && gen.jagsP.modelOriginal === "x ~ dnorm(0, 1)" && Array.isArray(gen.jagsP.parameters),
				"the JAGS textarea generates its derived {modelOriginal, model, columns, parameters} object: " + JSON.stringify(gen.jagsP))
		}

		function test_storage_shape_has_no_send_trees()
		{
			plainForm.setOptions({
				depP:			{ value: ["TestInts"], types: ["scale"] },
				factorLevelsP:	[{ name: "FacP 1", levels: ["LevP 1", "LevP 2"] }],
				swP:			true
			})

			const storage = generatedOf(true)
			verify(storage[".meta"] === undefined, "Storage carries no .meta tree (send-time artifact, C8/C11)")
			verify(storage[".filterIds"] === undefined, "Storage carries no sidecar tree")
			verify(storage.depP.filterIds === undefined,
				"non-aware terms have no provenance to embed, so Storage stays exactly the value tree")

			const engineValues = generatedOf(false)
			delete engineValues[".meta"]
			delete engineValues[".filterIds"]
			compare(deepDiff(engineValues, storage, ""), "", "Storage == Engine values (non-aware: nothing to embed)")
		}
	}
}
