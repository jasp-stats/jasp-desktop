import QtTest
import QtQuick
import JASP
import JASP.Controls

// mds-plan D3.1/D3.2 - the Engine-json EQUIVALENCE HARNESS, part 2: the AWARE form.
//
// Same contract as tst_generatedOptionsJson.qml (read that header first), but here the analysis is
// multiDataSetAware: two selecting AvailableVariablesLists (per-list selection, mds-plan C10/D2.2)
// plus a TextArea host, so different lists resolve through DIFFERENT filters - and the transient
// `.filterIds` sidecar (D3.2) must carry that provenance per term where the mirror still carries
// it per option in `.meta` (until D3.4 deletes the stamping). The flagship: "TestInts" selected in
// TWO lists through TWO filters of the same dataset generates byte-identical VALUES but
// distinguishable provenance.
//
// Fixture (Tests/testqml.cpp): dataset 1 (TestInts/...) plus "Second" (TestInts with different
// values + SecondOnly) with extra filters, reopened from the database - so at least two filters of
// "Second" carry a column named TestInts.
Item
{
	width:	1000
	height:	1600

	property alias form: awareForm

	Form
	{
		id:		awareForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsA;	name: "allVarsA";	dataSetSelectionOption:	"dataSetA" }
			// resolves through allVarsA's selection (related available list, mds-plan D2.2)
			AssignedVariablesList	{ id: depA;		name: "depA" }
			// a variable combobox served by the same host
			DropDown				{ id: comboA;	name: "comboA";	source: allVarsA; addEmptyValue: true }
		}

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsB;	name: "allVarsB";	dataSetSelectionOption:	"dataSetB" }
			// the flagship partner of depA: same column name, bound through THIS host's selection
			AssignedVariablesList	{ id: depB;		name: "depB" }
			AssignedVariablesList	{ id: fixedB;		name: "fixedB";	allowedColumns: ["nominal", "ordinal"] }
		}

		// A list with no host anywhere: today it is stamped through the FORM's VariableInfo
		// (analysis filter else shown filter) and the sidecar must carry that same id (C13
		// host-bound fallback), so an aware run never loses the option-level provenance.
		AssignedVariablesList	{ id: aloneA;	name: "aloneA" }

		// a factor level list on the aware form: always-encodeValue, encodeThis + stamp
		FactorLevelList			{ id: facA;	name: "factorLevelsA";	factorName: "FacA";	levelName: "LevA" }

		// the TextArea host (own selection, exactly like rCodeArea in tst_dataSetSelectionVariablesList)
		TextArea
		{
			id:						codeB
			name:					"rCodeBoxB"
			dataSetSelectionOption:	"codeSet"
		}

		Switch				{ id: swA;	name: "swA" }
	}

	TestCase
	{
		id:		testCase
		name:	"TestGeneratedOptionsJsonAware"

		SignalSpy
		{
			id:				spyCompleted
			target:			awareForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyCompleted.wait(3000)
			compare(spyCompleted.count, 1, "The form should have completed")
			verify(awareForm.initialized(), "the dummy analysis must have completed the setup pass (NO_DESKTOP_MODE)")
		}

		// ---- helpers (shared shape with tst_generatedOptionsJson.qml) --------------------------

		function isObj(v)
		{
			return v !== null && typeof v === "object"
		}

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
			return JSON.parse(awareForm.analysis.boundValuesAsJson())
		}

		function generatedOf(storage)
		{
			return JSON.parse(awareForm.createOptionsJsonAsJson(storage ? 1 : 0))
		}

		function collectStamps(node, path, out)
		{
			if (Array.isArray(node))
			{
				for (const elt of node)
					collectStamps(elt, path, out)
				return out
			}
			if (isObj(node))
			{
				if (typeof node.filterId === "number")
					out.push({ path: path, filterId: node.filterId, dataSetId: node.dataSetId })
				for (const k of Object.keys(node))
					collectStamps(node[k], path === "" ? k : path + "." + k, out)
			}
			return out
		}

		function sidecarAt(sidecar, path)
		{
			let node = sidecar
			for (const part of path.split("."))
			{
				if (!isObj(node))
					return undefined
				node = node[part]
			}
			return node
		}

		function assertEngineEqualsMirror(stage)
		{
			wait(100)

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

		// ---- binding --------------------------------------------------------------------------------

		function _secondFilterIds()
		{
			const ids = []
			for (const entry of allVarsA.filterSelectionValues)
				if (String(entry.label).indexOf("Second - ") === 0)
					ids.push(parseInt(entry.value))
			return ids
		}

		// Binds the whole aware form onto two DISTINCT filters fA != fB of dataset "Second"
		// (plus the codeB host onto fA) and returns them.
		function _bindAwareForm()
		{
			const secondFilters = _secondFilterIds()
			verify(secondFilters.length >= 2, "fixture must offer at least two filters of dataset 'Second', got " + secondFilters.length)

			const fA = secondFilters[0], fB = secondFilters[1]
			verify(fA !== fB, "the two 'Second' filters must be distinct")

			awareForm.setOptions({
				dataSetA:	String(fA),
				dataSetB:	String(fB),
				codeSet:	String(fA),
				depA:		{ value: ["TestInts"], types: ["scale"] },
				depB:		{ value: ["TestInts"], types: ["scale"] },
				fixedB:		{ value: ["SecondOnly"], types: ["nominal"] },
				aloneA:		{ value: ["TestDoubles"], types: ["scale"] },
				comboA:		{ value: "TestInts", types: "scale" },
				rCodeBoxB:	"y ~ TestInts",
				swA:		true
			})
			return { fA: fA, fB: fB }
		}

		function test_aware_engine_matches_mirror()
		{
			_bindAwareForm()

			assertEngineEqualsMirror("aware form")

			// C8 in the other direction, asserted on the RAW generated tree (the equivalence above
			// stripped both sides symmetrically): the generator must not emit a single provenance
			// key, while every other meta key of the mirror's stamped nodes survives.
			const rawGenerated	= generatedOf(false)
			const genStamps		= collectStamps(rawGenerated[".meta"] || {}, "", [])
			compare(genStamps.length, 0, "generated .meta holds NO dataSetId/filterId keys (C8): " + JSON.stringify(genStamps))

			const mirrorMeta = mirrorOf()[".meta"] || {}
			const mirrorStamps = collectStamps(mirrorMeta, "", [])
			verify(mirrorStamps.length >= 5, "the aware mirror stamps several options, got " + JSON.stringify(mirrorStamps))
			for (const stamp of mirrorStamps)
			{
				const genNode = sidecarAt(rawGenerated[".meta"], stamp.path)
				verify(genNode !== undefined, "the generated meta keeps a node for stamped option '" + stamp.path + "'")
				verify(genNode.shouldEncode === true, "shouldEncode survives into the generated (send-time) meta of '" + stamp.path + "'")
				if (stamp.path === "factorLevelsA")
					verify(Array.isArray(genNode.encodeThis), "encodeThis survives into the generated meta (C6)")
			}
		}

		function test_aware_sidecar_covers_every_mirrored_stamp()
		{
			_bindAwareForm()
			wait(100)

			const gen		 = generatedOf(false)
			const sidecar	 = gen[".filterIds"]
			verify(sidecar !== undefined, "the aware form generates the transient .filterIds tree")

			const stamps = collectStamps(mirrorOf()[".meta"] || {}, "", [])
			for (const stamp of stamps)
			{
				const node = sidecarAt(sidecar, stamp.path)
				verify(node !== undefined, "sidecar has a node for stamped option '" + stamp.path + "'")
				if (Array.isArray(node))
				{
					for (const id of node)
						verify(id === null || id >= 0, "positional sidecar entries are int (stamped) or null (unqualified): " + JSON.stringify(node))
					// every host-fallback position must agree with the mirror's option-level stamp;
					// a term may only deviate through its own typed filterId (flagship test):
					verify(node.indexOf(stamp.filterId) !== -1,
						"positional sidecar for '" + stamp.path + "' contains the mirrored stamp id " + stamp.filterId + ": " + JSON.stringify(node))
				}
				else
					compare(node, stamp.filterId, "scalar sidecar id == mirror stamp for '" + stamp.path + "'")
			}

			// and the sidecar stays EMPTY on an analysis whose values are not column-bearing:
			verify(sidecar.swA === undefined, "scalars generate no sidecar node (C3/M5)")
			verify(sidecar.dataSetA === undefined && sidecar.dataSetB === undefined && sidecar.codeSet === undefined,
				"the selection options generate no sidecar (their value IS the filter id)")
		}

		function test_aware_sidecar_distinguishes_same_name_in_two_lists()
		{
			// The flagship (C9/C11): "TestInts" selected through TWO different filters of the same
			// dataset in ONE form. The values are byte-identical, the mirror stamps the two options
			// with their host filters - and the sidecar must carry the distinction per term, which
			// is what D3.3's slice-queue build and D3.7's per-term encoding consume.
			const sel = _bindAwareForm()
			wait(100)

			const gen		= generatedOf(false)
			const sidecar	= gen[".filterIds"]
			verify(sidecar !== undefined, "aware sidecar present")

			const depAIds = sidecarAt(sidecar, "depA")
			const depBIds = sidecarAt(sidecar, "depB")
			verify(Array.isArray(depAIds) && depAIds.length === 1, "depA sidecar is a one-entry positional array")
			verify(Array.isArray(depBIds) && depBIds.length === 1, "depB sidecar is a one-entry positional array")
			compare(depAIds[0], sel.fA, "depA's term carries ITS host filter id")
			compare(depBIds[0], sel.fB, "depB's term carries ITS host filter id")
			verify(depAIds[0] !== depBIds[0], "the same column name in two lists with different selections generates DISTINGUISHABLE terms")

			// the option VALUES are identical - the distinction is provenance-only:
			compare(deepDiff(gen.depA, gen.depB, ""), "",
				"the two lists' term values/types are byte-identical; only the sidecar separates them")

			// the combo under host A resolves to A's filter (the selected model term's id):
			compare(sidecarAt(sidecar, "comboA"), sel.fA, "the variable combobox's scalar sidecar follows its host's selection")
			// the factor level list has no host: it falls back to the ANALYSIS-WIDE filter id
			// (today's form-varInfo stamp), distinct from both host selections:
			const facNode = sidecarAt(sidecar, "factorLevelsA")
			verify(typeof facNode === "number" && facNode >= 0 && facNode !== sel.fA && facNode !== sel.fB,
				"the hostless aware factorLevelList gets the analysis-wide fallback id, not a host id: " + JSON.stringify(facNode))
			// and a list that carries no columns at all (aloneA has no source/host, so JASP does
			// not treat it as variable-bearing) generates neither meta nor sidecar - as today:
			verify(sidecar.aloneA === undefined, "a non-column-bearing list generates no sidecar (mirror stays unstamped too)")
		}

		function test_aware_midswitch_sidecar_follows_without_stale_ids()
		{
			const sel = _bindAwareForm()
			wait(100)

			// Mid-session switch of host A onto ANOTHER filter of the same dataset rewrites no
			// value (D2.5's restamp class): the sidecar must move with the typed terms and leave
			// the abandoned filter out of the tree entirely (no phantom-slice input, C3/M5).
			allVarsA.selectedFilterId = sel.fB
			wait(100)

			const gen		 = generatedOf(false)
			const sidecar	 = gen[".filterIds"]
			compare(sidecarAt(sidecar, "depA")[0], sel.fB, "depA's sidecar followed the host switch")

			const seen = []
			;(function walk(node)
			{
				if (Array.isArray(node)) { for (const e of node) walk(e); return }
				if (isObj(node)) { for (const k of Object.keys(node)) walk(node[k]); return }
				if (typeof node === "number") seen.push(node)
			})(sidecar)
			verify(seen.indexOf(sel.fA) === -1, "the abandoned filter id is gone from the whole sidecar: " + JSON.stringify(seen))

			// generator still == mirror after the switch (the .meta restamp half moved too):
			assertEngineEqualsMirror("aware form after mid-session host switch")

			allVarsA.selectedFilterId = sel.fA   //restore for alphabetical siblings
			wait(40)
		}

		// ---- pre-attach parking (mds-plan D3.5/C12) ---------------------------------------------

		function test_pending_json_applied_once_and_stamps_never_read()
		{
			// Options may arrive as PENDING JSON before/while the controls bind (file load parks
			// them via setBoundValues, setAnalysisUp applies them exactly once through the
			// controls). From the D3.3 flip on, NOTHING reads pending JSON for behaviour: bindTo
			// only walks control names, the mirror is cleared and rebuilt by the controls, and
			// the generated Engine tree (values + sidecar) comes from component state - so a
			// wedged-in ".meta" stamp in the pending tree is inert input that must vanish with
			// the parking itself.
			_bindAwareForm()
			wait(100)

			// re-bind (park + attach) with LYING provenance stamps in the pending .meta:
			const sel = _bindAwareForm()
			awareForm.setOptions({
				dataSetA:	String(sel.fA),
				dataSetB:	String(sel.fB),
				codeSet:	String(sel.fA),
				depA:		{ value: ["TestInts"], types: ["scale"] },
				depB:		{ value: ["TestInts"], types: ["scale"] },
				fixedB:		{ value: ["SecondOnly"], types: ["nominal"] },
				comboA:		{ value: "TestInts", types: "scale" },
				".meta":	{ depA: { shouldEncode: true, dataSetId: 99, filterId: 4242 } }
			})
			wait(150)

			// state present: the pending VALUES were applied through the controls once...
			const gen = generatedOf(false)
			compare(gen.depA.value.length, 1, "pending option applied exactly once (not duplicated)")
			compare(gen.depA.value[0], "TestInts", "pending value present after attach")

			// ...and the pending STAMPS were never read: the id appears nowhere - neither in the
			// generated Engine tree (sidecar comes from the bound controls)...
			const genText = JSON.stringify(gen)
			verify(genText.indexOf("4242") === -1, "pending-stamped filterId never read into behaviour: " + genText.slice(0, 500))
			verify(genText.indexOf("\"99\"") === -1 && !JSON.stringify(gen[".filterIds"]).match(/(^|[^0-9])99([^0-9]|$)/),
				"no ghost dataSetId from the pending tree either")
			// ...nor anywhere in the mirror (attach cleared the parking; the controls rewrote it
			// with their OWN host stamps):
			verify(JSON.stringify(mirrorOf()).indexOf("4242") === -1, "the mirror after attach carries no pending stamps")

			// the sidecar carries the real host provenance the controls resolved:
			compare(sidecarAt(gen[".filterIds"], "depA")[0], sel.fA, "depA's sidecar follows its bound host, not the pending stamp")

			assertEngineEqualsMirror("aware form after pending-stamp re-attach")
		}

		// ---- Storage shape (D5.1 preview) --------------------------------------------------------

		function test_storage_roundtrips_through_the_json_to_state_appliers()
		{
			// C12/D3.5: the JSON->state appliers (bindTo path) stay in charge until Phase 5 changes
			// the file format, so a generated Storage tree must re-enter the form and regenerate
			// identically (the embedded filterIds member is ignored on parse).
			_bindAwareForm()
			wait(100)
			const first = JSON.stringify(generatedOf(true))

			awareForm.setOptions(JSON.parse(first))
			wait(150)

			const again = JSON.stringify(generatedOf(true))
			compare(again, first, "a Storage tree round-trips through setOptions -> rebind -> regenerate")

			// and the Engine shape stays mirror-equivalent after the round-trip too:
			assertEngineEqualsMirror("aware form after Storage round-trip")
		}

		function test_storage_shape_is_clean_typed_state()
		{
			_bindAwareForm()
			wait(100)

			const engine	= generatedOf(false)
			const storage	= generatedOf(true)

			verify(storage[".meta"] === undefined, "Storage carries no .meta tree (it is a send-time artifact, C8/C11)")
			verify(storage[".filterIds"] === undefined, "Storage carries no sidecar tree - provenance rides INSIDE the term options")

			verify(storage.depA.filterIds !== undefined && storage.depA.filterIds[0] === sidecarAt(engine[".filterIds"], "depA")[0],
				"Storage terms keep the same provenance the Engine sidecar carries")
			verify(storage.fixedB.filterIds !== undefined, "every aware term list embeds its positional filterIds in Storage")
			verify(storage.swA === true && storage.dataSetA !== undefined, "scalars and selection options ride along unchanged")

			// the value tree is identical to the Engine values once the embedded filterIds come out:
			const engineValues = generatedOf(false)
			delete engineValues[".meta"]
			delete engineValues[".filterIds"]
			stripKey(storage, "filterIds")
			compare(deepDiff(engineValues, storage, ""), "", "Storage == Engine values once the embedded filterIds come out")
		}
	}
}
