import QtTest
import QtQuick
import JASP
import JASP.Controls

// mds-plan D3.1/D3.2/D3.4 - the Engine-json GOLDEN HARNESS, part 2: the AWARE form.
//
// Same contract as tst_generatedOptionsJson.qml (read that header first - the mirror is deleted,
// the expectations are its frozen bytes), but here the analysis is multiDataSetAware: two
// selecting AvailableVariablesLists (per-list selection, mds-plan C10/D2.2) plus a TextArea host,
// so different lists resolve through DIFFERENT filters. The golden mirror bytes were captured with
// their dataSetId/filterId stamp keys stripped (those were the harness' one documented tolerance,
// and they never shipped: C8/C15) - the provenance they carried is asserted on the transient
// `.filterIds` sidecar instead, against the LIVE host selections (never against captured ids, so
// the file stays fixture-order-robust). The flagship: "TestInts" selected in TWO lists through
// TWO filters of the same dataset generates byte-identical VALUES but distinguishable provenance.
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

		// The frozen expected trees, captured from the deleted mirror at the D3.4 deletion
		// commit by this harness while it still asserted generated == mirror (so they are the
		// authoritative old bytes), with the dead stamp keys removed - the harness' one documented
		// tolerance. goldenAware is the state after _bindAwareForm(); the mid-session switch and
		// the Storage round-trip reach the same bytes (same VALUES; the moved ids lived only in the
		// stripped keys / the sidecar, asserted live below). goldenReattach is the pending-stamp
		// re-attach bind, which omits aloneA/rCodeBoxB and therefore holds their defaults.
		readonly property string goldenAware: `{".meta":{"comboA":{"shouldEncode":true},"depA":{"shouldEncode":true},"depB":{"shouldEncode":true},"factorLevelsA":{"encodeThis":["FacA 1","LevA 1","LevA 2"],"shouldEncode":true},"fixedB":{"shouldEncode":true}},"aloneA":{"types":["scale"],"value":["TestDoubles"]},"codeSet":"2","comboA":{"types":"ordinal","value":"TestInts"},"dataSetA":"2","dataSetB":"3","depA":{"types":["ordinal"],"value":["TestInts"]},"depB":{"types":["ordinal"],"value":["TestInts"]},"factorLevelsA":[{"levels":["LevA 1","LevA 2"],"name":"FacA 1"}],"fixedB":{"types":["nominal"],"value":["SecondOnly"]},"plotHeight":320,"plotWidth":480,"rCodeBoxB":"y ~ TestInts","swA":true}`
		readonly property string goldenReattach: `{".meta":{"comboA":{"shouldEncode":true},"depA":{"shouldEncode":true},"depB":{"shouldEncode":true},"factorLevelsA":{"encodeThis":["FacA 1","LevA 1","LevA 2"],"shouldEncode":true},"fixedB":{"shouldEncode":true}},"aloneA":{"types":[],"value":[]},"codeSet":"2","comboA":{"types":"ordinal","value":"TestInts"},"dataSetA":"2","dataSetB":"3","depA":{"types":["ordinal"],"value":["TestInts"]},"depB":{"types":["ordinal"],"value":["TestInts"]},"factorLevelsA":[{"levels":["LevA 1","LevA 2"],"name":"FacA 1"}],"fixedB":{"types":["nominal"],"value":["SecondOnly"]},"plotHeight":320,"plotWidth":480,"rCodeBoxB":"","swA":true}`

		// the options that the mirror stamped in the bound aware state (paths, no ids: id
		// equality is pinned live by the flagship/coverage tests; presence + shape here)
		readonly property var stampedPaths: ["comboA", "depA", "depB", "factorLevelsA", "fixedB"]

		function assertEngineEqualsGolden(stage, golden)
		{
			wait(100)

			const generated	= generatedOf(false)

			const expected = JSON.parse(golden)
			verify(expected[".filterIds"] === undefined, stage + ": the golden (mirror bytes) never carries .filterIds")

			const sidecar = generated[".filterIds"]
			delete generated[".filterIds"]

			// the golden was captured with the stamp keys stripped (the mirror's, dead since the
			// D3.3 flip); strip the generated side symmetrically - and the raw-tree C8 audit in
			// test_aware_engine_matches_mirror_bytes proves the generated side has nothing to strip.
			stripMetaProvenance(generated)

			const d = deepDiff(expected, generated, "")
			if (d !== "")
				console.log("GOLDEN-DIFF[" + stage + "] golden=" + golden + " generated=" + JSON.stringify(generated) + " diffs=" + d)
			compare(d, "",
				stage + ": createOptionsJson(Engine) differs from the golden mirror bytes frozen at the D3.4 deletion")

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

		function test_aware_engine_matches_mirror_bytes()
		{
			_bindAwareForm()

			assertEngineEqualsGolden("aware form", goldenAware)

			// C8 in the other direction, asserted on the RAW generated tree (the golden comparison
			// stripped both sides symmetrically): the generator must not emit a single provenance
			// key, while every other meta key of the options the mirror stamped survives.
			const rawGenerated	= generatedOf(false)
			const genStamps		= collectStamps(rawGenerated[".meta"] || {}, "", [])
			compare(genStamps.length, 0, "generated .meta holds NO dataSetId/filterId keys (C8): " + JSON.stringify(genStamps))

			compare(stampedPaths.length, 5, "the frozen golden state stamped several options")	// the mirror-side guard of the golden capture
			for (const stampPath of stampedPaths)
			{
				const genNode = sidecarAt(rawGenerated[".meta"], stampPath)
				verify(genNode !== undefined, "the generated meta keeps a node for the golden-stamped option '" + stampPath + "'")
				verify(genNode.shouldEncode === true, "shouldEncode survives into the generated (send-time) meta of '" + stampPath + "'")
				if (stampPath === "factorLevelsA")
					verify(Array.isArray(genNode.encodeThis), "encodeThis survives into the generated meta (C6)")
			}
		}

		function test_aware_sidecar_covers_every_golden_stamp()
		{
			const sel = _bindAwareForm()
			wait(100)

			const gen		 = generatedOf(false)
			const sidecar	 = gen[".filterIds"]
			verify(sidecar !== undefined, "the aware form generates the transient .filterIds tree")

			// Presence/shape at every path the golden mirror state stamped; the id VALUES are
			// live-state facts pinned against the LIVE hosts (the option-level stamp the mirror
			// wrote for fixedB was host B's filter - the sidecar's live truth is the same id read
			// through the host, not a frozen number) and by the flagship/midswitch tests below.
			for (const stampPath of stampedPaths)
			{
				const node = sidecarAt(sidecar, stampPath)
				verify(node !== undefined, "sidecar has a node for the golden-stamped option '" + stampPath + "'")
				if (Array.isArray(node))
				{
					for (const id of node)
						verify(id === null || id >= 0, "positional sidecar entries are int (stamped) or null (unqualified): " + JSON.stringify(node))
				}
				else
					verify(typeof node === "number" && node >= 0, "scalar sidecar node for '" + stampPath + "': " + node)
			}

			compare(sidecarAt(sidecar, "fixedB")[0], sel.fB,
				"fixedB's positional entry carries ITS host filter - the option-level stamp the mirror held, live")
			compare(sidecarAt(sidecar, "comboA"), sel.fA,
				"comboA's scalar entry carries its host filter - the equality the mirror stamp asserted")

			// and the sidecar stays EMPTY on an analysis whose values are not column-bearing:
			verify(sidecar.swA === undefined, "scalars generate no sidecar node (C3/M5)")
			verify(sidecar.dataSetA === undefined && sidecar.dataSetB === undefined && sidecar.codeSet === undefined,
				"the selection options generate no sidecar (their value IS the filter id)")

			// the reverse half of "generated == golden + sidecar" (the mirror never carried
			// .filterIds): nothing outside the stamped set may have a sidecar node.
			for (const k of Object.keys(sidecar))
				verify(stampedPaths.indexOf(k) !== -1, "no sidecar node outside the golden stamped set: " + k)
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

			// generator still == the golden bytes after the switch - same values with ONE legal
			// change: dataSetA is the selection option, its VALUE is the filter id, so it follows
			// the host (that is the option's own state, never provenance). The stamps the mirror
			// moved with its restamp half live, stripped, in the golden; their truth is the
			// sidecar above.
			const expectedSwitched = JSON.parse(goldenAware)
			expectedSwitched.dataSetA = String(sel.fB)
			assertEngineEqualsGolden("aware form after mid-session host switch", JSON.stringify(expectedSwitched))

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
			// ...and the parking is GONE: setAnalysisUp took it once through bindTo (the
			// successor of the mirror-side "attach cleared the parking; the controls rewrote it
			// with their OWN host stamps" check - the pending tree is consumed wholesale):
			verify(!awareForm.analysis.hasPendingOptions(), "the attach consumed the parking lot - the lying stamps left with it")

			// the sidecar carries the real host provenance the controls resolved:
			compare(sidecarAt(gen[".filterIds"], "depA")[0], sel.fA, "depA's sidecar follows its bound host, not the pending stamp")

			assertEngineEqualsGolden("aware form after pending-stamp re-attach", goldenReattach)
		}

		// ---- Storage shape (D5.1 final) ----------------------------------------------------------

		function test_storage_roundtrips_through_the_json_to_state_appliers()
		{
			// C12/D3.5 + D5.1: the file format IS the Storage tree now, so the JSON->state appliers
			// (bindTo path) must take it back losslessly: generate, feed through setOptions,
			// regenerate, BYTE-EQUAL - the exact save/reload loop of a .jasp file.
			_bindAwareForm()
			wait(100)
			const first = JSON.stringify(generatedOf(true))

			awareForm.setOptions(JSON.parse(first))
			wait(150)

			const again = JSON.stringify(generatedOf(true))
			compare(again, first, "a Storage tree round-trips through setOptions -> rebind -> regenerate")

			// and the Engine shape stays mirror-equivalent after the round-trip too:
			assertEngineEqualsGolden("aware form after Storage round-trip", goldenAware)
		}

		function test_storage_shape_is_clean_typed_state()
		{
			const sel = _bindAwareForm()
			wait(100)

			const engine	= generatedOf(false)
			const storage	= generatedOf(true)

			verify(storage[".meta"] === undefined, "Storage carries no .meta tree (it is a send-time artifact, C8/C11)")
			verify(storage[".filterIds"] === undefined, "Storage carries no sidecar tree - provenance rides INSIDE the term objects")
			verify(storage.depA.types === undefined, "no outer positional types in Storage (per-term since D5.1)")
			verify(storage.depA.filterIds === undefined, "the D5.1-preview positional filterIds member is gone")

			// D5.1 final: per-term {name, filterId?, types}. mds-plan post (ORIGIN-AUTHORITATIVE):
			// a bare bind is UNQUALIFIED - a bind is not an origin - so Storage honestly omits the
			// member, and the Engine sidecar serves the provenance PER SEND off the C13 chain
			// (against the LIVE host, like every provenance assertion in this file):
			compare(storage.depA.value.length, 1)
			verify(storage.depA.value[0].name === "TestInts", "term value lives inside the per-term object")
			verify(storage.depA.value[0].filterId === undefined,
				"a bare bind stays unqualified in Storage (stamps are born at origins only, never at intake)")
			compare(sidecarAt(engine[".filterIds"], "depA")[0], sel.fA,
				"the Engine sidecar backfills the resolved host per send (C13 chain)")
			verify(Array.isArray(storage.depA.value[0].types), "types ride per term")
			verify(storage.fixedB.value[0].filterId === undefined, "the second host's bare bind likewise stays unqualified in Storage")
			compare(sidecarAt(engine[".filterIds"], "fixedB")[0], sel.fB,
				"and ITS sidecar backfills its own host id per send")

			// unqualified ⟹ member omitted, never null:
			verify(storage.aloneA.value.length === 1 && storage.aloneA.value[0].filterId === undefined,
				"a hostless list omits filterId (nothing to say)")

			// the Engine wire is untouched: bare values + outer types + positional sidecar (frozen):
			compare(JSON.stringify(engine.depA.value), JSON.stringify(["TestInts"]), "Engine keeps bare term values (frozen R contract)")
			verify(Array.isArray(engine.depA.types) && engine[".filterIds"].depA.length === 1, "Engine keeps {value, types} + positional sidecar")

			// non-term families store exactly the engine value; scalars and selection options ride along:
			compare(deepDiff(storage.factorLevelsA, engine.factorLevelsA, ""), "", "the factorLevelList stores the engine value unchanged")
			compare(deepDiff(storage.rCodeBoxB, engine.rCodeBoxB, ""), "", "the textarea code stays raw in Storage (rCode contract, D7.1 keeps it so)")
			verify(storage.swA === true && storage.dataSetA !== undefined, "scalars and selection options ride along unchanged")
		}

		function test_storage_twin_terms_survive_the_save_reload_cycle()
		{
			// The D5.1 flagship (C9, strict Term equality from D2.1): TWO terms named "TestInts"
			// with DIFFERENT filterIds in ONE list must not dedupe through a save/reload cycle.
			// The Engine sidecar expresses them positionally ([fA, fB] over identical value
			// entries); the Storage format carries the ids inline, so the distinction survives
			// verbatim - and the intake stamping (ListModel::_setAllowedType, owns-the-stamp since
			// D5.1) leaves the loaded ids alone even though host A resolves to fA only.
			const sel = _bindAwareForm()
			wait(100)

			awareForm.setOptions({
				dataSetA:	String(sel.fA),
				dataSetB:	String(sel.fB),
				depA:		{ value: [
					{ name: "TestInts", types: ["scale"], filterId: sel.fA },
					{ name: "TestInts", types: ["scale"], filterId: sel.fB }
				] }
			})
			wait(150)

			// typed state: both twins present, each with its own provenance (the second one is
			// FOREIGN to host A and must have survived the bind untouched):
			compare(depA.model.termProvenanceForTest().join(","), "TestInts@" + sel.fA + ",TestInts@" + sel.fB,
				"twins coexist in the model and the foreign stamp survives intake")

			// Storage generation keeps both, ids inline:
			const storage	= generatedOf(true)
			const first		= JSON.stringify(storage)
			compare(storage.depA.value.length, 2, "twins survive Storage generation")
			compare(storage.depA.value[0].filterId, sel.fA, "twin 0 keeps fA inline")
			compare(storage.depA.value[1].filterId, sel.fB, "twin 1 keeps fB inline")

			// the reload leg: feed the stored tree back through the appliers, regenerate, byte-equal:
			awareForm.setOptions(JSON.parse(first))
			wait(150)
			compare(JSON.stringify(generatedOf(true)), first, "the twin Storage tree round-trips byte-for-byte")
			compare(depA.model.termProvenanceForTest().join(","), "TestInts@" + sel.fA + ",TestInts@" + sel.fB,
				"and the typed twins (with the foreign stamp) survived the rebind too")

			// the Engine wire expresses the same distinction positionally, aligned with the bare
			// value array (the frozen D3.3 contract - per-term objects never reach it):
			const engine	= generatedOf(false)
			compare(JSON.stringify(engine.depA.value), JSON.stringify(["TestInts", "TestInts"]), "Engine values stay bare and aligned")
			const ids		= engine[".filterIds"].depA
			compare(ids.length, 2, "the sidecar is positional over the twins")
			compare(ids[0], sel.fA, "a STAMPED term's sidecar entry is its own id (Storage == sidecar for stamped terms)")
			verify(ids[0] !== ids[1], "the twin distinction survives on the frozen wire")
		}
		function test_wrapper_generation_uses_the_selection_title_surface()
		{
			// mds-plan D8.3: the aware wrapper must not pin `data <- datasets[[1]]` - the primary
			// dataset is resolved per selection option through the TITLE surface (C4/RENDER), and
			// the datasets contract is stated on the wrapper itself.
			const sel = _bindAwareForm()
			wait(100)

			const wrapper = awareForm.generateWrapper("jaspTestModule", "multiDataSetFunc", "testMultiDataSet.qml", "Test", true, true)
			verify(wrapper.indexOf("datasets = NULL,") >= 0, "aware wrapper takes the datasets argument")
			verify(wrapper.indexOf("data = NULL,") < 0, "aware wrapper has no single-dataset data argument")
			verify(wrapper.indexOf("datasets[[1]]") < 0, "no positional datasets[[1]] pin")
			verify(wrapper.indexOf("selectionOptions <- c(\"dataSetA\", \"dataSetB\", \"codeSet\")") >= 0,
				"the selection options are emitted in registration order: " + wrapper)
			verify(wrapper.indexOf("for (selectionOption in selectionOptions)") >= 0, "the body resolves data per selection option")
			verify(wrapper.indexOf("must be a named list keyed by dataset title") >= 0, "a mismatch fails loudly naming the datasets contract")
			verify(wrapper.indexOf("#' @param datasets") >= 0, "the datasets contract is documented on the wrapper")
			verify(wrapper.indexOf(", datasets = datasets))") >= 0, "runWrappedAnalysis receives the datasets")

			// C4/RENDER: the selection defaults enter the wrapper as surface strings, never ids
			// (here the unbound FilterSelect default resolves to the first dataset's title).
			verify(/dataSetA = "[^0-9]/.test(wrapper), "dataSetA default is a title surface, not an id: " + wrapper.match(/dataSetA = "[^"]*"/))
			verify(/dataSetB = "[^0-9]/.test(wrapper), "dataSetB default is a title surface, not an id")
			verify(wrapper.indexOf('dataSetA = "' + sel.fA + '"') < 0, "no raw filter id leaks into the wrapper defaults")

			// the same form, generated NON-aware: the classic template is untouched
			const plain = awareForm.generateWrapper("jaspTestModule", "multiDataSetFunc", "testMultiDataSet.qml", "Test", true, false)
			verify(plain.indexOf("data = NULL,") >= 0, "non-aware wrapper keeps the data argument")
			verify(plain.indexOf("datasets = NULL,") < 0, "non-aware wrapper has no datasets argument")
			verify(plain.indexOf("jaspBase::storeDataSet(data)") >= 0, "non-aware wrapper keeps the store block")
			verify(plain.indexOf("selectionOptions <- c(") < 0, "non-aware wrapper has no selection resolution block")
		}
	}
}
