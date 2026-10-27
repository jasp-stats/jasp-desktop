import QtTest
import QtQuick
import JASP
import JASP.Controls

// the FactorsForm provenance deferral closed, control-level pins.
//
// The factors option's transient `.filterIds` sidecar is an ARRAY-OF-ARRAYS (element i = factor
// i's per-term filterId array, FactorsFormBase::createFilterIds) and its Storage shape carries the
// per-term objects ({name, filterId?, types} - the per-term discipline applied per factor), so a
// same-name term in two factors of one form keeps two DISTINCT provenances through a save/reload
// cycle (the flagship twins, at factor granularity). The engine-side consumption is pinned
// C++-side (testPerFactorProvenanceEncodesViaOwnViews + the no-misfire pins); this file pins the
// CONTROL: generation, the bindTo applier (Storage + legacy/engine shapes), and the roundtrip.
//
// Fixture (Tests/testqml.cpp): dataset 1 + "Second" (TestInts in both, extra filters, reopened).

Item
{
	width:	800
	height: 800

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	600

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsA;	name: "allVarsA";	dataSetSelectionOption: "dataSetA" }
			AssignedVariablesList	{ id: srcA;		name: "srcA" }
		}

		// a second selecting host: its filter ids are the foreign stamps the factor twins carry
		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsB;	name: "allVarsB";	dataSetSelectionOption: "dataSetB" }
		}

		FactorsForm
		{
			id:					factorsFF
			name:				"factorsFF"
			initNumberFactors:	2
			baseName:			"Fac"
			baseTitle:			"Factor"
			allowedColumns:		["scale", "ordinal", "nominal"]
		}

		// the interaction flavour: optionKey "components", engine value = component arrays,
		// Storage = the row/nested-term-object discipline
		FactorsForm
		{
			id:					factorsInt
			name:				"factorsInt"
			initNumberFactors:	1
			baseName:			"IntFac"
			baseTitle:			"Int Factor"
			allowedColumns:		["scale", "ordinal", "nominal"]
			allowInteraction:	true
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestFactorsFormProvenance"

		SignalSpy
		{
			id:			spyCompleted
			target:		jaspForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyCompleted.wait(3000)
			compare(spyCompleted.count, 1, "The form should have completed")
			verify(jaspForm.initialized(), "the dummy analysis must have completed the setup pass (NO_DESKTOP_MODE)")
		}

		// ---- helpers ------------------------------------------------------------------------

		function secondFilterIds()
		{
			const ids = []
			for (const entry of allVarsA.filterSelectionValues)
				if (String(entry.label).indexOf("Second - ") === 0)
					ids.push(parseInt(entry.value))
			return ids
		}

		function generatedOf(storage)
		{
			return JSON.parse(jaspForm.createOptionsJsonAsJson(storage ? 1 : 0))
		}

		function stripKey(tree, key)
		{
			if (Array.isArray(tree))
			{
				for (const elt of tree)
					stripKey(elt, key)
				return tree
			}
			if (tree !== null && typeof tree === "object")
			{
				delete tree[key]
				for (const k of Object.keys(tree))
					stripKey(tree[k], key)
			}
			return tree
		}

		// ---- tests (alphabetical execution order: each binds its own state explicitly) ------

		// The legacy/engine shapes still bind byte-identically: the bare value shape and the
		// {value, types} shape parse unqualified, and the sidecar backfills the option's host id
		// per term (the old option-level scalar's id, now per position).
		function test_engineShapesStillBind_andSidecarBackfillsHost()
		{
			try
			{
				jaspForm.setOptions({
					factorsFF: [
						{ name: "Fac1", title: "Factor 1", indicators: { value: ["TestInts"], types: ["scale"] } },
						{ name: "Fac2", title: "Factor 2", indicators: { value: ["TestDoubles"], types: ["scale"] } }
					]
				})
				wait(200)

				const gen = generatedOf(false)
				verify(gen[".filterIds"] !== undefined, "the aware form generates a sidecar")
				const node = gen[".filterIds"]["factorsFF"]
				verify(Array.isArray(node), "the factors sidecar node is an array (of per-factor arrays)")
				compare(node.length, 2, "one per-factor array per factor")
				verify(Array.isArray(node[0]) && Array.isArray(node[1]), "each factor's node is its per-term array")
				compare(node[0].length, 1, "factor 1 has one term position")
				compare(node[1].length, 1, "factor 2 has one term position")
				const hostId = node[0][0]
				verify(typeof hostId === "number" && hostId >= 0, "unqualified factor terms are backfilled with the option's host id")
				compare(node[1][0], hostId, "both factors' unqualified terms ride the same host id (the old option-level scalar's semantics)")

				//the engine VALUES are untouched by the sidecar shape (frozen R contract)
				compare(gen["factorsFF"][0]["indicators"]["value"][0], "TestInts", "factor 1's term value survives")
				compare(gen["factorsFF"][1]["indicators"]["value"][0], "TestDoubles", "factor 2's term value survives")

				//and the bare legacy shape (no {value, types} wrapper) parses the same way
				jaspForm.setOptions({
					factorsFF: [
						{ name: "Fac1", title: "Factor 1", indicators: ["TestInts"] }
					]
				})
				wait(200)
				const genBare = generatedOf(false)
				compare(genBare["factorsFF"][0]["indicators"]["value"][0], "TestInts", "the bare legacy shape still binds")
				compare(genBare[".filterIds"]["factorsFF"][0][0], hostId, "bare-shape terms backfill the host id too")
			}
			finally
			{
				jaspForm.setOptions({ factorsFF: [] })
			}
		}

		// The flagship: the SAME column name in two factors of one form, bound through the Storage
		// shape with two DIFFERENT filter ids, keeps both provenances through generation AND the
		// save/reload cycle (createStorageValue -> bindTo -> regenerate, byte-equal).
		function test_storageTwinsKeepDistinctFilterIds_throughSaveReload()
		{
			const filters = secondFilterIds()
			verify(filters.length >= 2, "fixture must offer at least two 'Second' filters, got " + filters.length)
			const f1 = filters[0], f2 = filters[1]
			verify(f1 !== f2, "the two filters must be distinct")

			try
			{
				//the Storage shape, per factor: {value: [{name, types, filterId?}]}
				jaspForm.setOptions({
					factorsFF: [
						{ name: "Fac1", title: "Factor 1", indicators: { value: [{ name: "TestInts", types: ["scale"], filterId: f1 }] } },
						{ name: "Fac2", title: "Factor 2", indicators: { value: [{ name: "TestInts", types: ["scale"], filterId: f2 }] } }
					]
				})
				wait(200)

				//the engine sidecar is factor-positionally provenanced: twin TestInts, two filters
				const gen = generatedOf(false)
				const sidecar = gen[".filterIds"]["factorsFF"]
				verify(Array.isArray(sidecar) && sidecar.length === 2, "per-factor sidecar for the two factors")
				compare(sidecar[0][0], f1, "factor 1's TestInts encodes through ITS filter's view")
				compare(sidecar[1][0], f2, "factor 2's SAME-NAME TestInts encodes through the OTHER filter's view")
				compare(gen["factorsFF"][0]["indicators"]["value"][0], "TestInts", "engine values stay the frozen shape")
				compare(gen["factorsFF"][1]["indicators"]["value"][0], "TestInts", "the twin keeps its own value entry (no dedupe)")

				//Storage roundtrip: generate Storage -> bind it back -> regenerate byte-equal,
				//with the twins' distinct filterIds intact through the applier
				const storage1 = generatedOf(true)
				compare(storage1["factorsFF"][0]["indicators"]["value"][0]["filterId"], f1, "Storage keeps factor 1's term filterId")
				compare(storage1["factorsFF"][1]["indicators"]["value"][0]["filterId"], f2, "Storage keeps factor 2's term filterId")
				compare(storage1["factorsFF"][0]["indicators"]["value"][0]["name"], "TestInts", "Storage term objects carry the name")

				jaspForm.setOptions({ factorsFF: storage1["factorsFF"] })
				wait(200)

				const storage2 = generatedOf(true)
				compare(JSON.stringify(storage2["factorsFF"]), JSON.stringify(storage1["factorsFF"]),
						"the Storage tree round-trips byte-equal through bindTo (twins' provenance lossless)")
				const genAfter = generatedOf(false)
				compare(genAfter[".filterIds"]["factorsFF"][0][0], f1, "the sidecar still names factor 1's filter after the roundtrip")
				compare(genAfter[".filterIds"]["factorsFF"][1][0], f2, "the sidecar still names factor 2's filter after the roundtrip")

				//and the interaction flavour: the row/nested-term-object Storage discipline
				jaspForm.setOptions({
					factorsInt: [
						{ name: "IntFac1", title: "Int Factor 1",
						  components: { value: [{ components: { name: ["TestInts", "TestDoubles"], types: ["scale", "scale"], filterId: f2 } }], optionKey: "components" } }
					]
				})
				wait(200)

				const genInt = generatedOf(false)
				const intSidecar = genInt[".filterIds"]["factorsInt"]
				verify(Array.isArray(intSidecar) && intSidecar.length === 1 && Array.isArray(intSidecar[0]),
						"the interaction factor gets its per-term array too")
				compare(intSidecar[0][0], f2, "the interaction term's components share the term-level filterId")
				compare(JSON.stringify(genInt["factorsInt"][0]["components"]["value"][0]), JSON.stringify(["TestInts", "TestDoubles"]),
						"the interaction term's engine value is the component array (frozen shape)")

				const storageInt1 = generatedOf(true)
				compare(storageInt1["factorsInt"][0]["components"]["value"][0]["components"]["filterId"], f2,
						"Storage keeps the interaction term's filterId under the optionKey nesting")
				jaspForm.setOptions({ factorsInt: storageInt1["factorsInt"] })
				wait(200)
				compare(JSON.stringify(generatedOf(true)["factorsInt"]), JSON.stringify(storageInt1["factorsInt"]),
						"the interaction Storage tree round-trips byte-equal")
			}
			finally
			{
				jaspForm.setOptions({ factorsFF: [], factorsInt: [] })
			}
		}
	}
}
