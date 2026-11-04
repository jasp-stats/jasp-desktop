import QtTest
import QtQuick
import JASP
import JASP.Controls

// The terms of an interaction list whose variables come from another list (here: model terms made of the factors, like
// Log-Linear Regression) must keep the type that list gave them. TestInts is ordinal in the dataset, but the factors list
// only allows nominal, so it becomes nominal there: the model terms must use nominal as well, for the main effect and for
// the interaction, or they are encoded as different columns and the analysis sees an interaction without its main effect.
//
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: termTypesForm

	Form
	{
		id:		termTypesForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsT;		name: "allVarsT" }
			AssignedVariablesList	{ id: factorsT;		name: "factorsT";	allowedColumns: ["nominal"] }
			AssignedVariablesList	{ id: anyTypeT;		name: "anyTypeT" }
		}

		VariablesForm
		{
			AvailableVariablesList	{ id: availableTermsT;	name: "availableTermsT";	source: ["factorsT"] }
			AssignedVariablesList	{ id: modelTermsT;		name: "modelTermsT";		listViewType: JASP.Interaction }
		}
	}

	TestCase
	{
		name: "TestInteractionTermTypes"

		SignalSpy
		{
			id:			spyLoader
			target:		termTypesForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function generated()
		{
			wait(100) // let queued termsChanged/setBoundValue writes settle
			return JSON.parse(termTypesForm.createOptionsJsonAsJson(0))
		}

		function factorsAndUntypedModelTerms(modelTerms)
		{
			termTypesForm.setOptions({
				factorsT:		{ value: ["TestInts", "TestLetters"], types: ["nominal", "nominal"] },
				modelTermsT:	modelTerms
			})
		}

		// The options of an R syntax call or of jaspTools give model terms without types
		function test_untypedModelTermsTakeTheTypesOfTheFactors()
		{
			factorsAndUntypedModelTerms([ { components: "TestInts" }, { components: "TestLetters" }, { components: ["TestInts", "TestLetters"] } ])

			const gen = generated()
			compare(JSON.stringify(gen.factorsT.types), JSON.stringify(["nominal", "nominal"]), "The factors list converts the ordinal TestInts to nominal")
			compare(JSON.stringify(gen.modelTermsT.types), JSON.stringify(["nominal", "nominal", ["nominal", "nominal"]]),
					"The main effect and the interaction take the type TestInts has in the factors, not its ordinal column type")
		}

		function test_untypedInteractionAloneTakesTheTypesOfTheFactors()
		{
			factorsAndUntypedModelTerms([ { components: ["TestInts", "TestLetters"] } ])

			const gen = generated()
			compare(JSON.stringify(gen.modelTermsT.types[gen.modelTermsT.types.length - 1]), JSON.stringify(["nominal", "nominal"]),
					"An interaction term without types takes the types of the factors")
		}

		// Moving a factor into the model terms, as a user does in the form
		function test_movedTermKeepsTheTypeOfTheFactors()
		{
			termTypesForm.setOptions({ factorsT: { value: ["TestInts"], types: ["nominal"] }, modelTermsT: [] })
			tryCompare(availableTermsT, "count", 1, 1000, "The available terms come from the factors")

			availableTermsT.itemDoubleClicked(0)
			tryCompare(modelTermsT, "count", 1, 1000, "The double clicked term is added to the model terms")

			const gen = generated()
			compare(JSON.stringify(gen.modelTermsT.types), JSON.stringify(["nominal"]),
					"A term moved from the factors keeps its nominal type instead of going back to its ordinal column type")
		}

		// A list fed by the dataset still takes the column type of an untyped variable
		function test_listFedByTheDataSetUsesTheColumnType()
		{
			termTypesForm.setOptions({ factorsT: [], anyTypeT: ["TestInts"], modelTermsT: [] })

			const gen = generated()
			compare(JSON.stringify(gen.anyTypeT.types), JSON.stringify(["ordinal"]), "An untyped variable from the dataset gets its column type")
		}
	}
}
