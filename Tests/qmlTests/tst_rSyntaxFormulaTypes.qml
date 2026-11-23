import QtTest
import QtQuick
import JASP.Controls
import JASP

// A formula read back from R syntax gives each variable the type it has in the data, also when the variable lists
// get it next to variables of another type (here a scale covariate after two nominal factors and their interaction,
// as MixedModelsGLMM's MWCount ~ Chronotype * TimeOfDay + Sleep + (1 | Subject)).
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1000

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		Formula
		{
			lhs: "dependentFT"
			rhs: "modelFT"
		}

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVarsFT" }
			AssignedVariablesList	{ name: "dependentFT";	singleVariable: true;	allowedColumns: ["scale"];				allowTypeChange: true }
			AssignedVariablesList	{ name: "fixedFT";		allowedColumns: ["nominal", "scale"];	allowTypeChange: true }
		}

		VariablesForm
		{
			AvailableVariablesList	{ name: "componentsFT";	source: "fixedFT" }
			AssignedVariablesList	{ name: "modelFT";		listViewType: JASP.Interaction;	addInteractionsByDefault: false }
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestRSyntaxFormulaTypes"

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

		function test_formula_terms_keep_their_own_type()
		{
			// The formula as jaspBase gives it for dependentFT = TestInts, TestLetters * TestNominal + TestDoubles
			var formula = {
				formula:	"TestInts ~ TestLetters * TestNominal + TestDoubles",
				lhs:		{ vars: "TestInts" },
				rhs:		{ fixed: { vars: ["TestLetters", "TestNominal", "TestDoubles", "TestLetters:TestNominal"], intercept: true } }
			}
			var parsed = JSON.parse(jaspForm.parseOptionsToJson(JSON.stringify({ formula: formula })))
			verify(parsed.ok, "the formula is valid: " + parsed.error)

			var fixed = parsed.options.fixedFT
			compare(JSON.stringify(fixed.value), JSON.stringify(["TestLetters", "TestNominal", "TestDoubles"]), "the variables of the formula")
			compare(JSON.stringify(fixed.types), JSON.stringify(["nominal", "nominal", "scale"]), "each variable keeps its type, the scale one too")

			var model = parsed.options.modelFT
			compare(JSON.stringify(model.types), JSON.stringify(["nominal", "nominal", "scale", ["nominal", "nominal"]]), "the model terms keep their types")
		}
	}
}
