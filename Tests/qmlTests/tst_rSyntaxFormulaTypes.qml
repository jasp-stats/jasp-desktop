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

		function test_formula_interaction_components_take_the_type_of_their_list()
		{
			// TestInts is ordinal, which fixedFT does not allow: there it becomes nominal, and so must the TestInts in the
			// interaction, also when the interaction comes without its main effect (jasp-stats/INTERNAL-jasp#3467)
			var cases = [
				{ formula: "TestDoubles ~ TestLetters * TestInts",				vars: ["TestLetters", "TestInts", "TestLetters:TestInts"],
				  modelTypes: ["nominal", "nominal", ["nominal", "nominal"]] },
				{ formula: "TestDoubles ~ TestLetters + TestLetters:TestInts",	vars: ["TestLetters", "TestLetters:TestInts"],
				  modelTypes: ["nominal", ["nominal", "nominal"]] }
			]

			for (var i = 0; i < cases.length; i++)
			{
				var formula = {
					formula:	cases[i].formula,
					lhs:		{ vars: "TestDoubles" },
					rhs:		{ fixed: { vars: cases[i].vars, intercept: true } }
				}
				var parsed = JSON.parse(jaspForm.parseOptionsToJson(JSON.stringify({ formula: formula })))
				verify(parsed.ok, cases[i].formula + " is valid: " + parsed.error)

				var fixed = parsed.options.fixedFT
				compare(JSON.stringify(fixed.value), JSON.stringify(["TestLetters", "TestInts"]), cases[i].formula + ": the variables")
				compare(JSON.stringify(fixed.types), JSON.stringify(["nominal", "nominal"]), cases[i].formula + ": the ordinal variable becomes nominal")
				compare(JSON.stringify(parsed.options.modelFT.types), JSON.stringify(cases[i].modelTypes), cases[i].formula + ": the interaction has the types of the variables list")
			}
		}

		function test_formula_keeps_the_type_given_with_a_variable()
		{
			// fixedFT allows nominal and scale: TestDoubles.nominal stays nominal, though its column is scale and scale is allowed,
			// as a main effect and in an interaction (jasp-stats/INTERNAL-jasp#3467)
			var formula = {
				formula:	"TestInts ~ TestDoubles.nominal * TestLetters",
				lhs:		{ vars: "TestInts" },
				rhs:		{ fixed: { vars: ["TestDoubles.nominal", "TestLetters", "TestDoubles.nominal:TestLetters"], intercept: true } }
			}
			var parsed = JSON.parse(jaspForm.parseOptionsToJson(JSON.stringify({ formula: formula })))
			verify(parsed.ok, "the formula is valid: " + parsed.error)

			var fixed = parsed.options.fixedFT
			compare(JSON.stringify(fixed.value), JSON.stringify(["TestDoubles", "TestLetters"]), "the variables of the formula, without their type")
			compare(JSON.stringify(fixed.types), JSON.stringify(["nominal", "nominal"]), "the scale variable has the type given with it")
			compare(JSON.stringify(parsed.options.modelFT.types), JSON.stringify(["nominal", "nominal", ["nominal", "nominal"]]), "the model terms too")
		}

		function test_formula_keeps_the_type_of_a_single_variable_already_given()
		{
			// R gives a list of one variable as a string: the formula adds its variables next to it
			var formula = {
				formula:	"TestInts ~ TestLetters",
				lhs:		{ vars: "TestInts" },
				rhs:		{ fixed: { vars: ["TestLetters"], intercept: true } }
			}
			var options = { formula: formula, fixedFT: { value: "TestDoubles", types: "nominal" } }
			var parsed = JSON.parse(jaspForm.parseOptionsToJson(JSON.stringify(options)))
			verify(parsed.ok, "the options are valid: " + parsed.error)

			var fixed = parsed.options.fixedFT
			compare(JSON.stringify(fixed.value), JSON.stringify(["TestDoubles", "TestLetters"]), "the given variable and the one of the formula")
			compare(JSON.stringify(fixed.types), JSON.stringify(["nominal", "nominal"]), "the given variable keeps its type, also when it is not the type of its column")
		}
	}
}
