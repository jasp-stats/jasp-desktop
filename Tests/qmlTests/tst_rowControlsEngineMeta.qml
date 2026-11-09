import QtTest
import QtQuick
import JASP.Controls

// The Engine `.meta` of a control in the rows of a list (here a variables list in the rows of a ComponentsList, like
// the predictors of each model in jaspSurvival's advanced model specification) must be generated too:
// createOptionsJson only walks the controls of the form, so the `shouldEncode` of such a row control was lost and
// its column names reached R unencoded (a variable whose type differs from its column even as "name.type").
// The meta is placed under the names of the parent lists, not under a row key, as the deleted options mirror did.
//
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1000

	property alias form: rowMetaForm

	Form
	{
		id:		rowMetaForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVarsRM" }
			AssignedVariablesList	{ name: "covariatesRM" }
		}

		ComponentsList
		{
			id:				modelsRM
			name:			"modelsRM"
			minimumItems:	1
			rowComponent:	ComponentsList
			{
				name:			"predictorsRM"
				source:			"covariatesRM"
				optionKey:		"variable"
				rowComponent:	CheckBox { name: "transformRM" }
			}
		}
	}

	TestCase
	{
		name:	"TestRowControlsEngineMeta"

		SignalSpy
		{
			id:				spyCompleted
			target:			rowMetaForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyCompleted.wait(3000)
			compare(spyCompleted.count, 1, "The form should have completed")
		}

		function test_variablesListInRowsGetsShouldEncode()
		{
			rowMetaForm.setOptions({
				covariatesRM:	{ value: ["TestDoubles", "TestInts"], types: ["scale", "scale"] },
				modelsRM:		[{ value: "#", predictorsRM: [{ variable: "TestInts", transformRM: true }] }]
			})

			const options = JSON.parse(rowMetaForm.createOptionsJsonAsJson(0))
			const meta = options[".meta"] || {}

			compare(options.modelsRM.length, 1, "One model row: " + JSON.stringify(options.modelsRM))
			verify(options.modelsRM[0].predictorsRM !== undefined, "The model row has its predictors: " + JSON.stringify(options.modelsRM))
			verify(meta.covariatesRM !== undefined && meta.covariatesRM.shouldEncode === true, "A form level variables list is encoded: " + JSON.stringify(meta))
			verify(meta.modelsRM !== undefined && meta.modelsRM.predictorsRM !== undefined, "The meta of the predictors in the model rows is generated: " + JSON.stringify(meta))
			compare(meta.modelsRM.predictorsRM.shouldEncode, true, "The variables in the model rows are encoded")
			verify(meta.modelsRM.shouldEncode === undefined, "The model list itself (no variables) is not encoded: " + JSON.stringify(meta))
		}
	}
}
