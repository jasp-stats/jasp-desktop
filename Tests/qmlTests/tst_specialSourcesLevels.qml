import QtQuick
import JASP.Controls
import JASP

// Section "Source with levels" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesLevels"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList2"; id: allVariablesList2 }
			AssignedVariablesList
			{
				id				: variableWithLabels
				name			: "variableWithLabels"
				singleVariable	: true
				allowedColumns	: ["nominal", "ordinal"]
			}

			AssignedVariablesList
			{
				id				: severalVariablesWithLabels
				name			: "severalVariablesWithLabels"
				allowedColumns	: ["nominal", "ordinal"]
			}
		}

		DropDown
		{
			id		: labelsFor1Variable
			name	: "labelsFor1Variable"
			label	: "Labels for 1 variable: " + variableWithLabels.levels.length
			values	: variableWithLabels.levels
		}

		DropDown
		{
			id		: labelsForSeveralVariable
			name	: "labelsForSeveralVariable"
			label	: "Labels for several variables: " + severalVariablesWithLabels.levels.length
			values	: severalVariablesWithLabels.levels
		}
	}

	function test_levels()
	{
		moveVariables(allVariablesList2, variableWithLabels, ["TestLetters"])
		checkList(labelsFor1Variable, ["A", "B", "C", "D", "E"],	"Labels of 1 variable")
		compare(labelsFor1Variable.label, "Labels for 1 variable: 5",	"The number of levels is shown in the title")

		moveVariables(allVariablesList2, severalVariablesWithLabels, ["TestInts", "TestNominal"])
		checkList(labelsForSeveralVariable, ["1", "2", "3", "4", "5"],	"Labels of several variables: duplicate labels are removed")
		compare(labelsForSeveralVariable.label, "Labels for several variables: 7")

		moveVariables(variableWithLabels, allVariablesList2, ["TestLetters"])
		checkList(labelsFor1Variable, [], "No labels without variable")
		compare(labelsFor1Variable.label, "Labels for 1 variable: 0")

		moveVariables(allVariablesList2, severalVariablesWithLabels, ["TestLetters"])
		checkList(labelsForSeveralVariable, ["1", "2", "3", "4", "5", "A", "B", "C", "D", "E"],	"The labels follow the order of the variables")
		compare(labelsForSeveralVariable.label, "Labels for several variables: 12")

		// A variable can be moved from one assigned list to the other
		moveVariables(severalVariablesWithLabels, variableWithLabels, ["TestNominal"])
		checkList(labelsFor1Variable, ["1", "2"], "Labels of another variable")
		checkList(labelsForSeveralVariable, ["1", "2", "3", "4", "5", "A", "B", "C", "D", "E"],	"TestNominal labels are also in TestInts")
		compare(labelsForSeveralVariable.label, "Labels for several variables: 10")

		labelsFor1Variable.currentValue			= "2"
		labelsForSeveralVariable.currentValue	= "D"
		compare(labelsFor1Variable.currentValue,		"2")
		compare(labelsForSeveralVariable.currentValue,	"D")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		labelsFor1Variable.currentValue			= "1"
		labelsForSeveralVariable.currentValue	= "1"
		reloadOptions(options)

		checkList(labelsFor1Variable,		["1", "2"],													"Labels of 1 variable after reload")
		checkList(labelsForSeveralVariable,	["1", "2", "3", "4", "5", "A", "B", "C", "D", "E"],			"Labels of several variables after reload")
		compare(labelsFor1Variable.currentValue,		"2")
		compare(labelsForSeveralVariable.currentValue,	"D")
	}
}
