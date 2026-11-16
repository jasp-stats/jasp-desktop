import QtQuick
import JASP.Controls
import JASP

// Section "Source with conditions" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesWithConditions"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList5"; id: allVariablesList5 }
			AssignedVariablesList
			{
				id				: selectedVariablesWithCondition
				name			: "selectedVariablesWithControls2"

				rowComponent	: Row
				{
					DropDown { name: "extraValue"; values: ["one", "two", "three", "four", "five"] }
					CheckBox { name: "check" }
				}
			}
		}

		AssignedVariablesList
		{
			id				: variablesWithConditions
			name			: "variablesWithConditions"
			source			: [ {name: "selectedVariablesWithControls2", condition: "check == false && extraValue == 'one'"} ]
		}

		AssignedVariablesList
		{
			id				: variablesWithConditions2
			name			: "variablesWithConditions2"
			source			: [ {name: "selectedVariablesWithControls2", condition: "check == true && extraValue != 'one'"} ]
		}
	}

	function extra(name)	{ return rowControl(selectedVariablesWithCondition, name, "extraValue") }
	function check(name)	{ return rowControl(selectedVariablesWithCondition, name, "check") }

	function test_sourceWithConditions()
	{
		moveVariables(allVariablesList5, selectedVariablesWithCondition, ["TestDoubles", "TestInts", "TestLetters"])
		checkList(variablesWithConditions,	["TestDoubles", "TestInts", "TestLetters"],	"All variables are unchecked with value 'one'")
		checkList(variablesWithConditions2,	[],											"No variable is checked")

		check("TestDoubles").click()
		checkList(variablesWithConditions,	["TestInts", "TestLetters"],	"A checked variable does not satisfy the first condition")
		checkList(variablesWithConditions2,	[],								"A checked variable with value 'one' does not satisfy the second condition")

		extra("TestDoubles").currentValue = "two"
		checkList(variablesWithConditions2,	["TestDoubles"],				"A checked variable with value 'two' satisfies the second condition")

		extra("TestInts").currentValue = "three"
		checkList(variablesWithConditions,	["TestLetters"],				"An unchecked variable with value 'three' does not satisfy the first condition")
		checkList(variablesWithConditions2,	["TestDoubles"],				"An unchecked variable does not satisfy the second condition")

		check("TestInts").click()
		checkList(variablesWithConditions2,	["TestDoubles", "TestInts"],	"Both checked variables have a value different than 'one'")

		extra("TestDoubles").currentValue = "one"
		checkList(variablesWithConditions,	["TestLetters"],				"A checked variable with value 'one' does not satisfy the first condition")
		checkList(variablesWithConditions2,	["TestInts"],					"A checked variable with value 'one' does not satisfy the second condition anymore")

		moveVariables(allVariablesList5, selectedVariablesWithCondition, ["TestNominal"])
		checkList(variablesWithConditions,	["TestLetters", "TestNominal"],	"A new variable satisfies the first condition")
		checkList(variablesWithConditions2,	["TestInts"],					"A new variable does not satisfy the second condition")

		moveVariables(selectedVariablesWithCondition, allVariablesList5, ["TestInts"])
		checkList(variablesWithConditions2,	[],								"A removed variable is removed from the conditional lists")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		check("TestNominal").click()
		reloadOptions(options)

		checkList(selectedVariablesWithCondition, ["TestDoubles", "TestLetters", "TestNominal"], "Variables after reload")
		compare(check("TestDoubles").checked,		true)
		compare(check("TestNominal").checked,		false)
		compare(extra("TestDoubles").currentValue,	"one")
		checkList(variablesWithConditions,	["TestLetters", "TestNominal"],	"First condition after reload")
		checkList(variablesWithConditions2,	[],								"Second condition after reload")
	}
}
