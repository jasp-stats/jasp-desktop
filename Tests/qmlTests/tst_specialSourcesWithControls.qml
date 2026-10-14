import QtQuick
import JASP.Controls
import JASP

// Section "Source with controls" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesWithControls"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList4"; id: allVariablesList4 }
			AssignedVariablesList
			{
				id				: selectedVariablesWithControls
				name			: "selectedVariablesWithControls"

				rowComponent	: Row
				{
					DropDown { name: "extra"; values: ["one", "two", "three", "four", "five"] }
					CheckBox { name: "check" }
				}
			}
		}

		DropDown
		{
			id		: controlVariables
			name	: "controlVariables"
			source	: [{ name: "selectedVariablesWithControls.extra", condition: "check" }]
		}
	}

	function extra(name)	{ return rowControl(selectedVariablesWithControls, name, "extra") }
	function check(name)	{ return rowControl(selectedVariablesWithControls, name, "check") }

	function test_sourceWithControls()
	{
		moveVariables(allVariablesList4, selectedVariablesWithControls, ["TestDoubles", "TestInts", "TestLetters"])
		checkList(controlVariables, [], "No variable is checked")

		for (var name of ["TestDoubles", "TestInts", "TestLetters"])
			compare(extra(name).currentValue, "one", "The default value of the row DropDown is 'one'")

		check("TestDoubles").click()
		checkList(controlVariables, ["one"],			"Only the values of the checked variables are used")

		extra("TestDoubles").currentValue = "two"
		checkList(controlVariables, ["two"],			"A changed value is propagated")

		extra("TestInts").currentValue = "two"
		checkList(controlVariables, ["two"],			"The value of an unchecked variable is not used")
		check("TestInts").click()
		checkList(controlVariables, ["two"],			"Duplicate values are removed")

		extra("TestInts").currentValue = "three"
		checkList(controlVariables, ["two", "three"],	"The values of all checked variables are used")

		extra("TestLetters").currentValue = "three"
		check("TestLetters").click()
		check("TestLetters").click()
		checkList(controlVariables, ["two", "three"],	"Unchecking a variable having the same value as another checked variable keeps the value")

		// All checked variables have a value different than 'one': a new variable gets the default 'one'
		moveVariables(allVariablesList4, selectedVariablesWithControls, ["TestNominal"])
		compare(extra("TestNominal").currentValue, "one", "A new variable gets the default value")
		checkList(controlVariables, ["two", "three"],	"The new variable is not checked")
		check("TestNominal").click()
		checkList(controlVariables, ["two", "three", "one"], "The value of the new checked variable is added")

		// Remove the variable with the current value: the DropDown gets automatically another value
		controlVariables.currentValue = "three"
		compare(controlVariables.currentValue, "three")
		moveVariables(selectedVariablesWithControls, allVariablesList4, ["TestInts"])
		checkList(controlVariables, ["two", "one"], "The value of the removed variable is removed")
		verify(["two", "one"].indexOf(controlVariables.currentValue) >= 0,
			   "The DropDown value should be set to another value, but is '" + controlVariables.currentValue + "'")

		controlVariables.currentValue = "one"
		compare(controlVariables.currentValue, "one")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		controlVariables.currentValue = "two"
		reloadOptions(options)

		checkList(selectedVariablesWithControls, ["TestDoubles", "TestLetters", "TestNominal"], "Variables after reload")
		compare(extra("TestDoubles").currentValue,	"two")
		compare(extra("TestLetters").currentValue,	"three")
		compare(extra("TestNominal").currentValue,	"one")
		compare(check("TestDoubles").checked,		true)
		compare(check("TestLetters").checked,		false)
		compare(check("TestNominal").checked,		true)
		checkList(controlVariables, ["two", "one"],	"Controls values after reload")
		compare(controlVariables.currentValue,		"one")
	}
}
