import QtQuick
import JASP.Controls
import JASP

// When a factor of a FactorsForm is removed, its variables must become available again.
SpecialSourcesTestCase
{
	name:	"TestFactorsFormRemoveFactor"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		FactorsForm
		{
			id:					factors
			name:				"factors"
			initNumberFactors:	3
		}
	}

	function available()	{ return factors.availableVariablesList }
	function factor(i)		{ return factors.factorsItems[i] }

	function test_removeFactor()
	{
		checkList(available(), ["TestDoubles", "TestInts", "TestLetters", "TestNominal"], "All variables are available")

		moveVariables(available(), factor(0), ["TestDoubles"])
		moveVariables(available(), factor(1), ["TestInts"])
		moveVariables(available(), factor(2), ["TestLetters", "TestNominal"])
		checkList(available(), [], "All variables are assigned")

		factors.removeFactor()
		tryCompare(factors.factorsItems, "length", 2)
		checkList(available(),	["TestLetters", "TestNominal"],	"The variables of the removed factor are available again")
		checkList(factor(0),	["TestDoubles"],				"The other factors keep their variables")
		checkList(factor(1),	["TestInts"],					"The other factors keep their variables")

		factors.removeFactor()
		tryCompare(factors.factorsItems, "length", 1)
		checkList(available(), ["TestInts", "TestLetters", "TestNominal"], "The variables of the removed factor are available again")

		// The available variables can be assigned again
		factors.addFactor()
		tryCompare(factors.factorsItems, "length", 2)
		moveVariables(available(), factor(1), ["TestLetters"])
		checkList(available(), ["TestInts", "TestNominal"], "An available variable can be assigned to a new factor")

		// Duplicate the analysis: the controls should have the same values.
		// The FactorsForm updates its value asynchronously when the variables of a factor change: wait for it before getting the options.
		tryVerify(function()
		{
			var factorsOption = form.options()["factors"]
			return factorsOption.length === 2 && JSON.stringify(factorsOption[1]["indicators"]["value"]) === JSON.stringify(["TestLetters"])
		}, 1000, "The option of the FactorsForm gets the variables of its factors")
		var options = form.options()
		reloadOptions(options)
		tryCompare(factors.factorsItems, "length", 2)
		checkList(factor(0),	["TestDoubles"],				"First factor after reload")
		checkList(factor(1),	["TestLetters"],				"Second factor after reload")
		checkList(available(),	["TestInts", "TestNominal"],	"Available variables after reload")
	}
}
