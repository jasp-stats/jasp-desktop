import QtQuick
import JASP.Controls
import JASP

// A FactorsForm used as source with 'use: "title"' gives the titles of its factors.
// Adding, removing or renaming a factor must be seen in the controls having this source.
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesFactorsForm"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		FactorsForm
		{
			id					: factors
			name				: "factors"
			initNumberFactors	: 2
		}

		ComponentsList
		{
			id				: factorsList
			name			: "factorsList"
			source			: [{ name: "factors", use: "title" }]
			rowComponent	: TextField { name: "text" }
		}

		DropDown
		{
			id		: factorsDropDown
			name	: "factorsDropDown"
			source	: [{ name: "factors", use: "title" }]
		}
	}

	function text(title) { return rowControl(factorsList, title, "text") }

	function test_factorsTitles()
	{
		checkList(factorsList,		["Factor 1", "Factor 2"],	"ComponentsList gets the titles of the factors")
		checkList(factorsDropDown,	["Factor 1", "Factor 2"],	"DropDown gets the titles of the factors")
		compare(factorsDropDown.currentValue, "Factor 1")

		text("Factor 1").value = "first"
		text("Factor 2").value = "second"
		factorsDropDown.currentValue = "Factor 2"

		factors.addFactor()
		checkList(factorsList,		["Factor 1", "Factor 2", "Factor 3"],	"ComponentsList gets the added factor")
		checkList(factorsDropDown,	["Factor 1", "Factor 2", "Factor 3"],	"DropDown gets the added factor")
		compare(text("Factor 1").value,			"first",	"The row values are kept when a factor is added")
		compare(text("Factor 2").value,			"second",	"The row values are kept when a factor is added")
		compare(text("Factor 3").value,			"",			"The new row has the default value")
		compare(factorsDropDown.currentValue,	"Factor 2",	"The DropDown value is kept when a factor is added")

		factors.titleChanged(0, "Anxiety")
		checkList(factorsList,		["Anxiety", "Factor 2", "Factor 3"],	"ComponentsList gets the new title")
		checkList(factorsDropDown,	["Anxiety", "Factor 2", "Factor 3"],	"DropDown gets the new title")
		compare(text("Factor 2").value,			"second",	"The values of the other rows are kept when a factor is renamed")
		compare(factorsDropDown.currentValue,	"Factor 2")

		factors.titleChanged(1, "Depression")
		checkList(factorsDropDown, ["Anxiety", "Depression", "Factor 3"], "DropDown gets the new title")
		compare(factorsDropDown.currentValue, "Depression", "The DropDown value follows the renamed factor")

		factors.removeFactor()
		checkList(factorsList,		["Anxiety", "Depression"],	"ComponentsList without the removed factor")
		checkList(factorsDropDown,	["Anxiety", "Depression"],	"DropDown without the removed factor")
		compare(factorsDropDown.currentValue, "Depression")

		factors.removeFactor()
		checkList(factorsList,		["Anxiety"],	"ComponentsList without the removed factor")
		checkList(factorsDropDown,	["Anxiety"],	"DropDown without the removed factor")
		compare(factorsDropDown.currentValue, "Anxiety", "The value of the removed factor is replaced")

		factors.addFactor()
		checkList(factorsList, ["Anxiety", "Factor 2"], "ComponentsList gets the added factor")
		text("Factor 2").value = "second again"

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		text("Factor 2").value = ""
		reloadOptions(options)
		checkList(factorsList,		["Anxiety", "Factor 2"],	"ComponentsList after reload")
		checkList(factorsDropDown,	["Anxiety", "Factor 2"],	"DropDown after reload")
		compare(text("Factor 2").value,			"second again")
		compare(factorsDropDown.currentValue,	"Anxiety")

		// The row of a renamed factor keeps its values
		text("Anxiety").value = "anxious"
		factors.titleChanged(0, "Fear")
		checkList(factorsList,		["Fear", "Factor 2"],	"ComponentsList gets the new title")
		checkList(factorsDropDown,	["Fear", "Factor 2"],	"DropDown gets the new title")
		compare(text("Fear").value,				"anxious",		"The row of a renamed factor keeps its values")
		compare(text("Factor 2").value,			"second again",	"The other rows keep their values")
		compare(factorsDropDown.currentValue,	"Fear",			"The DropDown value follows the renamed factor")

		options = form.options()
		text("Fear").value = ""
		reloadOptions(options)
		checkList(factorsList, ["Fear", "Factor 2"], "ComponentsList after a reload following a rename")
		compare(text("Fear").value,				"anxious")
		compare(text("Factor 2").value,			"second again")
		compare(factorsDropDown.currentValue,	"Fear")
	}
}
