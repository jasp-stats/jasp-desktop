import QtQuick
import JASP.Controls
import JASP

// A FactorsForm used as source with 'use: "title"' by an AvailableVariablesList, as for the Second-Order factors in the CFA (jaspFactor).
// Adding, removing or renaming a factor must be seen in the available and the assigned lists.
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesFactorsFormVariables"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		FactorsForm
		{
			id					: factors
			name				: "factors"
			initNumberFactors	: 3
		}

		VariablesForm
		{
			AvailableVariablesList	{ name: "availableFactors";	id: availableFactors;	source: [{ name: "factors", use: "title" }] }
			AssignedVariablesList	{ name: "secondOrder";		id: secondOrder			}
		}
	}

	function test_factorsAsVariables()
	{
		checkList(availableFactors,	["Factor 1", "Factor 2", "Factor 3"],	"The available list gets the titles of the factors")
		checkList(secondOrder,		[],										"Nothing assigned yet")

		moveVariables(availableFactors, secondOrder, ["Factor 1", "Factor 3"])
		checkList(availableFactors,	["Factor 2"],				"Assigned factors are removed from the available list")
		checkList(secondOrder,		["Factor 1", "Factor 3"],	"Factors assigned")

		factors.addFactor()
		checkList(availableFactors,	["Factor 2", "Factor 4"],	"The added factor is available")
		checkList(secondOrder,		["Factor 1", "Factor 3"],	"The assigned factors are kept when a factor is added")

		factors.titleChanged(1, "Depression")
		checkList(availableFactors,	["Depression", "Factor 4"],	"A renamed available factor gets its new title")
		checkList(secondOrder,		["Factor 1", "Factor 3"],	"The assigned factors are kept when an available factor is renamed")

		factors.removeFactor()
		checkList(availableFactors,	["Depression"],				"A removed available factor is removed from the available list")
		checkList(secondOrder,		["Factor 1", "Factor 3"],	"The assigned factors are kept when an available factor is removed")

		factors.removeFactor()
		checkList(availableFactors,	["Depression"],	"Removing an assigned factor does not change the available list")
		checkList(secondOrder,		["Factor 1"],	"A removed assigned factor is removed from the assigned list")

		factors.addFactor()
		checkList(availableFactors,	["Depression", "Factor 3"],	"The added factor is available")

		moveVariables(availableFactors, secondOrder, ["Factor 3"])
		checkList(secondOrder, ["Factor 1", "Factor 3"], "Factor assigned")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		moveVariables(secondOrder, availableFactors, ["Factor 1"])
		reloadOptions(options)
		checkList(availableFactors,	["Depression"],				"Available factors after reload")
		checkList(secondOrder,		["Factor 1", "Factor 3"],	"Assigned factors after reload")

		// A renamed assigned factor stays assigned with its new title
		factors.titleChanged(0, "Anxiety")
		checkList(secondOrder,		["Anxiety", "Factor 3"],	"A renamed assigned factor gets its new title in the assigned list")
		checkList(availableFactors,	["Depression"],				"A renamed assigned factor is not available")

		options = form.options()
		moveVariables(secondOrder, availableFactors, ["Anxiety"])
		reloadOptions(options)
		checkList(availableFactors,	["Depression"],				"Available factors after a reload following a rename")
		checkList(secondOrder,		["Anxiety", "Factor 3"],	"Assigned factors after a reload following a rename")
	}
}
