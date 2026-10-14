import QtQuick
import JASP.Controls
import JASP

// Section "Nested Factors Form" of the jaspTestModule analysis "Test Factors Form" (testFactorsForm.qml):
// the n+1th model gets all the variables of the nth model, and these variables are locked (not movable) in the n+1th model.
SpecialSourcesTestCase
{
	name:	"TestFactorsFormNested"

	// The controls must be visible and have a size, otherwise the layouts are not computed
	width:		800
	height:		2000
	visible:	true
	when:		windowShown

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm
		width:	800

		VariablesForm
		{
			AvailableVariablesList	{ id: availableVariables;	name: "availableVariables";	title: "Available variables"	}
			AssignedVariablesList	{ id: variables;			name: "variables";			title: "Selected variables"		}
		}

		FactorsForm
		{
			id:								factors
			name:							"nestedFactors"
			nested:							nested.checked
			allowInteraction:				true
			initNumberFactors:				2
			baseName:						"model"
			baseTitle:						"Model"
			availableVariablesList.source:	"variables"
			startIndex:						0
			availableVariablesListName:		"availableFactors"
			allowedColumns:					["ordinal", "scale"]
			allowTypeChange:				true
		}

		CheckBox
		{
			id:			nested
			label:		"Nested"
			name:		"nested"
			checked:	true
		}
	}

	function model(i)		{ return factors.factorsItems[i] }
	function available()	{ return factors.availableVariablesList }

	function removeButton()
	{
		var buttons = findItems(factors, "toolTip", []).filter(function(item) { return item.toolTip === "Remove last Model" })
		compare(buttons.length, 1, "The FactorsForm has a button to remove the last model")
		return buttons[0]
	}

	function checkModels(expected, message)
	{
		tryCompare(factors.factorsItems, "length", expected.length, 1000, message + ": number of models")
		for (var i = 0; i < expected.length; i++)
			checkLockState(model(i), expected[i], message + ": Model " + i)
	}

	function test_nestedFactorsForm()
	{
		compare(JSON.stringify(factors.factorsTitles), JSON.stringify([{label: "Model 0", value: "model0"}, {label: "Model 1", value: "model1"}]), "The titles start with 'Model 0'")
		verify(!removeButton().enabled, "The minimum number of models is 2: the remove button is disabled")

		// The selected variables are added automatically to the last model
		moveVariables(availableVariables, variables, ["TestDoubles", "TestInts", "TestLetters"])
		checkList(available(), ["TestDoubles", "TestInts", "TestLetters"], "The available list of the FactorsForm gets the selected variables")
		checkModels([[], ["TestDoubles", "TestInts", "TestLetters"]], "Selected variables are added to the last model")

		// A new model gets all the variables of the model before, locked
		factors.addFactor()
		checkModels([[], ["TestDoubles", "TestInts", "TestLetters"], ["TestDoubles(locked)", "TestInts(locked)", "TestLetters(locked)"]], "Model added")
		verify(removeButton().enabled, "With 3 models, the remove button is enabled")

		// Move a variable from a model to a model below it: it stays (locked) in the models above
		moveTerms(model(1), model(0), ["TestDoubles"])
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestInts", "TestLetters"], ["TestDoubles(locked)", "TestInts(locked)", "TestLetters(locked)"]], "TestDoubles moved from Model 1 to Model 0")

		// Move a variable from a model to a model above it: it is removed from the models below, and becomes movable in its new model
		moveTerms(model(0), model(2), ["TestDoubles"])
		checkModels([[], ["TestInts", "TestLetters"], ["TestInts(locked)", "TestLetters(locked)", "TestDoubles"]], "TestDoubles moved from Model 0 to Model 2")

		moveTerms(model(2), model(0), ["TestDoubles"])
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestInts", "TestLetters"], ["TestDoubles(locked)", "TestInts(locked)", "TestLetters(locked)"]], "TestDoubles moved from Model 2 to Model 0")

		// Remove a variable from a model: it is removed also from the models above
		moveTerms(model(1), available(), ["TestInts"])
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestLetters"], ["TestDoubles(locked)", "TestLetters(locked)"]], "TestInts removed from Model 1")
		checkList(available(), ["TestDoubles", "TestInts", "TestLetters"], "The available list keeps all variables (interactions are allowed)")

		moveTerms(available(), model(2), ["TestInts"])
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestLetters"], ["TestDoubles(locked)", "TestLetters(locked)", "TestInts"]], "TestInts added to Model 2")

		// Without nesting, all variables are movable, and no variable is added to the models above
		nested.click()
		checkModels([["TestDoubles"], ["TestDoubles", "TestLetters"], ["TestDoubles", "TestLetters", "TestInts"]], "Not nested: all variables are movable")
		moveTerms(model(2), available(), ["TestDoubles"])
		checkModels([["TestDoubles"], ["TestDoubles", "TestLetters"], ["TestLetters", "TestInts"]], "Not nested: a variable can be removed from a model only")

		// With nesting again, the constraints are applied again
		nested.click()
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestLetters"], ["TestDoubles(locked)", "TestLetters(locked)", "TestInts"]], "Nested again: the models get the variables of the models below")

		// Change the type of a variable
		compare(model(2).getVariableType("TestInts"), 2, "TestInts is ordinal")
		model(2).setVariableType(toArray(model(2).columnsNames).indexOf("TestInts"), 1)
		tryVerify(function() { return model(2).getVariableType("TestInts") === 1 }, 1000, "TestInts is now scale")
		compare(model(1).getVariableType("TestLetters"), 2, "The nominal TestLetters is used as ordinal (only ordinal and scale are allowed)")

		// Duplicate the analysis: the controls should have the same values, with the same types.
		// The FactorsForm updates its value asynchronously when the variables of a model change: wait for it before getting the options.
		tryVerify(function()
		{
			var models = form.options()["nestedFactors"]
			return models.length === 3 && JSON.stringify(models[2]["components"]["types"]) === JSON.stringify(["scale", "ordinal", "scale"])
		}, 1000, "The option of the FactorsForm gets the variables and types of its models")
		var options = form.options()
		reloadOptions(options)
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestLetters"], ["TestDoubles(locked)", "TestLetters(locked)", "TestInts"]], "After reload")
		// Known issue: since 85024a2c01, ListModel::_setAllowedType sets the type of a term back to the real type of its variable
		// when this real type is allowed, so a type changed by the user is lost when the options are loaded.
		expectFailContinue("", "A changed type is set back to the real type of the variable when the options are loaded")
		compare(model(2).getVariableType("TestInts"),		1, "The changed type is kept after reload")
		compare(model(1).getVariableType("TestLetters"),	2, "The type of TestLetters is kept after reload")

		// Remove the last model: the remove button is then disabled again
		removeButton().clicked()
		checkModels([["TestDoubles"], ["TestDoubles(locked)", "TestLetters"]], "Last model removed")
		verify(!removeButton().enabled, "With 2 models, the remove button is disabled again")
	}
}
