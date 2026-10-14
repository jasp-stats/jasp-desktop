import QtQuick
import JASP.Controls
import JASP

// Section "Simple Factors Form" of the jaspTestModule analysis "Test Factors Form" (testFactorsForm.qml):
// a FactorsForm as in the CFA analysis, with a TabView having a tab per factor. Each tab has a VariablesList with
// as source the VariablesList of its factor, and a DropDown per variable with the titles of the factors.
SpecialSourcesTestCase
{
	name:	"TestFactorsFormSimple"

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

		FactorsForm
		{
			id:					simpleFactors
			name:				"factors"
			initNumberFactors:	1
			allowedColumns:		["ordinal", "scale"]
			allowTypeChange:	true
		}

		TabView
		{
			id:					factorsList
			name:				"factorsList"
			values:				simpleFactors.factorsTitles
			addItemManually:	false

			rowComponent: VariablesList
			{
				implicitWidth	: form.width - 2 * jaspTheme.generalAnchorMargin
				implicitHeight	: jaspTheme.smallDefaultVariablesFormHeight

				draggable		: false
				source			: simpleFactors.factorsItems[rowIndex]
				listViewType	: JASP.AssignedVariables
				name			: "newfactors"

				rowComponent	: Row
				{
					CheckBox { name: "check" }
					DropDown { name: "drop"; values: simpleFactors.factorsTitles }
				}
			}
		}
	}

	function available()		{ return simpleFactors.availableVariablesList }
	function factor(i)			{ return simpleFactors.factorsItems[i] }
	function tabList(tab)		{ return rowControl(factorsList, tab, "newfactors") }
	function check(tab, name)	{ return rowControl(tabList(tab), name, "check") }
	function drop(tab, name)	{ return rowControl(tabList(tab), name, "drop") }

	function tabLabels()
	{
		// The labels shown in the tab buttons
		return findItems(factorsList.itemTabBar, "toolTipText", []).map(function(button) { return button.text !== undefined && button.text !== "" ? button.text : findItems(button, "elide", [])[0].text })
	}

	function test_simpleFactorsForm()
	{
		compare(JSON.stringify(simpleFactors.factorsTitles), JSON.stringify([{label: "Factor 1", value: "Factor1"}]), "The titles start with 'Factor 1'")
		checkList(factorsList, ["Factor1"], "The TabView has a tab per factor")

		moveVariables(available(), factor(0), ["TestDoubles", "TestInts"])
		checkLockState(factor(0), ["TestDoubles", "TestInts"], "Without nesting, the variables can be moved")
		checkList(tabList("Factor1"),			["TestDoubles", "TestInts"],	"The VariablesList of the tab gets the variables of its factor")
		checkList(drop("Factor1", "TestInts"),	["Factor1"],					"The DropDown gets the factors")

		// No interaction: the available list gives the variables only once
		checkList(available(), ["TestLetters", "TestNominal"], "The assigned variables are removed from the available list")

		simpleFactors.addFactor()
		checkList(factorsList, ["Factor1", "Factor2"], "A tab is added with the factor")
		checkList(tabList("Factor2"),				[],						"The new factor has no variables yet")
		checkList(drop("Factor1", "TestInts"),		["Factor1", "Factor2"],	"The DropDown gets the added factor")
		checkList(tabList("Factor1"),				["TestDoubles", "TestInts"], "The first tab keeps its variables")

		moveVariables(available(), factor(1), ["TestLetters"])
		checkList(tabList("Factor2"), ["TestLetters"], "The VariablesList of the second tab gets the variables of its factor")
		compare(factor(1).getVariableType("TestLetters"),		2, "The nominal TestLetters is used as ordinal (only ordinal and scale are allowed)")
		// Known issue: since 85024a2c01, ListModel::_setAllowedType sets the type of a term back to the real type of its variable
		// when this real type is allowed (here the VariablesList of the tab allows all types), so the type of its source is lost.
		var tabListOfFactor2 = tabList("Factor2")
		expectFailContinue("", "The VariablesList of the tab gets the real type of the variable instead of the type in its source")
		compare(tabListOfFactor2.getVariableType("TestLetters"),	2, "The VariablesList of the tab gets the type of the variable in its factor")

		// Set some values in the tabs
		check("Factor1", "TestInts").click()
		drop("Factor1", "TestInts").currentValue		= "Factor2"
		drop("Factor2", "TestLetters").currentValue	= "Factor1"
		compare(check("Factor1", "TestInts").checked,			true)
		compare(drop("Factor1", "TestInts").currentValue,		"Factor2")

		// Rename a factor: the tab and the DropDowns get the new title, and keep their values
		simpleFactors.titleChanged(1, "Anxiety")
		tryVerify(function() { return JSON.stringify(simpleFactors.factorsTitles) === JSON.stringify([{label: "Factor 1", value: "Factor1"}, {label: "Anxiety", value: "Factor2"}]) }, 1000, "The FactorsForm has the new title")
		tryVerify(function() { return tabLabels().indexOf("Anxiety") >= 0 }, 1000, "The tab gets the new title, but has " + JSON.stringify(tabLabels()))
		tryCompare(drop("Factor1", "TestInts"), "currentText", "Anxiety", 1000, "The DropDown shows the new title")
		compare(drop("Factor1", "TestInts").currentValue,	"Factor2",		"The DropDown keeps its value")
		compare(check("Factor1", "TestInts").checked,		true,			"The tab keeps its values")
		checkList(tabList("Factor2"), ["TestLetters"], "The renamed tab keeps its variables")

		// Change the type of a variable in a factor: the VariablesList of the tab gets this type
		factor(0).setVariableType(toArray(factor(0).columnsNames).indexOf("TestDoubles"), 2)
		tryVerify(function() { return factor(0).getVariableType("TestDoubles") === 2 }, 1000, "TestDoubles is now ordinal in its factor")
		tryVerify(function() { return tabList("Factor1").getVariableType("TestDoubles") === 2 }, 1000, "The VariablesList of the tab gets the changed type")

		// Duplicate the analysis: the controls should have the same values.
		// The FactorsForm updates its value asynchronously when the variables of a factor change: wait for it before getting the options.
		tryVerify(function()
		{
			var factorsOption = form.options()["factors"]
			return factorsOption.length === 2 && JSON.stringify(factorsOption[0]["indicators"]["types"]) === JSON.stringify(["ordinal", "ordinal"])
		}, 1000, "The option of the FactorsForm gets the variables and types of its factors")
		var options = form.options()
		reloadOptions(options)
		checkList(factorsList,			["Factor1", "Factor2"],			"Tabs after reload")
		checkList(factor(0),			["TestDoubles", "TestInts"],	"First factor after reload")
		checkList(factor(1),			["TestLetters"],				"Second factor after reload")
		checkList(tabList("Factor1"),	["TestDoubles", "TestInts"],	"First tab after reload")
		checkList(tabList("Factor2"),	["TestLetters"],				"Second tab after reload")
		compare(check("Factor1", "TestInts").checked,			true,		"CheckBox restored after reload")
		compare(drop("Factor1", "TestInts").currentValue,		"Factor2",	"DropDown restored after reload")
		compare(drop("Factor2", "TestLetters").currentValue,	"Factor1",	"DropDown restored after reload")
		compare(drop("Factor1", "TestInts").currentText,		"Anxiety",	"The renamed title is restored after reload")
		// Known issue: since 85024a2c01, ListModel::_setAllowedType sets the type of a term back to the real type of its variable
		// when this real type is allowed, so a type changed by the user is lost when the options are loaded.
		expectFailContinue("", "A changed type is set back to the real type of the variable when the options are loaded")
		compare(factor(0).getVariableType("TestDoubles"),		2,			"The changed type is kept after reload")

		// Remove a factor: its tab is removed, and the DropDowns do not have it anymore
		simpleFactors.removeFactor()
		checkList(factorsList,					["Factor1"],					"The tab of the removed factor is removed")
		checkList(drop("Factor1", "TestInts"),	["Factor1"],					"The DropDown does not have the removed factor anymore")
		compare(drop("Factor1", "TestInts").currentValue, "Factor1",			"The DropDown value of the removed factor is replaced")
		checkList(available(),					["TestLetters", "TestNominal"],	"The variables of the removed factor are available again")
	}
}
