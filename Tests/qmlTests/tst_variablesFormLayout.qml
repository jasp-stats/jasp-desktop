import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls


// The theme of the QML test engine does not set its sizes, so this test makes the layout independent of them:
// - the VariablesForm is not placed in the form, whose GridLayout uses these sizes: it gets an explicit size (the controls only need the form to register).
// - the sizes used by a VariablesForm are set in the theme before the form is loaded.
// The form is not placed in the TestCase either: a TestCase is invisible, and the layout of a VariablesForm depends on the visibility of its controls.
Item
{
	width:	1000
	height:	800

	Loader
	{
		id:		formLoader
		active:	false

		sourceComponent: Item
		{
			property alias form:		jaspForm // The form must be in the context of the JASPControls
			property alias varsForm:	varsForm
			property alias allVars:		allVars
			property alias listA:		listA
			property alias singleList:	singleList
			property alias dropDown:	dropDown
			property alias listB:		listB
			property alias fixedList:	fixedList

			Form { id: jaspForm }

			VariablesForm
			{
				id:								varsForm
				width:							800
				height:							500
				// By default these values use preferencesModel.uiScale, which does not exist in the QML test engine
				marginBetweenVariablesLists:	8
				minimumHeightVariablesLists:	25

				AvailableVariablesList	{ name: "allVars";		id: allVars }
				AssignedVariablesList	{ name: "listA";		id: listA;		title: "List A" }
				AssignedVariablesList	{ name: "singleList";	id: singleList;	title: "Single variable";	singleVariable: true }
				DropDown				{ name: "dropDown";		id: dropDown;	label: "Drop down";			values: ["one", "two"] }
				AssignedVariablesList	{ name: "listB";		id: listB }
				AssignedVariablesList	{ name: "fixedList";	id: fixedList;	title: "Fixed size";		height: 90;		width: 150 }
			}
		}
	}

	TestCase
	{
		id:			testCase
		name:		"TestVariablesFormLayout"
		when:		windowShown

		readonly property int	formHeight:		500
		readonly property var	varsForm:		formLoader.item ? formLoader.item.varsForm		: null
		readonly property var	allVars:		formLoader.item ? formLoader.item.allVars		: null
		readonly property var	listA:			formLoader.item ? formLoader.item.listA			: null
		readonly property var	singleList:		formLoader.item ? formLoader.item.singleList	: null
		readonly property var	dropDown:		formLoader.item ? formLoader.item.dropDown		: null
		readonly property var	listB:			formLoader.item ? formLoader.item.listB			: null
		readonly property var	fixedList:		formLoader.item ? formLoader.item.fixedList		: null
		readonly property real	margin:			varsForm ? varsForm.marginBetweenVariablesLists : 0
		readonly property real	titleHeight:	jaspTheme.variablesListTitle

		// Sizes of Desktop/components/JASP/Theme/Theme.qml used by the layout of a VariablesForm
		readonly property var	themeSizes:			({ defaultVariablesFormHeight: 350, defaultSingleItemListHeight: 44, variablesListTitle: 16, comboBoxHeight: 20 })
		property var			savedThemeSizes:	({})

		function initTestCase()
		{
			for (var size in themeSizes)
			{
				savedThemeSizes[size]	= jaspTheme[size]
				jaspTheme[size]			= themeSizes[size]
			}

			formLoader.active = true
			tryCompare(allVars, "count", dataSetInfo.variableCount, 3000, "The form should be set up")
			compare(allVars.height, formHeight, "The available list takes the height of the VariablesForm")
		}

		function cleanupTestCase()
		{
			formLoader.active = false

			for (var size in savedThemeSizes)
				jaspTheme[size] = savedThemeSizes[size]
		}

		function cleanup()
		{
			var form = varsForm

			form.height				= formHeight
			form.removeInvisibles	= false
			form.listWidth			= Qt.binding(function() { return form.width * 2 / 5; })
			dropDown.label			= "Drop down"
			dropDown.visible		= true
			listA.visible			= true
		}

		// The controls must be placed one below the other, and the last one must end at the bottom of the available list
		function checkColumn(controls, message)
		{
			fuzzyCompare(controls[0].y, 0, 0.5, message + ": " + controls[0].name + " is at the top")

			for (var i = 1; i < controls.length; i++)
				fuzzyCompare(controls[i].y, controls[i - 1].y + controls[i - 1].height + margin, 0.5, message + ": " + controls[i].name + " is below " + controls[i - 1].name)

			var last = controls[controls.length - 1]
			fuzzyCompare(last.y + last.height, allVars.height, 0.5, message + ": the column is as high as the available list")
		}

		function test_heights()
		{
			compare(allVars.height, varsForm.height, "The available list takes the height of the VariablesForm")
			checkColumn([listA, singleList, dropDown, listB, fixedList], "All controls visible")
			fuzzyCompare(listA.height - titleHeight, listB.height, 0.5, "The lists with a changeable height get the same height (without their title)")
			verify(listB.height > varsForm.minimumHeightVariablesLists, "The lists do not need the minimum height")
			compare(singleList.height, jaspTheme.defaultSingleItemListHeight, "A single variable list keeps its height")
			compare(fixedList.height, 90, "A list with an explicit height keeps it")
		}

		function test_heightsFollowTheFormHeight()
		{
			var listAHeight = listA.height
			var listBHeight = listB.height

			varsForm.height = formHeight + 100
			compare(allVars.height, formHeight + 100, "The available list follows the height of the VariablesForm")
			fuzzyCompare(listA.height, listAHeight + 50, 0.5, "The 2 lists with a changeable height share the extra height")
			fuzzyCompare(listB.height, listBHeight + 50, 0.5, "The 2 lists with a changeable height share the extra height")
			checkColumn([listA, singleList, dropDown, listB, fixedList], "Higher form")
		}

		function test_minimumHeight()
		{
			varsForm.height = 100
			compare(listB.height, varsForm.minimumHeightVariablesLists, "A list does not get less than the minimum height")
			compare(listA.height, titleHeight + varsForm.minimumHeightVariablesLists, "The title of a list comes on top of the minimum height")
		}

		function test_dropDownHeight()
		{
			compare(dropDown.setLabelAbove, true, "The label of a DropDown is set above its field")

			var dropDownHeight	= dropDown.height
			var listBHeight		= listB.height

			dropDown.label = ""
			verify(dropDown.height < dropDownHeight, "Without label the DropDown is less high")
			fuzzyCompare(listB.height, listBHeight + (dropDownHeight - dropDown.height) / 2, 0.5, "The height freed by the DropDown is shared by the 2 lists")
			checkColumn([listA, singleList, dropDown, listB, fixedList], "DropDown without label")
		}

		function test_invisibleControlKeepsItsPlace()
		{
			var singleListY = singleList.y
			var listBHeight = listB.height

			listA.visible = false
			compare(singleList.y, singleListY, "Without removeInvisibles, an invisible list keeps its place")
			compare(listB.height, listBHeight, "Without removeInvisibles, the other lists keep their height")
		}

		function test_removeInvisibles()
		{
			var listAHeight = listA.height
			var listBHeight = listB.height
			var singleListY = singleList.y

			varsForm.removeInvisibles = true
			checkColumn([listA, singleList, dropDown, listB, fixedList], "removeInvisibles without invisible control")

			listA.visible = false
			checkColumn([singleList, dropDown, listB, fixedList], "listA invisible")
			verify(listB.height > listBHeight, "listB gets the height of the invisible list")
			fuzzyCompare(listA.height, listAHeight, 0.5, "The invisible list already has the height it will get back")

			listA.visible = true
			checkColumn([listA, singleList, dropDown, listB, fixedList], "listA visible again")
			fuzzyCompare(listB.height, listBHeight, 0.5, "listB gets back its height")
			compare(singleList.y, singleListY, "singleList gets back its place")
		}

		function test_removeInvisiblesWithDropDown()
		{
			var listBHeight		= listB.height
			var dropDownSpace	= dropDown.height + margin

			varsForm.removeInvisibles	= true
			dropDown.visible			= false
			checkColumn([listA, singleList, listB, fixedList], "DropDown invisible")
			fuzzyCompare(listB.height, listBHeight + dropDownSpace / 2, 0.5, "The space of the invisible DropDown is shared by the 2 lists")
		}

		function test_widths()
		{
			compare(allVars.x, 0, "The available list is on the left")

			for (var list of [allVars, listA, singleList, listB])
				compare(list.width, varsForm.listWidth, list.name + " gets the list width")

			compare(fixedList.width, 150, "A list with an explicit width keeps it")
			compare(dropDown.fieldWidth, varsForm.listWidth, "The field of a DropDown gets the list width")

			for (var control of [listA, singleList, dropDown, listB, fixedList])
				fuzzyCompare(control.x + control.width, varsForm.width, 0.5, control.name + " is right aligned")
		}

		function test_widthsFollowTheListWidth()
		{
			varsForm.listWidth = 180

			for (var list of [allVars, listA, singleList, listB])
				compare(list.width, 180, list.name + " follows the list width")

			compare(dropDown.fieldWidth, 180, "The field of a DropDown follows the list width")
			fuzzyCompare(listA.x + listA.width, varsForm.width, 0.5, "The lists stay right aligned")
		}
	}
}
