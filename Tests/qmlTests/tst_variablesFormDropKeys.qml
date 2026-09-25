import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls


// The form is not placed in the TestCase: a TestCase is invisible, and only visible lists are used as drop keys.
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVars";	id: allVars }
			AssignedVariablesList	{ name: "listA";	id: listA }
			AssignedVariablesList	{ name: "listB";	id: listB }
			AssignedVariablesList	{ name: "listC";	id: listC }
		}
	}

	TestCase
	{
		name:		"TestVariablesFormDropKeys"

		SignalSpy
		{
			id:				spyLoader
			target:			jaspForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
			compare(allVars.count, dataSetInfo.variableCount, "The available list gets all variables")
		}

		function cleanup()
		{
			// Move the variables back to the available list: the first key of an assigned list is always the available list
			for (var list of [listA, listB, listC])
			{
				for (var i = list.count; i > 0; i--)
					list.itemDoubleClicked(0)

				list.visible = true
				list.enabled = true
			}
		}

		function keys(list)
		{
			return list.dropKeys.join(",")
		}

		function test_dropKeys()
		{
			compare(keys(allVars),	"listA,listB,listC",			"The available list accepts the variables of the assigned lists")
			compare(keys(listA),	"allVars,listA,listB,listC",	"An assigned list accepts the variables of the available list (first key) and of the assigned lists")
			compare(keys(listC),	"allVars,listA,listB,listC",	"An assigned list accepts the variables of the available list (first key) and of the assigned lists")
		}

		function test_invisibleListIsRemovedFromTheDropKeys()
		{
			listB.visible = false
			compare(keys(allVars),	"listA,listC",			"An invisible list is removed from the drop keys")
			compare(keys(listA),	"allVars,listA,listC",	"An invisible list is removed from the drop keys")
			compare(keys(listB),	"allVars,listA,listC",	"The available list stays the first key of an invisible list")

			listB.visible = true
			compare(keys(allVars),	"listA,listB,listC",			"A list that becomes visible again is added back")
			compare(keys(listA),	"allVars,listA,listB,listC",	"A list that becomes visible again is added back")
		}

		function test_disabledListIsRemovedFromTheDropKeys()
		{
			listA.enabled = false
			compare(keys(allVars),	"listB,listC",			"A disabled list is removed from the drop keys")
			compare(keys(listC),	"allVars,listB,listC",	"A disabled list is removed from the drop keys")

			listA.enabled = true
			compare(keys(allVars),	"listA,listB,listC",	"A list that becomes enabled again is added back")
		}

		function test_doubleClickMovesToTheFirstActiveList()
		{
			listA.enabled = false
			listB.visible = false

			allVars.itemDoubleClicked(0)
			compare(listC.count,	1,								"A double click moves the variable to the first visible and enabled list")
			compare(listA.count,	0,								"A disabled list does not get the variable")
			compare(listB.count,	0,								"An invisible list does not get the variable")
			compare(allVars.count,	dataSetInfo.variableCount - 1,	"The variable left the available list")

			listC.itemDoubleClicked(0)
			compare(listC.count,	0,								"A double click in an assigned list moves the variable back to the available list")
			compare(allVars.count,	dataSetInfo.variableCount,		"The variable is back in the available list")
		}

		function test_moveWithoutTargetGoesToTheFirstActiveList()
		{
			// As with the Return or Space key: moveSelectedItems without target uses the first drop key
			listA.visible = false

			allVars.setSelectedItem(0)
			allVars.moveSelectedItems()
			tryCompare(listB, "count", 1, 1000, "The selected variable goes to the first visible and enabled list")
			compare(listA.count, 0, "An invisible list does not get the variable")
		}
	}
}
