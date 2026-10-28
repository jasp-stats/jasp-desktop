import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls


// The form is not placed in the TestCase: a TestCase is invisible, and the Tab order skips invisible controls.
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
			id:					varsForm
			preferredHeight:	500

			AvailableVariablesList	{ name: "allVars";	id: allVars }
			AssignedVariablesList	{ name: "listA";	id: listA }
			DropDown				{ name: "dropDown";	id: dropDown;	label: "Drop down";	values: ["one", "two"] }
			AssignedVariablesList	{ name: "listB";	id: listB }
			AssignedVariablesList	{ name: "listC";	id: listC }
			CheckBox				{ name: "checkBox";	id: checkBox;	label: "Check box" }
			Group
			{
				CheckBox			{ name: "groupCheck1";	id: groupCheck1;	label: "Group check 1" }
				CheckBox			{ name: "groupCheck2";	id: groupCheck2;	label: "Group check 2" }
			}
		}
	}

	TestCase
	{
		id:			testCase
		name:		"TestVariablesFormTabOrder"
		when:		windowShown

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
			verify(assignButtonOf(listA) && assignButtonOf(listB) && assignButtonOf(listC), "Each assigned list has an assign button")
		}

		function cleanup()
		{
			for (var control of [listA, dropDown, listB, listC, checkBox])
			{
				control.visible = true
				control.enabled = true
			}
		}

		function assignButtonOf(list)
		{
			for (var i = 0; i < varsForm.children.length; i++)
				if (varsForm.children[i].rightSource === list)
					return varsForm.children[i]

			return null
		}

		// Name of the control (or assign button) containing the item with the active focus
		function focusedName()
		{
			var named = [	[allVars, "allVars"], [listA, "listA"], [dropDown, "dropDown"], [listB, "listB"], [listC, "listC"], [checkBox, "checkBox"],
							[groupCheck1, "groupCheck1"], [groupCheck2, "groupCheck2"],
							[assignButtonOf(listA), "buttonA"], [assignButtonOf(listB), "buttonB"], [assignButtonOf(listC), "buttonC"]	]

			for (var item = testCase.Window.activeFocusItem; item; item = item.parent)
				for (var pair of named)
					if (pair[0] === item)
						return pair[1]

			return "none"
		}

		function tabSequence(start, count)
		{
			start.forceActiveFocus()

			var names = []
			for (var i = 0; i < count; i++)
			{
				keyClick(Qt.Key_Tab)
				names.push(focusedName())
			}

			return names
		}

		function test_tabTargets()
		{
			var buttonA = assignButtonOf(listA)
			var buttonB = assignButtonOf(listB)
			var buttonC = assignButtonOf(listC)

			compare(allVars.KeyNavigation.tab,					buttonA,	"The available list goes to the first assign button")
			compare(buttonA.KeyNavigation.tab,					buttonB,	"An assign button goes to the next assign button")
			compare(buttonB.KeyNavigation.tab,					buttonC,	"An assign button goes to the next assign button")
			compare(buttonC.KeyNavigation.tab,					listA,		"The last assign button goes to the first control")
			compare(listA.KeyNavigation.tab,					dropDown,	"A DropDown between the lists is in the Tab order")
			compare(dropDown.innerControl.KeyNavigation.tab,	listB,		"The ComboBox of a DropDown handles the Tab key itself, so it gets the Tab order")
			compare(listB.KeyNavigation.tab,					listC,		"A list goes to the next control")
			compare(listC.KeyNavigation.tab,					checkBox,	"A CheckBox is in the Tab order")
			compare(checkBox.innerControl.KeyNavigation.tab,	null,		"A Group does not take the focus itself: Qt's Tab handling moves the focus to its children")
		}

		function test_tabTargetsSkipInvisibleAndDisabledControls()
		{
			listA.enabled = false
			compare(allVars.KeyNavigation.tab,					assignButtonOf(listB),	"The assign button of a disabled list is skipped")
			compare(assignButtonOf(listC).KeyNavigation.tab,	dropDown,				"A disabled list is skipped")

			listB.visible = false
			compare(allVars.KeyNavigation.tab,					assignButtonOf(listC),	"The assign button of an invisible list is skipped")
			compare(dropDown.innerControl.KeyNavigation.tab,	listC,					"An invisible list is skipped")

			dropDown.enabled = false
			compare(assignButtonOf(listC).KeyNavigation.tab,	listC,					"A disabled DropDown is skipped")

			listC.visible		= false
			checkBox.visible	= false
			compare(allVars.KeyNavigation.tab,					null,					"Only the Group is left: Qt's Tab handling moves the focus to its children")
		}

		function test_tabKey()
		{
			if (!testCase.Window.active)
				skip("The focus can only be tested in an active window (e.g. with QT_QPA_PLATFORM=offscreen)")

			compare(tabSequence(allVars, 10),
					["buttonA", "buttonB", "buttonC", "listA", "dropDown", "listB", "listC", "checkBox", "groupCheck1", "groupCheck2"],
					"The Tab key goes from the available list to the assign buttons and then to all controls of the form")
		}

		function test_tabKeySkipsInvisibleAndDisabledLists()
		{
			if (!testCase.Window.active)
				skip("The focus can only be tested in an active window (e.g. with QT_QPA_PLATFORM=offscreen)")

			// As ClassicalMantelHaenszelPeto.qml with its default method: the lists after the DropDown are invisible and disabled
			listB.visible = false
			listB.enabled = false
			listC.visible = false
			listC.enabled = false

			compare(tabSequence(allVars, 5), ["buttonA", "listA", "dropDown", "checkBox", "groupCheck1"], "The Tab key does not stop at an invisible or disabled list")
		}
	}
}
