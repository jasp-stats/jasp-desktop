import QtTest
import QtQuick
import JASP.Controls
import JASP

// The tooltip of a tab whose name is truncated must show its whole name
TestCase
{
	name:	"TestTabViewToolTip"

	// The controls must be visible and have a size, otherwise the layouts are not computed (and no text is truncated)
	width:		400
	height:		400
	visible:	true
	when:		windowShown

	property alias form: jaspForm // The form must be in the context of the JASPControls

	readonly property string longName:	"A very very long name for a tab that cannot be shown completely"
	readonly property string shortName:	"Short"

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}

	Form
	{
		id:		jaspForm
		width:	400

		TabView
		{
			id:					tabView
			name:				"tabView"
			addItemManually:	false
			values:				[longName, shortName]
			rowComponent:		CheckBox { name: "check" }
		}
	}

	function findItems(item, property, result)
	{
		var children = []
		for (var i = 0; i < item.children.length; i++)
			children.push(item.children[i])

		// The buttons of a TabBar are in its contentModel, not in its children
		if (item.contentModel !== undefined && typeof item.itemAt === "function")
			for (var j = 0; j < item.count; j++)
				children.push(item.itemAt(j))

		for (var child of children)
		{
			if (!child || result.indexOf(child) >= 0)
				continue
			if (child.hasOwnProperty(property))
				result.push(child)
			findItems(child, property, result)
		}
		return result
	}

	function test_toolTipOfTruncatedTab()
	{
		spyLoader.wait(1000)
		tryCompare(tabView, "count", 2)

		var buttons = findItems(tabView.itemTabBar, "toolTipText", [])
		compare(buttons.length, 2, "There are 2 tab buttons")

		tryVerify(function() { return buttons[0].toolTipText !== "" }, 1000, "The long name is truncated, so its tab button has a tooltip")
		compare(buttons[0].toolTipText,	longName,	"The tooltip of a truncated tab is its whole name")
		compare(buttons[1].toolTipText,	"",			"A tab whose name is not truncated has no tooltip")

		// The field used to edit the name of a tab starts with this name
		var fields = findItems(buttons[0], "fieldWidth", [])
		compare(fields.length, 1, "The tab button has a field to edit its name")
		compare(fields[0].value, longName, "The edit field gets the name of the tab")
	}
}
