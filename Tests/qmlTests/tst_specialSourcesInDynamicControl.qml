import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

// Section "Source in a dynamic created control" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesInDynamicControl"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList6";	id: allVariablesList6	}
			AssignedVariablesList	{ name: "assigned";				id: assigned			}
		}

		ComponentsList
		{
			id				: compList
			name			: "compList"
			depends			: "assigned"
			minimumItems	: 1
			rowComponent	: RowLayout
			{
				DropDown
				{
					name			: 'dropDownValuesa'
					source			: 'assigned'
					addEmptyValue	: true
				}
			}
		}
	}

	function dropDown(row) { return rowControl(compList, compList.columnsNames[row], "dropDownValuesa") }

	function test_sourceInDynamicControl()
	{
		compare(compList.count, 1, "The ComponentsList has a minimum of 1 item")
		checkList(dropDown(0), [""], "No variable assigned yet")

		moveVariables(allVariablesList6, assigned, ["TestDoubles", "TestInts", "TestLetters"])
		checkList(dropDown(0), ["", "TestDoubles", "TestInts", "TestLetters"], "The DropDown gets the assigned variables")

		compList.addItem()
		tryCompare(compList, "count", 2)
		checkList(dropDown(1), ["", "TestDoubles", "TestInts", "TestLetters"], "A new DropDown gets directly the right values")

		dropDown(0).currentValue = "TestInts"
		dropDown(1).currentValue = "TestLetters"
		compare(dropDown(0).currentValue, "TestInts")
		compare(dropDown(1).currentValue, "TestLetters")

		// Load the options (as when a JASP file is loaded) while the source is empty: the source must be set
		// before the DropDowns, otherwise their values would not be in their option values.
		var options = form.options()
		moveVariables(assigned, allVariablesList6, ["TestDoubles", "TestInts", "TestLetters"])
		checkList(dropDown(0), [""], "All variables are removed")
		checkList(dropDown(1), [""], "All variables are removed")
		reloadOptions(options)

		checkList(assigned, ["TestDoubles", "TestInts", "TestLetters"], "Assigned variables after reload")
		compare(compList.count, 2)
		checkList(dropDown(0), ["", "TestDoubles", "TestInts", "TestLetters"], "First DropDown values after reload")
		checkList(dropDown(1), ["", "TestDoubles", "TestInts", "TestLetters"], "Second DropDown values after reload")
		compare(dropDown(0).currentValue, "TestInts",		"First DropDown value is restored")
		compare(dropDown(1).currentValue, "TestLetters",	"Second DropDown value is restored")
	}
}
