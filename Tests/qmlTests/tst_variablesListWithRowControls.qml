import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls


TestCase
{
	name:		"TestVariablesListWithRowControls"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}

	SignalSpy
	{
		id:				spyAllVarsChanged
		target:			allVars
		signalName:		"countChanged"
	}

	SignalSpy
	{
		id:				spyAssignedVarsChanged
		target:			assignedVars
		signalName:		"countChanged"
	}

	SignalSpy
	{
		id:				spyCheckedVarsChanged
		target:			checkedVars
		signalName:		"countChanged"
	}

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			id:			varsForm
			AvailableVariablesList{  name: "allVars";		id: allVars }
			AssignedVariablesList {  name: "assignedVars";	id:	assignedVars; rowComponent: CheckBox {name: "check"}}  //		title: qsTr("Variables"); allowedColumns: ["scale"]; minNumericLevels: 2 }
		}

		AssignedVariablesList { name: "checkedVars";	id: checkedVars; source: [{name: "assignedVars", condition: "check" }] }
	}

	function test_variablesListWithRowControls()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1);
		compare(allVars.count, dataSetInfo.variableCount,					"The Available VariablesList gets all variables");

		allVars.setSelectedItem(0);
		allVars.addSelectedItem(1);
		spyAllVarsChanged.clear();
		allVars.moveSelectedItems(assignedVars);
		spyAllVarsChanged.wait(400);
		compare(spyAllVarsChanged.count,	1);
		compare(assignedVars.count,			2,								"The 2 first variables have been selected and moved to the Assigned VariablesList");
		compare(allVars.count,				dataSetInfo.variableCount - 2,	"2 first variables are now missing in the Available VariablesList");
		compare(checkedVars.count,			0,								"The checkedVars VariablesList has for source the Assigned VariablesList, but with condition that the associated CheckBox is checked: no CheckBox is checked, so 0 variable is in the VariableList");

		var columns = assignedVars.columnsNames
		compare(columns.length,				2);
		var checkBox = assignedVars.getRowControl(columns[1], "check")
		spyCheckedVarsChanged.clear()
		checkBox.click()
		spyCheckedVarsChanged.wait(400);
		compare(checkedVars.count,			1,								"The CheckBox of the 2nd variable has been checked, so the the checkedVars VariablesList has one variable");
		compare(checkedVars.columnsNames[0], assignedVars.columnsNames[1])

		spyAssignedVarsChanged.clear();
		assignedVars.setSelectedItem(1);
		assignedVars.moveSelectedItems(allVars);
		spyAssignedVarsChanged.wait(400);
		compare(assignedVars.count,			1,								"The second variable is back to the Available VariablesList, so only 1 variable is left on the assigned VariablesList")
		compare(checkedVars.count,			0,								"No checked variable left in Assigned VariablesList, so checkedVars VariablesList has no variable")
	}
}
