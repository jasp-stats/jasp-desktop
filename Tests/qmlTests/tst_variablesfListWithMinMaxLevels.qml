import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls
import JASP


TestCase
{
	name:		"TestVariablesListWithMinMaxLevels"

	property alias form: jaspForm

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}


	SignalSpy
	{
		id:				spyAssigned
		target:			assignedVars
		signalName:		"countChanged"
	}

	SignalSpy
	{
			id:			spyError
			target:		assignedVars
			signalName: "hasErrorChanged"
	}



	Form
	{
		id:		jaspForm

		VariablesForm
		{
			id:			varsForm
			AvailableVariablesList{  name: "allVars";			id: allVars }
			AssignedVariablesList {  name: "assignedVars";		id:	assignedVars; }  //		title: qsTr("Variables"); allowedColumns: ["scale"]; minNumericLevels: 2 }
		}
	}

	function test_variableListsWithMinMaxLevels()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count,				1);
		compare(allVars.count,					dataSetInfo.variableCount);

		spyAssigned.clear()
		allVars.setSelectedItem(0)
		allVars.moveSelectedItems(assignedVars);
		spyAssigned.wait(400);
		compare(spyAssigned.count,				1);
		compare(allVars.count,					dataSetInfo.variableCount - 1);
		compare(assignedVars.count,				1);

		spyAssigned.clear()
		assignedVars.setSelectedItem(0)
		assignedVars.moveSelectedItems(allVars);
		spyAssigned.wait(500);
		compare(spyAssigned.count,				1);
		compare(allVars.count,					dataSetInfo.variableCount);
		compare(assignedVars.count,				0);

		// Test min/max Levels checks
		allVars.setSelectedItemWithName("TestLetters")
		allVars.moveSelectedItems(assignedVars);
		spyAssigned.wait(500);
		compare(allVars.count,					dataSetInfo.variableCount - 1);
		compare(assignedVars.count,				1);
		compare(assignedVars.columnsNames[0],	"TestLetters")


		assignedVars.minLevels = 10;			//TestLetters has 5 levels
		compare(assignedVars.hasError,			true);

		assignedVars.minLevels = 2;
		compare(assignedVars.hasError,			false);

		assignedVars.maxLevels = 2;
		compare(assignedVars.hasError,			true);

		assignedVars.maxLevels = 5;
		compare(assignedVars.hasError,			false);

		assignedVars.setSelectedItem(0)
		assignedVars.moveSelectedItems(allVars);

		// Test min/max Numeric Levels checks
		allVars.setSelectedItemWithName("TestDoubles")
		allVars.moveSelectedItems(assignedVars);
		spyAssigned.wait(500);
		compare(allVars.count,					dataSetInfo.variableCount - 1);
		compare(assignedVars.count,				1);
		compare(assignedVars.columnsNames[0],	"TestDoubles")

		assignedVars.minNumericLevels = 10;		//TestDoubles has 5 different values
		compare(assignedVars.hasError,			true);

		assignedVars.minNumericLevels = 2;
		compare(assignedVars.hasError,			false);

		assignedVars.maxNumericLevels = 2;
		compare(assignedVars.hasError,			true);

		assignedVars.maxNumericLevels = 5;
		compare(assignedVars.hasError,			false);

	}
}
