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
		id:				spyFactorLevelListChanged
		target:			repeatedMeasuresFactors.model
		signalName:		"dataChanged"
	}

	SignalSpy
	{
		id:				spyRepeatedMeasuresCells
		target:			repeatedMeasuresCells
		signalName:		"columnsNamesChanged"
	}


	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList			{ name: "allVariablesList"; id: allVars }
			FactorLevelList					{ name: "repeatedMeasuresFactors";	id: repeatedMeasuresFactors; title: qsTr("Repeated Measures Factors"); factorName: qsTr("RM Factor")		}
			AssignedRepeatedMeasuresCells	{ name: "repeatedMeasuresCells";	id: repeatedMeasuresCells; title: qsTr("Repeated Measures Cells");	source: "repeatedMeasuresFactors"	}
		}
	}

	function test_repeatedMeasures()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1);
		compare(allVars.count, dataSetInfo.variableCount,					"The Available VariablesList gets all variables");
		compare(repeatedMeasuresFactors.factors.length, 1, "One default factor");
		compare(repeatedMeasuresFactors.factors[0], "RM Factor 1", "Default factor name is \"RM Factor 1\"");
		compare(repeatedMeasuresFactors.factorLevelMap["RM Factor 1"].length, 2, "2 default levels");
		compare(repeatedMeasuresFactors.factorLevelMap["RM Factor 1"][0], "Level 1", "First default level is \"Level 1\"");
		compare(repeatedMeasuresFactors.factorLevelMap["RM Factor 1"][1], "Level 2", "Second default level is \"Level 2\"");
		compare(repeatedMeasuresCells.columnsNames.length, 2, "2 empty levels");
		compare(repeatedMeasuresCells.count, 4, "2 empty levels + 2 empty cells");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(0,0)), "", "First row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(1,0)), "Level 1", "First row is Level 1");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(2,0)), "", "Second row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(3,0)), "Level 2", "Second row is Level 2");

		spyFactorLevelListChanged.clear()
		repeatedMeasuresFactors.model.itemChanged(1, "TEST 1")
		spyFactorLevelListChanged.wait(400)
		compare(repeatedMeasuresFactors.model.data(repeatedMeasuresFactors.model.index(1,0)), "TEST 1", "First Level is now TEST 1")
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(1,0)), "TEST 1", "First row is TEST 1");

		spyFactorLevelListChanged.clear()
		repeatedMeasuresFactors.model.itemChanged(3, "TEST 3")
		spyFactorLevelListChanged.wait(400)
		compare(repeatedMeasuresFactors.model.data(repeatedMeasuresFactors.model.index(3,0)), "TEST 3", "New Level is TEST 3")
		compare(repeatedMeasuresFactors.model.data(repeatedMeasuresFactors.model.index(4,0)), "New Level", "New level is added")
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(5,0)), "TEST 3", "Third row is TEST 3");

		spyFactorLevelListChanged.clear()
		repeatedMeasuresFactors.model.itemChanged(5, "Extra Factor")
		spyFactorLevelListChanged.wait(400)
		compare(repeatedMeasuresFactors.factors.length, 2, "2 factors");
		compare(repeatedMeasuresFactors.factors[0], "RM Factor 1", "First factor name is \"RM Factor 1\"");
		compare(repeatedMeasuresFactors.factors[1], "Extra Factor", "Second factor name is \"Extra Factor\"");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(0,0)), "", "1st row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(1,0)), "TEST 1,Level 1", "1st row is TEST 1,Level 1");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(2,0)), "", "2nd row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(3,0)), "TEST 1,Level 2", "2nd row is TEST 1,Level 2");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(4,0)), "", "3rd row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(5,0)), "Level 2,Level 1", "3rd row is Level 2,Level 1");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(6,0)), "", "4th row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(7,0)), "Level 2,Level 2", "4th row is Level 2,Level 2");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(8,0)), "", "5th row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(9,0)), "TEST 3,Level 1", "5th row is TEST 3,Level 1");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(10,0)), "", "6th row has Empty value");
		compare(repeatedMeasuresCells.model.data(repeatedMeasuresCells.model.index(11,0)), "TEST 3,Level 2", "6th row is TEST 3,Level 2");
	}
}
