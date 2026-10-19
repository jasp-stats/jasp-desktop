import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls
import JASP

TestCase
{
	name:		"TestVariablesListColumnTypeIcons"

	property alias form: jaspForm

	property var favVarsOriginalType0: null

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			id:			varsForm
			AvailableVariablesList{  name: "allVars";		id: allVars }
			AssignedVariablesList {  name: "assignedVars";	id: assignedVars }
		}
	}

	function test_availableVarsShowTheirRealTypeIcon()
	{
		spyLoader.wait(3000)
		compare(spyLoader.count, 1, "The form should have completed")

		compare(dataSetInfo.variableCount, 4, "The quicktest dataset should have 4 columns")

		// In the available variables list a column that is not manually changed must show
		// its real dataset type (default icon). The transformed ("manually changed type")
		// icon is only shown when getVariableType() differs from getVariableRealType()
		// (see ListModel::data, ColumnTypeIconRole).
		var names = allVars.columnsNames
		compare(names.length, dataSetInfo.variableCount)

		for (var i = 0; i < names.length; i++)
		{
			var name	= names[i]
			var type	= allVars.getVariableType(name)
			var real	= allVars.getVariableRealType(name)

			compare(type, real,
					"Available variable '" + name + "' was NOT manually changed, yet its shown type (" + type +
					") differs from its dataset type (" + real + "), so it would show the 'manually changed type' icon")
		}

		// Positive control: explicitly change a variable's type; then that one variable
		// should show the transformed icon, while the others keep their real-type icon.
		// The first column (TestDoubles) is scale (1); setting it to ordinal (2) differs
		// from its real type, so it should become "transformed". See Common/columntype.h.
		favVarsOriginalType0 = allVars.getVariableRealType(names[0])
		allVars.setVariableType(0, 2)
		wait(200)

		var changedName = names[0]
		for (var j = 0; j < names.length; j++)
		{
			var vName	= names[j]
			var vType	= allVars.getVariableType(vName)
			var vReal	= allVars.getVariableRealType(vName)

			if (vName === changedName)
				compare(vType !== vReal, true,
						"The variable whose type was changed ('" + vName + "') should show the transformed icon")
			else
compare(vType, vReal,
					"Variable '" + vName + "' was not changed, so it should show its real-type icon")
		}
	}

	function cleanup()
	{
		// Restore the shared quicktest dataset: the positive control changed column 0's type, which
		// is the same DataSetProvider dataset used by every tst_*.qml file in this JASPQuickTest run.
		// Leaving it changed leaks the ordinal type into later tests.
		if (favVarsOriginalType0 !== null)
			allVars.setVariableType(0, favVarsOriginalType0)
	}
}
