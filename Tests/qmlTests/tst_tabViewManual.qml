import QtTest
import QtQuick
import QtQuick.Controls
import JASP.Controls


TestCase
{
	name:		"TestRowControls"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}


	Form
	{
		id:		jaspForm

		ComponentsList
		{
			id:					componentsList
			addItemManually:	true
			minimumItems:		1

			rowComponent: RowLayout
			{
				TextField
				{
					name: 				"Hypothesis"
					startValue:			qsTr("H") + (rowIndex + 1)
				}
				IntegerField
				{
					name:				"Integer"
					startValue:			1
				}
			}
		}
	}

	function test_tabViewManual()
	{
		spyLoader.wait(1000)
		compare(componentsList.count,											1,			"minimumItems is 1, so 1 row is automatically created");
		var keys = componentsList.columnsNames
		compare(keys.length,													1);
		var textField = componentsList.getRowControl(keys[0], "Hypothesis")
		var integerField = componentsList.getRowControl(keys[0], "Integer")
		compare(textField.value,												"H1",		"startValue of the TextField is H + (rowIndex + 1), so the default value for the first row must be H1")
		compare(integerField.value,												1)

		componentsList.addIconItem.clicked()
		compare(componentsList.count,											2,			"The addIcon has been clicked, so a 2nd row has been created");
		keys = componentsList.columnsNames
		compare(keys.length,													2);
		textField = componentsList.getRowControl(keys[1], "Hypothesis")
		integerField = componentsList.getRowControl(keys[1], "Integer")
		compare(textField.value,												"H2",		"default value of the TextField of the 2nd row should be H2")
		compare(integerField.value,												1)

		textField.value = "ChangedBeforeAdding"
		componentsList.addIconItem.clicked()
		compare(componentsList.count,											3,			"The addIcon has been clicked, so a 3rd row has been created");
		keys = componentsList.columnsNames
		compare(keys.length,													3);
		textField = componentsList.getRowControl(keys[1], "Hypothesis")
		compare(textField.value,												"ChangedBeforeAdding",	"The TextField was changed to ChangedBeforeAdding just before clicking addItem, this value should be saved in the TextField")

		textField.value = "ChangedBeforeRemoving"
		componentsList.removeItem(2)
		compare(componentsList.count,											2,			"The 3rd row has been removed, so only 2 rows left");
		keys = componentsList.columnsNames
		compare(keys.length,													2);
		textField = componentsList.getRowControl(keys[1], "Hypothesis")
		compare(textField.value,												"ChangedBeforeRemoving",	"The TextField was changed to ChangedBeforeRemoving just before removuing the 3rd row, this value should be saved in the TextField")

	}
}
