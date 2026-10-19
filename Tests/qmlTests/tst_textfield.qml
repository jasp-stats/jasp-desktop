import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestTextfield"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		TextField
		{
			id:						textField
		}
	}

	function test_displayValue()
	{
		textField.value =					"Hallo wereld!"
		compare(textField.boundJson(),	"Hallo wereld!");
	}

	function test_otherValue()
	{
		textField.value =				"Iets anders!";
		compare(textField.boundJson(),  "Iets anders!");
	}
}
