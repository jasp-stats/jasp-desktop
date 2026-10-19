import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestCheckbox"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		CheckBox
		{
			id:						control
		}
	}

	function test_checked()
	{
		control.checked = true
		compare(control.boundJson(), true);
	}

	function test_otherValue()
	{
		control.checked = false
		compare(control.boundJson(), false);
	}
}
