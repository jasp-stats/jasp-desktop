import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestIntegerfield"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		IntegerField
		{
			id:						control
		}
	}

	function test_displayValue()
	{
		control.value = 0
		compare(control.boundJson(), 0);
	}

	function test_otherValue()
	{
		control.value = 2;
		compare(control.boundJson(),  2);
	}

	function test_anotherValue()
	{
		control.value = 12.34;
		compare(control.boundJson(),  12);
	}

	function test_aString()
	{
		control.value = "This isnt an integer at all!";
		compare(control.boundJson(),  0);
	}
}
