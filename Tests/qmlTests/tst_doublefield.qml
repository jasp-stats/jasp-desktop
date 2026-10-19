import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestDoublefield"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		DoubleField
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
		compare(control.boundJson(),  12.34);
	}

	function test_aString()
	{
		control.value = "This isnt a double at all!";
		compare(control.boundJson(),  0);
	}
}
