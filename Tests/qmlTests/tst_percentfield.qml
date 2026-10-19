import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestPercentfield"

	property alias form: jaspForm

	Form
	{
		id: jaspForm

		PercentField
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
		compare(control.boundJson(),  0.02);
	}

	function test_anotherValue()
	{
		control.value = 12.34;
		compare(control.boundJson(),  0.1234);
	}

	function test_aValueAbove100()
	{
		control.value = 122.34;
		compare(control.boundJson(),  1);
	}

	function test_aString()
	{
		control.value = "This isnt a percentage at all!";
		compare(control.boundJson(),  0);
	}
}
