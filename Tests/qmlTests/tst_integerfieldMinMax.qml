import QtQuick
import QtTest
import JASP.Controls
import JASP

TestCase
{
	name:		"TestIntegerfieldMinMax"

	property alias form: jaspForm

	SignalSpy
	{
		id:				spyLoader
		target:			jaspForm
		signalName:		"formCompletedSignal"
	}

	Form
	{
		id: jaspForm

		IntegerField
		{
			id: defaultIntegerLower
			label: "Default Integer Lower Bound"
			name: "lowerDefault"
			value: 1
		}

		IntegerField
		{
			id:		integerLower
			name:	"integerLower"
			label:  "Integer Lower Bound"
			max:	integerUpper.value
			defaultValue: defaultIntegerLower.value
			min:	0
		}

		IntegerField
		{
			id:		integerUpper
			name:	"integerUpper"
			label:  "Integer Upper Bound (Exclusive)"
			min:	integerLower.value
			defaultValue: 5
			max:	10
			inclusive: JASP.MinOnly
		}
	}

	function test_1setDefaultValue()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1, "The form should have completed")
		defaultIntegerLower.value = 2
		compare(integerLower.boundJson(), 2, "The default value and the current value of integerLower are both 1, so when changing the default value to 2, the current value also change to 2")
	}

	function test_2displayValue()
	{
		integerLower.value = 3
		compare(integerLower.boundJson(), 3, "The default value and the current value of integerLower are resp. 2 and 3")
		defaultIntegerLower.value = 4
		compare(integerLower.boundJson(), 3, "As the default value and the current value were different, changing the default value does not change the current value")
	}

	function test_3minValueError()
	{
		integerLower.value = 1
		compare(integerLower.hasError, false);
		integerLower.forceActiveFocus()
		integerLower.value = -1
		integerUpper.forceActiveFocus();		// The error will be triggered when the control loses the focus
		compare(integerLower.hasError, true, "The value cannot be negative")
		compare(integerLower.boundJson(), 1, "The value is set back to the previous valid value")
	}

	function test_4maxValueError()
	{
		integerLower.forceActiveFocus()
		integerLower.value = 5
		integerUpper.forceActiveFocus();
		compare(integerLower.hasError, false, "The max value is 5 and is inclusive, so 5 is valid");
		integerLower.forceActiveFocus()
		integerLower.value = 6
		integerUpper.forceActiveFocus();
		compare(integerLower.hasError, true, "The value is 6 is too high")
		compare(integerLower.boundJson(), 5, "The value is set back to the previous valid value")
	}

	function test_5exclusiveMaxValueError()
	{
		integerUpper.forceActiveFocus();
		integerUpper.value = 9
		integerLower.forceActiveFocus();
		compare(integerUpper.boundJson(), 9, "The max value is 10 and is exclusive, so 9 is valid");
		compare(integerUpper.hasError, false);
		integerUpper.forceActiveFocus();
		integerUpper.value = 10
		integerLower.forceActiveFocus();
		compare(integerUpper.hasError, true, "The max value is 10 and is exclusive, so 10 is not valie");
		compare(integerUpper.boundJson(), 9, "The value is set back to the previous valid value");
	}

}
