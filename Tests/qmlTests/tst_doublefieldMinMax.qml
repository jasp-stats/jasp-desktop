import QtQuick
import QtTest
import JASP.Controls
import JASP

TestCase
{
	name:		"TestDoublefieldMinMax"

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

		DoubleField
		{
			id: defaultDoubleLower
			label: "Default Double Lower Bound"
			name: "lowerIntegerDefault"
			value: 0.1
		}

		DoubleField
		{
			id:		doubleLower
			name:	"doubleLower"
			label:  "Double Lower Bound"
			max:	doubleUpper.value
			defaultValue: defaultDoubleLower.value
			min:	0
		}

		DoubleField
		{
			id:		doubleUpper
			name:	"doubleUpper"
			label:  "Double Upper Bound (Exclusive)"
			min:	doubleLower.value
			defaultValue: 0.50
			max:	1
			inclusive: JASP.MinOnly
		}
	}

	function test_1setDefaultValue()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1, "The form should have completed")
		defaultDoubleLower.value = .2
		compare(doubleLower.boundJson(), .2, "The default value and the current value of doubleLower are both .1, so when changing the default value to .2, the current value also change to .2");
	}

	function test_2setDefaultValue()
	{
		doubleLower.value = .3
		compare(doubleLower.boundJson(), .3, "The default value and the current value of doubleLower are resp. .2 and .3");
		defaultDoubleLower.value = .4
		compare(doubleLower.boundJson(), .3, "As the default value and the current value were different, changing the default value does not change the current value");
	}

	function test_3minValueError()
	{
		doubleLower.value = .1
		compare(doubleLower.hasError, false);
		doubleLower.forceActiveFocus()
		doubleLower.value = -0.1
		doubleUpper.forceActiveFocus();			// The error will be triggered when the control loses the focus
		compare(doubleLower.hasError, true, "The value cannot be negative")
		compare(doubleLower.boundJson(), .1, "The value is set back to the previous valid value")
	}

	function test_4maxValueError()
	{
		doubleLower.forceActiveFocus()
		doubleLower.value = .5
		doubleUpper.forceActiveFocus();
		compare(doubleLower.hasError, false, "The max value is .5 and is inclusive, so .5 is valid");
		doubleLower.forceActiveFocus()
		doubleLower.value = .51
		doubleUpper.forceActiveFocus();
		compare(doubleLower.hasError, true, "The value is .51 is too high");
		compare(doubleLower.boundJson(), .5, "The value is set back to the previous valid value");
	}

	function test_5exclusiveMaxValueError()
	{
		doubleUpper.forceActiveFocus();
		doubleUpper.value = .99
		doubleLower.forceActiveFocus();
		compare(doubleUpper.boundJson(), .99, "The max value is 1 and is exclusive, so .99 is valid");
		compare(doubleUpper.hasError, false);
		doubleUpper.forceActiveFocus();
		doubleUpper.value = 1
		doubleLower.forceActiveFocus();
		compare(doubleUpper.hasError, true, "The max value is 1 and is exclusive, so 1 is not valid");
		compare(doubleUpper.boundJson(), .99, "The value is set back to the previous valid value");
	}

}
