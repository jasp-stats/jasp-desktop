import QtQuick
import QtTest
import JASP.Controls

// The form is not placed in the TestCase: a TestCase is invisible, and an invisible control cannot get the keyboard focus.
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		DoubleField
		{
			id:			twoDecimals
			name:		"twoDecimals"
			label:		"Two decimals"
			decimals:	2
			max:		1000
		}

		DoubleField
		{
			id:			noDecimals
			name:		"noDecimals"
			label:		"No decimals"
			decimals:	0
			max:		1000
		}

		IntegerField
		{
			id:			integerField
			name:		"integerField"
			label:		"Integer"
			max:		1000
		}
	}

	TestCase
	{
		name:		"TestDoublefieldDecimals"
		when:		windowShown

		SignalSpy
		{
			id:				spyLoader
			target:			jaspForm
			signalName:		"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		// Simulates a user typing text in a field: this is the only way to check the validator, as setting the value directly does not filter the input.
		function typeIn(field, text)
		{
			field.control.forceActiveFocus()
			field.control.selectAll()

			for (var i = 0; i < text.length; i++)
				keyClick(text[i])
		}

		function test_0constraintsAreNotRelaxedByDefault()
		{
			compare(jaspForm.relaxInputConstraints, false, "The input constraints are only relaxed when the options are set programmatically (via the RPC interface)")
		}

		function test_1typeAllowedDecimals()
		{
			typeIn(twoDecimals, "1.23")
			compare(twoDecimals.displayValue, "1.23", "2 decimals are allowed")
			compare(twoDecimals.control.acceptableInput, true)
		}

		function test_2typeTooManyDecimals()
		{
			typeIn(twoDecimals, "1.234")
			compare(twoDecimals.displayValue, "1.23", "The 3rd decimal is refused when decimals is 2")
		}

		function test_3typeDecimalPointWhenNoDecimalsAllowed()
		{
			typeIn(noDecimals, "12.5")
			compare(noDecimals.displayValue, "125", "The decimal point is refused when decimals is 0")

			typeIn(integerField, "12.5")
			compare(integerField.displayValue, "125", "An IntegerField does not allow decimals")
		}

		// A value that is not typed in by the user (set by the R syntax, a JASP file or a QML binding) is not filtered, but must give an error.
		function test_4setValueWithTooManyDecimals()
		{
			typeIn(twoDecimals, "1.23")
			typeIn(noDecimals, "1")			// Losing the focus makes 1.23 the last valid value of twoDecimals
			compare(twoDecimals.hasError, false)

			twoDecimals.control.forceActiveFocus()
			twoDecimals.value = 1.234
			compare(twoDecimals.control.acceptableInput, false, "1.234 has too many decimals")

			noDecimals.control.forceActiveFocus()	// The error is only shown when the control loses the focus
			compare(twoDecimals.hasError, true, "The value has too many decimals")
			compare(twoDecimals.boundJson(), 1.23, "The value is set back to the previous valid value")
		}

		function test_5relaxedConstraintsAllowMoreDecimals()
		{
			jaspForm.relaxInputConstraints = true

			typeIn(twoDecimals, "1.234")
			compare(twoDecimals.displayValue, "1.234", "When the constraints are relaxed, more decimals than allowed are accepted")

			jaspForm.relaxInputConstraints = false

			typeIn(twoDecimals, "1.234")
			compare(twoDecimals.displayValue, "1.23", "The decimals are enforced again")
		}
	}
}
