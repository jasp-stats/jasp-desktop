import QtQuick
import QtTest
import JASP.Controls

TestCase
{
	name:		"TestFormulafield"

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

		Text
		{
			id: dummyText
			text: qsTr("Dummy")
		}

		FormulaField
		{
			id:						formulaField
			defaultValue:			"1"
			min:					1
			name:					"formulaField"
		}
	}

	function test_expressionValue()
	{
		spyLoader.wait(1000)
		compare(spyLoader.count, 1);
		compare(formulaField.displayValue,	"1");
		compare(formulaField.value,			1);
		formulaField.value =				"1+3"
		compare(formulaField.displayValue,	"1+3");
		compare(formulaField.value,			"1+3");
		//compare(formulaField.realValue,		"4"); // Cannot check this, as long as the unit test does not have an engine.

		formulaField.value =				.1
		dummyText.forceActiveFocus();		// The error will be triggered when the control loses the focus
		compare(formulaField.hasError, true, "The value must be bigger than 1")


		var options = jaspForm.options()
		options["formulaField"] = "1+2"
		jaspForm.setOptions(options)					// This mimic the loading of a JASP file having these options
		compare(formulaField.displayValue,	"1+2");
		compare(formulaField.value,			"1+2");
	}

}
