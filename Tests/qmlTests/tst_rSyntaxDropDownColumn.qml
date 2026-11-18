import QtTest
import QtQuick
import JASP.Controls

// A DropDown of columns is a list control with variables, but its model holds the columns it offers, not the one it
// selected: the R syntax gives the selected column by its name (paretoAddCountVariable = "Total"), never the list of
// all the columns it offers. Fixture (see Tests/testqml.cpp): TestInts, TestLetters, TestDoubles, TestNominal.
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		DropDown	{ id: countVariable; name: "countVariable"; label: "Count variable"; addEmptyValue: true }
	}

	TestCase
	{
		id:		testCase
		name:	"TestRSyntaxDropDownColumn"

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

		function test_selected_column_by_name()
		{
			try
			{
				jaspForm.setOptions({ countVariable: { value: "TestDoubles", types: "scale" } })
				wait(50)
				compare(countVariable.currentValue, "TestDoubles", "the fixture column must be selected")

				var syntax = jaspForm.rSyntaxText
				verify(syntax.indexOf('countVariable = "TestDoubles"') !== -1, "the selected column, by its name, got: " + syntax)
				verify(syntax.indexOf("TestLetters") === -1, "not the other columns the DropDown offers, got: " + syntax)
				verify(syntax.indexOf("list(") === -1, "not a list, got: " + syntax)
			}
			finally
			{
				jaspForm.setOptions({ countVariable: "" })
				wait(20)
			}
		}
	}
}
