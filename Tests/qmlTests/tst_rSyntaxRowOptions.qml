import QtTest
import QtQuick
import JASP.Controls

// In the R syntax, a row of a list control with a row component (a ComponentsList like the PlotBuilderTab of
// jaspPlotBuilder, a variables list with a CheckBox per variable) gives only its key and the options that differ from
// the default of their row control: reading the syntax back, a row control without option gets its default.
// With showAllROptions every option of every row is still given (not tested: the preferences of the test do not keep it).
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1000

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVarsRO";	id: allVarsRO }
			AssignedVariablesList	{ name: "varsRO";		id: varsRO;	rowComponent: CheckBox { name: "logRO" } }
		}

		ComponentsList
		{
			id:					tabsRO
			name:				"tabsRO"
			minimumItems:		1
			addItemManually:	true

			rowComponent: Group
			{
				TextField		{ name: "titleRO";	startValue: "Plot " + (rowIndex + 1) }
				IntegerField	{ name: "sizeRO";	defaultValue: 3 }
				CheckBox		{ name: "showRO" }
				ComponentsList
				{
					name:				"linesRO"
					minimumItems:		1
					addItemManually:	true
					rowComponent:		DoubleField { name: "widthRO"; defaultValue: 0.5 }
				}
			}
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestRSyntaxRowOptions"

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

		function rowsOf(name)
		{
			var option = JSON.parse(jaspForm.createOptionsJsonAsJson(0))[name]
			return Array.isArray(option) ? option : option.value
		}

		function test_componentsList_rows_give_their_non_default_options()
		{
			var initialRows = tabsRO.count
			try
			{
				tabsRO.addIconItem.clicked()
				compare(tabsRO.count, initialRows + 1, "a 2nd row is added")
				var keys = tabsRO.columnsNames
				tabsRO.getRowControl(keys[0], "showRO").click()
				tabsRO.getRowControl(keys[1], "sizeRO").value = 7
				wait(50)

				var rows = rowsOf("tabsRO")
				compare(rows[0].showRO,		true,		"the CheckBox of the 1st row is checked")
				compare(rows[1].sizeRO,		7,			"the IntegerField of the 2nd row is 7")
				compare(rows[1].titleRO,	"Plot 2",	"the default of a row control can depend on its row")

				var syntax = jaspForm.rSyntaxText
				verify(syntax.indexOf("showRO = TRUE") !== -1,		"the checked CheckBox, got: " + syntax)
				verify(syntax.indexOf("sizeRO = 7") !== -1,			"the changed IntegerField, got: " + syntax)
				verify(syntax.indexOf("showRO = FALSE") === -1,		"not the CheckBox left to its default, got: " + syntax)
				verify(syntax.indexOf("sizeRO = 3") === -1,			"not the IntegerField left to its default, got: " + syntax)
				verify(syntax.indexOf("titleRO") === -1,			"not the TextFields left to their default (Plot 1, Plot 2), got: " + syntax)
				verify(syntax.indexOf("widthRO") === -1,			"not the default rows of the ComponentsList inside a row, got: " + syntax)
				verify(syntax.indexOf(keys[0]) !== -1 && syntax.indexOf(keys[1]) !== -1, "each row keeps its key, got: " + syntax)
			}
			finally
			{
				while (tabsRO.count > initialRows)
					tabsRO.removeItem(tabsRO.count - 1)
				var firstShow = tabsRO.getRowControl(tabsRO.columnsNames[0], "showRO")
				if (firstShow.checked)
					firstShow.click()
				wait(20)
			}
		}

		function test_componentsList_rows_without_default_options_read_back_the_same()
		{
			var initialRows = tabsRO.count
			try
			{
				tabsRO.addIconItem.clicked()
				var keys = tabsRO.columnsNames
				tabsRO.getRowControl(keys[1], "sizeRO").value = 9
				wait(50)
				var fullRows = rowsOf("tabsRO")

				// What the R syntax gives: the key of each row and its options that differ from their default
				var shortRows = [ { value: fullRows[0].value }, { value: fullRows[1].value, sizeRO: 9 } ]
				var parsed = JSON.parse(jaspForm.parseOptionsToJson(JSON.stringify({ tabsRO: shortRows })))
				verify(parsed.ok, "the short rows are valid options: " + parsed.error)

				var parsedRows = parsed.options.tabsRO
				parsedRows = Array.isArray(parsedRows) ? parsedRows : parsedRows.value
				compare(JSON.stringify(parsedRows), JSON.stringify(fullRows), "the row controls without option get their default")
			}
			finally
			{
				while (tabsRO.count > initialRows)
					tabsRO.removeItem(tabsRO.count - 1)
				wait(20)
			}
		}

		// No test calls setOptions: it sets the controls up again, which connects the add/move handlers twice (a click
		// would add 2 rows, a move would move 2 variables).
		function test_assignedVariables_rows_give_their_non_default_options()
		{
			try
			{
				allVarsRO.setSelectedItem(0)
				allVarsRO.addSelectedItem(1)
				allVarsRO.moveSelectedItems(varsRO)
				wait(50)
				compare(varsRO.count, 2, "2 variables are assigned")

				var columns = varsRO.columnsNames
				varsRO.getRowControl(columns[1], "logRO").click()
				wait(50)

				var syntax = jaspForm.rSyntaxText
				verify(syntax.indexOf("logRO = TRUE") !== -1,	"the checked CheckBox of the 2nd variable, got: " + syntax)
				verify(syntax.indexOf("logRO = FALSE") === -1,	"not the CheckBox of the 1st variable, left to its default, got: " + syntax)
				verify(syntax.indexOf(columns[0]) !== -1 && syntax.indexOf(columns[1]) !== -1, "both variables, got: " + syntax)
			}
			finally
			{
				for (var i = 0; i < varsRO.count; i++)
				{
					var check = varsRO.getRowControl(varsRO.columnsNames[i], "logRO")
					if (check.checked)
						check.click()
				}
				varsRO.setSelectedItem(0)
				for (var j = 1; j < varsRO.count; j++)
					varsRO.addSelectedItem(j)
				varsRO.moveSelectedItems(allVarsRO)
				wait(50)
			}
		}
	}
}
