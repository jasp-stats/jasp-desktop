import QtTest
import QtQuick
import JASP.Controls

// A TableView whose size is a QML binding on other controls must keep the values it is loaded with (a reopened
// JASP file, R syntax through the syntax bridge, jaspTools), also when these controls change for a moment while
// the options are bound. This is the shape of jaspQualityControl's doeFactorial.qml: the table is bound before
// factorialType (alphabetical order, no dependency between them), factorialType's onValueChanged then sets the
// number of levels to 3, so the table shrinks to 4 columns, and the number of levels gets its loaded value back
// (JASPControl::_resetBindingValue), so the table grows again: the columns beyond the 4th used to come back with
// their default values, i.e. every level beyond the 3rd was lost.
Item
{
	width:	1000
	height:	1000

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		IntegerField
		{
			id:					numberOfCategorical
			name:				"numberOfCategorical"
			min:				2
			defaultValue:		3
			max:				256
			property int intValue: defaultValue
			onValueChanged:		intValue = value !== "" ? value : 0
		}

		IntegerField
		{
			id:					numberOfLevels
			name:				"categoricalNoLevels"
			min:				2
			defaultValue:		2
			max:				20
		}

		RadioButtonGroup
		{
			name:				"factorialType"
			onValueChanged:		numberOfLevels.value = value !== "generalFullFactorial" ?  2 : 3

			RadioButton { name: "factorialTypeDefault";	checked: true	}
			RadioButton { name: "generalFullFactorial"					}
		}

		TableView
		{
			id:					categoricalVariables
			name:				"categoricalVariables"
			modelType:			JASP.Simple
			itemType:			JASP.String

			initialRowCount:	numberOfCategorical.intValue
			initialColumnCount: 1 + parseInt(numberOfLevels.value)
			rowCount:			numberOfCategorical.intValue
			columnCount:		1 + parseInt(numberOfLevels.value)

			// Factor names "A", "B", ... in the first column, level names "a", "b", ... in the others
			function getDefaultValue(columnIndex, rowIndex) { return columnIndex === 0 ? String.fromCharCode(65 + rowIndex) : String.fromCharCode(97 + columnIndex - 1) }
		}
	}

	TestCase
	{
		name: "TestTableViewKeepsLoadedColumns"

		SignalSpy
		{
			id:			spyLoader
			target:		jaspForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		// The value of the table as R (and jaspTools) give it: one list per column
		function tableOption(columns)
		{
			var levels = ["Row 0", "Row 1", "Row 2"]
			var result = []
			for (var col = 0; col < columns.length; col++)
				result.push({ levels: levels, name: "data " + (col + 1), values: columns[col] })
			return result
		}

		function loadDesign(nbLevels, columns)
		{
			// Start from the default design, so that factorialType changes while the options of the test are bound
			jaspForm.setOptions({ numberOfCategorical: 3, categoricalNoLevels: 2, factorialType: "factorialTypeDefault" })
			wait(50)
			jaspForm.setOptions({ numberOfCategorical: 3, categoricalNoLevels: nbLevels, factorialType: "generalFullFactorial", categoricalVariables: tableOption(columns) })
			wait(50) // let queued writes settle
			return JSON.parse(jaspForm.createOptionsJsonAsJson(0))
		}

		function compareTable(options, columns, what)
		{
			var table = options.categoricalVariables
			compare(table.length, columns.length, what + ": number of columns")
			for (var col = 0; col < columns.length; col++)
			{
				compare(table[col].name, "data " + (col + 1), what + ": name of column " + (col + 1))
				compare(table[col].values, columns[col], what + ": values of column " + (col + 1))
			}
		}

		function test_fourLevelsKeepTheFifthColumn()
		{
			var columns = [["F1", "F2", "F3"], ["x", "y", "z"], ["p", "q", "r"], ["k", "l", "m"], ["n", "", ""]]
			var options = loadDesign(4, columns)
			compare(options.categoricalNoLevels, 4, "the number of levels keeps its loaded value")
			compareTable(options, columns, "4 levels")
			compare(categoricalVariables.columnCount, 5, "and the table shows them")
		}

		function test_sixLevelsKeepTheFifthAndSixthColumns()
		{
			var columns = [["F1", "F2", "F3"], ["x", "y", "z"], ["p", "q", "r"], ["k", "l", "m"], ["n", "o", ""], ["s", "", ""], ["t", "", ""]]
			var options = loadDesign(6, columns)
			compare(options.categoricalNoLevels, 6, "the number of levels keeps its loaded value")
			compareTable(options, columns, "6 levels")
		}

		function test_userResizeAfterLoadUsesDefaults()
		{
			// Once the options are bound, a column the user adds (here by changing the number of levels) gets the
			// default values of that column, not a value the table was loaded with before.
			var columns = [["F1", "F2", "F3"], ["x", "y", "z"], ["p", "q", "r"], ["k", "l", "m"], ["n", "", ""]]
			loadDesign(4, columns)

			try
			{
				numberOfLevels.value = 2
				wait(50)
				compare(categoricalVariables.columnCount, 3, "2 levels: 3 columns")
				numberOfLevels.value = 4
				wait(50)

				var table = JSON.parse(jaspForm.createOptionsJsonAsJson(0)).categoricalVariables
				compare(table.length, 5, "4 levels again: 5 columns")
				compare(table[1].values, columns[1], "the kept columns keep their values")
				// The default of column i is the letter i (getDefaultValue above): the 4th column (index 3) is "c", the 5th "d"
				compare(table[3].values, ["c", "c", "c"], "the 4th column is new: default values of that column")
				compare(table[4].values, ["d", "d", "d"], "the 5th column is new: default values of that column")
			}
			finally
			{
				jaspForm.setOptions({ numberOfCategorical: 3, categoricalNoLevels: 2, factorialType: "factorialTypeDefault" })
				wait(50)
			}
		}
	}
}
