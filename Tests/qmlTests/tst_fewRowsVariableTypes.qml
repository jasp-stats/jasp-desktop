import QtTest
import QtQuick
import JASP.Controls

// A data set with fewer rows than columns (the small data sets of module unit tests run through
// jaspTools and the syntax bridge): every column must still report its type. It used to come back
// unknown for the columns at or past the row count (Filter::provideInfo indexed its own data rows),
// so a variable in a list that does not allow its type was not converted to an allowed type: the
// option said "unknown" and R received the column as a factor.
//
// Fixture (Tests/testqml.cpp): dataset "FewRows", 3 rows and 5 columns: FewA (scale), FewB, FewC,
// FewD and FewE (ordinal).
Item
{
	width:	1000
	height:	800

	property alias form: fewRowsForm

	Form
	{
		id:		fewRowsForm
		width:	800 // The theme of the QML test engine has no form width

		// Several assigned lists sharing one available list, as in a VariablesForm of a module
		VariablesForm
		{
			AvailableVariablesList	{ id: fewVarsL;	name: "fewVarsL";	dataSetSelectionOption: "fewRowsSet" }
			AssignedVariablesList	{ id: countL;	name: "countL";		allowedColumns: ["scale"];				singleVariable: true }
			AssignedVariablesList	{ id: sizeL;	name: "sizeL";		allowedColumns: ["scale"];				singleVariable: true }
			AssignedVariablesList	{ id: totalL;	name: "totalL";		allowedColumns: ["scale"];				singleVariable: true }
			AssignedVariablesList	{ id: timeL;	name: "timeL";		allowedColumns: ["scale", "ordinal"];	singleVariable: true }
		}
	}

	TestCase
	{
		name: "TestFewRowsVariableTypes"

		SignalSpy
		{
			id:			spyLoader
			target:		fewRowsForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function fewRowsFilterId()
		{
			for (var entry of fewVarsL.filterSelectionValues)
				if (entry.label.indexOf("FewRows") === 0)
					return entry.value

			fail("The fixture must offer the dataset 'FewRows', got: " + JSON.stringify(fewVarsL.filterSelectionValues))
		}

		function test_aColumnPastTheRowCountHasItsType()
		{
			fewVarsL.selectedFilterId = parseInt(fewRowsFilterId())
			wait(50)

			compare(fewVarsL.getVariableRealType("FewA"), 1, "FewA is scale")
			for (const name of ["FewB", "FewC", "FewD", "FewE"])
				compare(fewVarsL.getVariableRealType(name), 2, name + " is ordinal")
		}

		function test_variablesGetAnAllowedType()
		{
			// The options as R syntax / jaspTools give them: just the column names, no types
			fewRowsForm.setOptions({
				fewRowsSet:	fewRowsFilterId(),
				countL:		"FewA",
				totalL:		"FewB",	// before the row count
				sizeL:		"FewD",	// past the row count
				timeL:		"FewE"	// past the row count, its ordinal type is allowed
			})
			wait(100) // let queued termsChanged/setBoundValue writes settle

			const options = JSON.parse(fewRowsForm.createOptionsJsonAsJson(0))

			compare(JSON.stringify(options.countL.types),	JSON.stringify(["scale"]),		"A scale column stays scale")
			compare(JSON.stringify(options.totalL.types),	JSON.stringify(["scale"]),		"An ordinal column is made scale in a scale-only list")
			compare(JSON.stringify(options.sizeL.types),	JSON.stringify(["scale"]),		"An ordinal column past the row count is made scale in a scale-only list")
			compare(JSON.stringify(options.timeL.types),	JSON.stringify(["ordinal"]),	"An ordinal column keeps its type where ordinal is allowed")
		}
	}
}
