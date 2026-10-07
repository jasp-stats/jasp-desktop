import QtTest
import QtQuick
import JASP.Controls

// The dataset/filter selection of VariablesForm (dataSetSelection): it is enabled only for
// multiDataSetAware analyses, fed by the filters of all datasets in the workspace, and selecting a
// dataset is selecting one of its filters (which the analysis then runs on).
// The form is kept out of the TestCase (invisible) like the other VariablesForm tests do.
Item
{
	width:	1000
	height:	800

	property alias form: jaspForm

	Form
	{
		id:		jaspForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			id:					selectionForm
			dataSetSelection:	true

			AvailableVariablesList	{ name: "allVars";	id: allVars }
			AssignedVariablesList	{ name: "target";		id: targetList }
		}
	}

	TestCase
	{
		id:		testCase
		name:	"TestDataSelectingVariablesForm"

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

		function test_inert_when_not_aware()
		{
			var analysis = jaspForm.analysis
			verify(analysis, "The form should have an (dummy) analysis")
			analysis.multiDataSetAware = false

			compare(selectionForm.dataSetSelection, true)
			compare(selectionForm.dataSetSelectionAllowed, false, "dataSetSelection must stay inert for non-aware analyses")
		}

		function test_aware_enables_selection_and_values()
		{
			var analysis = jaspForm.analysis
			analysis.multiDataSetAware = true
			compare(selectionForm.dataSetSelectionAllowed, true)

			var entries = selectionForm.dataSetSelectionValues
			compare(entries.length, 3, "fixture: first datasets default filter, second datasets default filter and one extra filter")

			var filterIds = []

			for (var i = 0; i < entries.length; i++)
			{
				var filterId = parseInt(entries[i].value)

				verify(!isNaN(filterId) && filterId >= 0, "the value of an entry must be a filterId, got: " + entries[i].value)
				verify(entries[i].label.indexOf(" - ") > 0, "the label should read 'DataSet - Filter', got: " + entries[i].label)
				verify(filterIds.indexOf(filterId) === -1, "filter ids are unique, so no entry may be duplicated")
				filterIds.push(filterId)
			}
		}

		function test_selection_switches_analysis_filter()
		{
			var analysis = jaspForm.analysis
			analysis.multiDataSetAware = true
			compare(selectionForm.dataSetSelectionAllowed, true)
			compare(selectionForm.selectedFilterId, analysis.filterId)

			var current = selectionForm.selectedFilterId
			var other = -1

			for (var entry of selectionForm.dataSetSelectionValues)
			{
				var filterId = parseInt(entry.value)
				if (filterId !== current)
				{
					other = filterId
					break
				}
			}

			verify(other !== -1, "the fixture should offer another filter to select")

			selectionForm.selectedFilterId = other

			compare(selectionForm.selectedFilterId, other, "the form should report what is selected")
			compare(analysis.filterId, other, "selecting a dataset is selecting its filter: it is handed to the analysis")

			// The lists re-provision on the filter change and must stay functional afterwards:
			wait(50)
			compare(targetList.dropKeys[0], allVars.name, "the assigned list still knows its source")
		}

		// Render probe: the selection must occupy real height when allowed (a zero-height area would
		// make the dropdown invisible in the GUI while all the property plumbing still checks out).
		function test_selector_area_gets_real_height_when_aware()
		{
			var analysis = jaspForm.analysis
			selectionForm.height = 300

			analysis.multiDataSetAware = false
			wait(10)
			compare(selectionForm._selectorHeight, 0, "without awareness the lists start at the top")

			analysis.multiDataSetAware = true
			wait(10)
			verify(selectionForm._selectorHeight > 0,
				   "the dataset selection should occupy real height when aware (got " + selectionForm._selectorHeight + ")")
		}
	}
}
