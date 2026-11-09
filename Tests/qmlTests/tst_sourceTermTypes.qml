import QtTest
import QtQuick
import JASP
import JASP.Controls

// What a user does in ANOVA: an ordinal variable moved into Fixed Factors (nominal only) becomes nominal there. Components,
// whose source is Fixed Factors, and Model Terms, which take their terms from Components, must show it as nominal too,
// not as its ordinal column type.
//
// This form never calls setOptions: it only uses the moves a user makes, starting from an empty form.
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: sourceTypesForm

	Form
	{
		id:		sourceTypesForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsS;			name: "allVarsS" }
			AssignedVariablesList	{ id: fixedFactorsS;	name: "fixedFactorsS";	allowedColumns: ["nominal"] }
		}

		VariablesForm
		{
			AvailableVariablesList	{ id: componentsS;	name: "componentsS";	source: ["fixedFactorsS"] }
			AssignedVariablesList	{ id: modelTermsS;	name: "modelTermsS";	listViewType: JASP.Interaction }
		}
	}

	TestCase
	{
		name: "TestSourceTermTypes"

		SignalSpy
		{
			id:			spyLoader
			target:		sourceTypesForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function test_variableMovedIntoTheFixedFactorsIsNominalEverywhere()
		{
			allVarsS.setSelectedItemWithName("TestInts")
			allVarsS.moveSelectedItems(fixedFactorsS)
			tryCompare(fixedFactorsS, "count", 1, 1000, "TestInts is moved into the fixed factors")
			tryCompare(componentsS, "count", 1, 1000, "The components come from the fixed factors")

			compare(allVarsS.getVariableRealType("TestInts"), 2, "TestInts is ordinal in the dataset")
			compare(fixedFactorsS.getVariableType("TestInts"), 3, "The fixed factors make TestInts nominal")
			compare(componentsS.getVariableType("TestInts"), 3, "The components keep the nominal type of the fixed factors")

			if (modelTermsS.count === 0) // An interaction list may already have added the new factor by itself
				componentsS.itemDoubleClicked(0)
			tryCompare(modelTermsS, "count", 1, 1000, "TestInts is in the model terms")

			wait(100) // let queued termsChanged/setBoundValue writes settle
			const options = JSON.parse(sourceTypesForm.createOptionsJsonAsJson(0))
			compare(JSON.stringify(options.fixedFactorsS.types), JSON.stringify(["nominal"]), "The fixed factors give TestInts as nominal")
			compare(JSON.stringify(options.modelTermsS.types), JSON.stringify(["nominal"]), "The model terms keep the nominal type as well")
		}
	}
}
