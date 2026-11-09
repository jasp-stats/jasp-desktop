import QtTest
import QtQuick
import JASP
import JASP.Controls

// The type a user chose for a variable (e.g. scale for an ordinal column) is saved in its option ({value, types}), and must
// still be the type when the options are loaded again (a .jasp file, R syntax, jaspTools). A term made of variables of
// another list keeps the type they have there, whatever the options say (see tst_sourceTermTypes.qml).
//
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: loadedTypesForm

	Form
	{
		id:		loadedTypesForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsL;			name: "allVarsL" }
			AssignedVariablesList	{ id: variablesL;		name: "variablesL" }
			AssignedVariablesList	{ id: fixedFactorsL;	name: "fixedFactorsL";	allowedColumns: ["nominal"] }
		}

		VariablesForm
		{
			AvailableVariablesList	{ id: componentsL;	name: "componentsL";	source: ["fixedFactorsL"] }
			AssignedVariablesList	{ id: modelTermsL;	name: "modelTermsL";	listViewType: JASP.Interaction }
		}
	}

	TestCase
	{
		name: "TestLoadedVariableTypes"

		SignalSpy
		{
			id:			spyLoader
			target:		loadedTypesForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function loadOptions(variables)
		{
			loadedTypesForm.setOptions({
				variablesL:		variables,
				fixedFactorsL:	{ value: ["TestInts"], types: ["nominal"] },
				// a type in the options that differs from the one of the fixed factors
				modelTermsL:	{ optionKey: "components", value: [{ components: ["TestInts"] }], types: ["ordinal"] }
			})
			wait(100) // let queued termsChanged/setBoundValue writes settle
			return JSON.parse(loadedTypesForm.createOptionsJsonAsJson(0))
		}

		function test_aLoadedTypeIsKept()
		{
			const options = loadOptions({ value: ["TestInts", "TestDoubles"], types: ["scale", "ordinal"] })

			compare(allVarsL.getVariableRealType("TestInts"), 2, "TestInts is ordinal in the dataset")
			compare(variablesL.getVariableType("TestInts"), 1, "TestInts keeps the scale type of the options")
			compare(variablesL.getVariableType("TestDoubles"), 2, "TestDoubles keeps the ordinal type of the options")
			compare(JSON.stringify(options.variablesL.types), JSON.stringify(["scale", "ordinal"]), "The options give the loaded types back")
		}

		function test_aVariableWithoutTypeGetsItsColumnType()
		{
			const options = loadOptions({ value: ["TestInts"], types: [] })

			compare(variablesL.getVariableType("TestInts"), 2, "Without a type in the options, TestInts is ordinal as in the dataset")
			compare(JSON.stringify(options.variablesL.types), JSON.stringify(["ordinal"]))
		}

		function test_termsKeepTheTypeOfTheirSourceList()
		{
			const options = loadOptions({ value: [], types: [] })

			compare(fixedFactorsL.getVariableType("TestInts"), 3, "The fixed factors make TestInts nominal")
			compare(JSON.stringify(options.fixedFactorsL.types), JSON.stringify(["nominal"]))
			compare(JSON.stringify(options.modelTermsL.types), JSON.stringify(["nominal"]), "The model terms take the nominal type of the fixed factors")
		}
	}
}
