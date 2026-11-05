import QtTest
import QtQuick
import JASP
import JASP.Controls

// Binding the options of an already initialized form (the syntax bridge parses every R syntax call and every jaspTools
// test with a cached form, R syntax in JASP rebinds as well) must give an interaction list exactly the terms of its
// value. Binding the factors first gives the model terms their default interactions; the model terms of the value
// must replace these, not keep them (ANOVA "Simple Main Effects", RM ANOVA without interaction, MANOVA, ...).
//
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: rebindForm

	Form
	{
		id:		rebindForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ id: allVarsR;		name: "allVarsR" }
			AssignedVariablesList	{ id: factorsR;		name: "factorsR" }
		}

		VariablesForm
		{
			AvailableVariablesList	{ id: componentsR;	name: "componentsR";	source: ["factorsR"] }
			AssignedVariablesList	{ id: modelTermsR;	name: "modelTermsR";	listViewType: JASP.Interaction }
		}
	}

	TestCase
	{
		name: "TestInteractionRebind"

		SignalSpy
		{
			id:			spyLoader
			target:		rebindForm
			signalName:	"formCompletedSignal"
		}

		function initTestCase()
		{
			spyLoader.wait(3000)
			compare(spyLoader.count, 1, "The form should have completed")
		}

		function modelTerms()
		{
			wait(100) // let queued termsChanged/setBoundValue writes settle
			const gen = JSON.parse(rebindForm.createOptionsJsonAsJson(0))
			return JSON.stringify(gen.modelTermsR.value)
		}

		function test_givenModelTermsWithoutInteraction()
		{
			rebindForm.setOptions({ factorsR: ["TestLetters", "TestNominal"], modelTermsR: [ { components: "TestLetters" }, { components: "TestNominal" } ] })

			compare(modelTerms(), JSON.stringify([ { components: ["TestLetters"] }, { components: ["TestNominal"] } ]), "The default interaction of the factors is not added to the given model terms")
		}

		function test_givenModelTermsDropAnOldInteraction()
		{
			rebindForm.setOptions({ factorsR: ["TestLetters", "TestNominal"], modelTermsR: [ { components: "TestLetters" }, { components: "TestNominal" }, { components: ["TestLetters", "TestNominal"] } ] })
			compare(modelTerms(), JSON.stringify([ { components: ["TestLetters"] }, { components: ["TestNominal"] }, { components: ["TestLetters", "TestNominal"] } ]), "A given interaction is kept")

			rebindForm.setOptions({ factorsR: ["TestLetters", "TestNominal"], modelTermsR: [ { components: "TestLetters" }, { components: "TestNominal" } ] })
			compare(modelTerms(), JSON.stringify([ { components: ["TestLetters"] }, { components: ["TestNominal"] } ]), "The interaction of the previous options is not kept")
		}

		function test_missingModelTermsGetTheDefaultInteraction()
		{
			rebindForm.setOptions({ factorsR: ["TestLetters"], modelTermsR: [ { components: "TestLetters" } ] })
			rebindForm.setOptions({ factorsR: ["TestLetters", "TestNominal"] })

			compare(modelTerms(), JSON.stringify([ { components: ["TestLetters"] }, { components: ["TestNominal"] }, { components: ["TestLetters", "TestNominal"] } ]), "Without model terms, a new factor still gets its default interaction")
		}
	}
}
