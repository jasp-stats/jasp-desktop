import QtTest
import QtQuick
import JASP.Controls

// The options of R syntax and of jaspTools tests (the syntax bridge rebinds an already initialized form) give the
// model terms of a FactorsForm as bare variable names, without types, as in a regression:
// Covariates (scale) and Factors (nominal) are the sources of the available terms of the model.
//  - The FactorsForm must be bound after these sources: bound before them, the available terms were not yet there
//    and a term of a factor was dropped from the model (an ordinal column in Factors, an interaction).
//  - A term gets the type its variable has in these sources (an ordinal column in Covariates is scale), not the
//    type of the column.
// And a computed column name is free when the data set has no column with this name: the base analysis of the
// syntax bridge refused every name ("already exists and is not created by this analysis").
//
// Fixture (Tests/testqml.cpp): TestInts (ordinal), TestLetters (nominal), TestDoubles (scale), TestNominal (nominal).
Item
{
	width:	1000
	height:	1200

	property alias form: sourceTermsForm

	Form
	{
		id:		sourceTermsForm
		width:	800 // The theme of the QML test engine has no form width

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVarsST" }
			AssignedVariablesList	{ name: "covariatesST";	allowedColumns: ["scale"] }
			AssignedVariablesList	{ name: "factorsST";	allowedColumns: ["nominal"] }
		}

		FactorsForm
		{
			id:								modelTermsST
			name:							"modelTermsST"
			nested:							true
			allowInteraction:				true
			initNumberFactors:				2
			baseName:						"model"
			baseTitle:						"Model"
			startIndex:						0
			availableVariablesList.source:	["covariatesST", "factorsST"]
			availableVariablesListName:		"availableTermsST"
			allowedColumns:					[]
		}

		ComputedColumnField { id: newColumnST; name: "newColumnST"; fieldWidth: 120 }
	}

	TestCase
	{
		name: "TestFactorsFormSourceTerms"

		SignalSpy
		{
			id:			spyLoader
			target:		sourceTermsForm
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
			const gen = JSON.parse(sourceTermsForm.createOptionsJsonAsJson(0))
			return gen.modelTermsST
		}

		function test_modelTermsKeepTheTermsOfTheirSources()
		{
			sourceTermsForm.setOptions({
				covariatesST:	["TestDoubles"],
				factorsST:		["TestInts"],	// ordinal column, nominal in Factors
				modelTermsST:	[
					{ name: "model0", title: "Model 0", components: [] },
					{ name: "model1", title: "Model 1", components: ["TestInts", "TestDoubles", ["TestInts", "TestDoubles"]] }
				]
			})

			const model1 = modelTerms()[1]
			compare(JSON.stringify(model1.components.value), JSON.stringify([["TestInts"], ["TestDoubles"], ["TestInts", "TestDoubles"]]), "No term of the model is dropped")
			compare(JSON.stringify(model1.components.types), JSON.stringify(["nominal", "scale", ["nominal", "scale"]]), "The terms have the types of their sources")
		}

		function test_termsGetTheTypeOfTheirSource()
		{
			sourceTermsForm.setOptions({
				covariatesST:	["TestInts"],	// ordinal column, scale in Covariates
				factorsST:		["TestLetters"],
				modelTermsST:	[
					{ name: "model0", title: "Model 0", components: ["TestInts"] },
					{ name: "model1", title: "Model 1", components: ["TestInts", "TestLetters", ["TestInts", "TestLetters"]] }
				]
			})

			const terms = modelTerms()
			compare(JSON.stringify(terms[0].components.types), JSON.stringify(["scale"]), "The covariate is scale in the first model")
			compare(JSON.stringify(terms[1].components.types), JSON.stringify(["scale", "nominal", ["scale", "nominal"]]), "and in the interaction")
		}

		function test_aSingleVariableAsString()
		{
			// R syntax gives a vector of length 1 as a string
			sourceTermsForm.setOptions({
				covariatesST:	["TestDoubles"],
				factorsST:		[],
				modelTermsST:	[
					{ name: "model0", title: "Model 0", components: [] },
					{ name: "model1", title: "Model 1", components: "TestDoubles" }
				]
			})

			compare(JSON.stringify(modelTerms()[1].components.value), JSON.stringify([["TestDoubles"]]), "The variable is the term of the model")
		}

		function test_computedColumnNameIsFreeWhenNoColumnHasIt()
		{
			try
			{
				sourceTermsForm.setOptions({ newColumnST: "A new column" })
				compare(newColumnST.hasError, false, "No column has this name")

				sourceTermsForm.setOptions({ newColumnST: "TestInts" })
				compare(newColumnST.hasError, true, "A column of the data set has this name")
			}
			finally
			{
				sourceTermsForm.setOptions({ newColumnST: "" })
			}
		}
	}
}
