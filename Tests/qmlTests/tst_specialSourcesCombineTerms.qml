import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

// Section "Source with combineTerms" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesCombineTerms"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList";	id: allVariablesList	}
			AssignedVariablesList	{ name: "variables";		id: variables			}
		}

		ComponentsList
		{
			id		: combinations2way
			name	: "combinations2way"
			source	: [ {name: "variables", combineTerms: JASP.Combination2Way}]

			rowComponent: RowLayout
			{
				property var pair : rowValue.split(INTERACTION_SEPARATOR)

				DropDown
				{
					name			: "mediation"
					addEmptyValue	: true
					source			: [{name: "variables", discard: {values: pair} }]
				}
			}
		}

		ComponentsList
		{
			id		: combinations3way
			name	: "combinations3way"
			source	: [ {name: "variables", combineTerms: JASP.Combination3Way}]

			rowComponent: RowLayout
			{
				property var triple : rowValue.split(INTERACTION_SEPARATOR)

				DropDown
				{
					name			: "mediation"
					addEmptyValue	: true
					source			: [{name: "variables", discard: {values: triple} }]
				}
			}
		}

		ComponentsList
		{
			id		: combinationCross
			name	: "combinationCross"
			source	: [ {name: "variables", combineTerms: JASP.CombinationCross}]

			rowComponent: Text { text: rowValue }
		}
	}

	function test_combineTerms()
	{
		moveVariables(allVariablesList, variables, ["TestDoubles", "TestInts"])

		checkList(combinations2way,	[inter(["TestDoubles", "TestInts"])],								"Combination2Way with 2 variables")
		checkList(combinations3way,	[],																	"Combination3Way with 2 variables")
		checkList(combinationCross,	["TestDoubles", "TestInts", inter(["TestDoubles", "TestInts"])],	"CombinationCross with 2 variables")

		moveVariables(allVariablesList, variables, ["TestLetters", "TestNominal"])

		checkList(combinations2way,
				  [inter(["TestDoubles", "TestInts"]),		inter(["TestDoubles", "TestLetters"]),	inter(["TestDoubles", "TestNominal"]),
				   inter(["TestInts", "TestLetters"]),		inter(["TestInts", "TestNominal"]),		inter(["TestLetters", "TestNominal"])],
				  "Combination2Way gives all pairs")
		checkList(combinations3way,
				  [inter(["TestDoubles", "TestInts", "TestLetters"]),	inter(["TestDoubles", "TestInts", "TestNominal"]),
				   inter(["TestDoubles", "TestLetters", "TestNominal"]),	inter(["TestInts", "TestLetters", "TestNominal"])],
				  "Combination3Way gives all triples")
		compare(combinationCross.count,			15, "CombinationCross with 4 variables gives all 2^4 - 1 combinations")
		compare(combinationCross.columnsNames[14],	inter(["TestDoubles", "TestInts", "TestLetters", "TestNominal"]))

		// The extra DropDown gets the selected variables minus the ones used by the combination
		var pairKey		= inter(["TestDoubles", "TestLetters"])
		var tripleKey	= inter(["TestDoubles", "TestInts", "TestNominal"])
		var mediation2	= rowControl(combinations2way, pairKey,		"mediation")
		var mediation3	= rowControl(combinations3way, tripleKey,	"mediation")
		checkList(mediation2, ["", "TestInts", "TestNominal"],	"Mediation DropDown of a pair")
		checkList(mediation3, ["", "TestLetters"],				"Mediation DropDown of a triple")

		mediation2.currentValue = "TestNominal"
		mediation3.currentValue = "TestLetters"
		compare(mediation2.currentValue, "TestNominal")
		compare(mediation3.currentValue, "TestLetters")

		// Removing a variable removes its combinations, and the variable from the mediation DropDowns
		moveVariables(variables, allVariablesList, ["TestInts"])
		checkList(combinations2way,
				  [inter(["TestDoubles", "TestLetters"]),	inter(["TestDoubles", "TestNominal"]),	inter(["TestLetters", "TestNominal"])],
				  "Combination2Way without TestInts")
		checkList(combinations3way, [inter(["TestDoubles", "TestLetters", "TestNominal"])], "Combination3Way without TestInts")
		compare(combinationCross.count, 7, "CombinationCross with 3 variables")
		mediation2 = rowControl(combinations2way, pairKey, "mediation")
		checkList(mediation2, ["", "TestNominal"], "Mediation DropDown of a pair without TestInts")
		compare(mediation2.currentValue, "TestNominal", "The mediation value is kept")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		mediation2.currentValue = ""
		reloadOptions(options)

		checkList(variables,	["TestDoubles", "TestLetters", "TestNominal"], "Variables after reload")
		compare(combinations2way.count,	3)
		compare(combinations3way.count,	1)
		compare(combinationCross.count,	7)
		compare(rowControl(combinations2way, pairKey, "mediation").currentValue, "TestNominal", "Mediation value of the pair is restored")

		// Reload a second time, and check that the controls still work normally (setOptions must not set them up twice)
		reloadOptions(options)
		compare(rowControl(combinations2way, pairKey, "mediation").currentValue, "TestNominal", "Mediation value of the pair is restored after a second reload")

		moveVariables(allVariablesList, variables, ["TestInts"])
		compare(combinations2way.count,	6, "Combination2Way after a move following the reloads")
		compare(combinations3way.count,	4, "Combination3Way after a move following the reloads")
		compare(combinationCross.count,	15, "CombinationCross after a move following the reloads")
		checkList(rowControl(combinations2way, pairKey, "mediation"), ["", "TestNominal", "TestInts"], "Mediation DropDown of a pair after the move")

		moveVariables(variables, allVariablesList, ["TestDoubles", "TestNominal"])
		checkList(combinations2way, [inter(["TestLetters", "TestInts"])], "Combination2Way after removing 2 variables")
	}
}
