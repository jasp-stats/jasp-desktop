import QtQuick
import JASP.Controls
import JASP

// Section "Filter source with some types" of the jaspTestModule analysis "Test Sources with special attributes"
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesTypeFilter"

	property alias form: jaspForm // The form must be in the context of the JASPControls

	Form
	{
		id:		jaspForm

		VariablesForm
		{
			AvailableVariablesList	{ name: "allVariablesList3";	id: allVariablesList3	}
			AssignedVariablesList	{ name: "selectedVariables";	id: selectedVariables	}
		}

		DropDown
		{
			id		: scaleVariables
			name	: "scaleVariables"
			source	: [ {name: "selectedVariables", use: "type=scale"} ]
		}

		DropDown
		{
			id		: ordinalVariables
			name	: "ordinalVariables"
			source	: [ {name: "selectedVariables", use: "type=ordinal"} ]
		}

		DropDown
		{
			id		: ordinalAndNominalVariables
			name	: "ordinalAndNominalVariables"
			source	: [ {name: "selectedVariables", use: "type=ordinal|nominal"} ]
		}
	}

	function test_typeFilter()
	{
		moveVariables(allVariablesList3, selectedVariables, ["TestDoubles", "TestLetters"])
		checkList(scaleVariables,				["TestDoubles"],	"type=scale")
		checkList(ordinalVariables,				[],					"type=ordinal without ordinal variable")
		checkList(ordinalAndNominalVariables,	["TestLetters"],	"type=ordinal|nominal")

		moveVariables(allVariablesList3, selectedVariables, ["TestInts", "TestNominal"])
		checkList(scaleVariables,				["TestDoubles"],								"type=scale")
		checkList(ordinalVariables,				["TestInts"],									"type=ordinal")
		checkList(ordinalAndNominalVariables,	["TestLetters", "TestInts", "TestNominal"],		"type=ordinal|nominal")

		// If the value is not in the source anymore, the DropDown gets automatically another value
		ordinalAndNominalVariables.currentValue = "TestInts"
		compare(ordinalAndNominalVariables.currentValue, "TestInts")
		moveVariables(selectedVariables, allVariablesList3, ["TestInts"])
		checkList(ordinalVariables,				[],								"type=ordinal without TestInts")
		compare(ordinalVariables.currentValue,	"",								"No ordinal variable: no value")
		checkList(ordinalAndNominalVariables,	["TestLetters", "TestNominal"],	"type=ordinal|nominal without TestInts")
		verify(["TestLetters", "TestNominal"].indexOf(ordinalAndNominalVariables.currentValue) >= 0,
			   "The DropDown value should be set to another value, but is '" + ordinalAndNominalVariables.currentValue + "'")
		checkList(scaleVariables,				["TestDoubles"],				"type=scale is not changed")

		ordinalAndNominalVariables.currentValue = "TestNominal"
		compare(ordinalAndNominalVariables.currentValue, "TestNominal")

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		ordinalAndNominalVariables.currentValue = "TestLetters"
		reloadOptions(options)

		checkList(selectedVariables,			["TestDoubles", "TestLetters", "TestNominal"],	"Selected variables after reload")
		checkList(scaleVariables,				["TestDoubles"],								"type=scale after reload")
		checkList(ordinalVariables,				[],												"type=ordinal after reload")
		checkList(ordinalAndNominalVariables,	["TestLetters", "TestNominal"],					"type=ordinal|nominal after reload")
		compare(scaleVariables.currentValue,				"TestDoubles")
		compare(ordinalAndNominalVariables.currentValue,	"TestNominal")

		// The controls must still work normally after a reload (setOptions must not set them up twice)
		moveVariables(allVariablesList3, selectedVariables, ["TestInts"])
		checkList(selectedVariables,			["TestDoubles", "TestLetters", "TestNominal", "TestInts"],	"Selected variables after a move following the reload")
		checkList(ordinalVariables,				["TestInts"],												"type=ordinal after a move following the reload")
		checkList(ordinalAndNominalVariables,	["TestLetters", "TestNominal", "TestInts"],					"type=ordinal|nominal after a move following the reload")
		compare(ordinalAndNominalVariables.currentValue,	"TestNominal")
	}
}
