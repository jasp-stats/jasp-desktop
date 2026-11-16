import QtQuick
import QtQuick.Layouts
import JASP.Controls
import JASP

// The nested TabViews of the "Moderation Options" of the MNLFA analysis (jaspSem, ModeratedNonLinearFactorAnalysis.qml):
// the ComponentsLists of the last layer have the factors (or their variables) of a FactorsForm as source, and the TabView
// above them depends also on the number of factors. Adding, removing or renaming a factor must be seen in all of them.
SpecialSourcesTestCase
{
	name:	"TestSpecialSourcesFactorsFormTabView"

	// The controls must be visible and have a size, otherwise the layouts (like the StackLayout of the TabView) are not computed
	width:		800
	height:		2000
	visible:	true
	when:		windowShown

	property alias form: jaspForm // The form must be in the context of the JASPControls

	// As in the MNLFA analysis, these functions are defined in the root object of the file
	function getValuesModOptions(rwValue, factorCount)
	{
		if (rwValue == "indicators")
			return [{value: "loadings", label: "Loadings"}, {value: "intercepts", label: "Intercepts"}, {value: "residualVariances", label: "Residual variances"}];
		if (factorCount == 2)
			return [{value: "variances", label: "Variances"}, {value: "means", label: "Means"}, {value: "covariances", label: "Covariances"}];
		return [{value: "variances", label: "Variances"}, {value: "means", label: "Means"}];
	}

	function concatFactorTitles(factorTitles)
	{
		var resultLabel = [];
		for (var i = 0; i < factorTitles.length; i++)
			resultLabel.push(factorTitles[i].label);
		return [resultLabel.join(":")];
	}

	Form
	{
		id:		jaspForm
		width:	800

		FactorsForm
		{
			id:					factors
			name:				"factors"
			initNumberFactors:	1
		}

		Section
		{
			id:			moderationSection
			title:		"Moderation Options"
			expanded:	true

			Group
			{
				title: "Invariance Tests"
				CheckBox { id: invarianceTestConfigural; name: "invarianceTestConfigural"; label: "Configural"; checked: true }
			}

			TabView
			{
				id:				modFirstLayer
			Layout.columnSpan: 1
			preferredWidth: 100
			Layout.fillWidth: true
				name:			"includeIndividualModerationsList"
				values:			invarianceTestConfigural.checked ? [{value: invarianceTestConfigural.name, label: invarianceTestConfigural.label}] : []
				optionKey:		"keyValue"
				optionKeyLabel:	"keyLabel"
				addItemManually: false

				rowComponent: TabView
				{
					id:				modSecondLayer
					name:			"moderationTypeList"
					addItemManually: false
					values:			[{value: "indicators", label: "Indicators"}, {value: "factors", label: "Factors"}]
					optionKey:		"keyValue"
					optionKeyLabel:	"keyLabel"

					rowComponent: TabView
					{
						id:				modThirdLayer
						property string moderationTypeValue: rowValue
						name:			"moderationParameterList"
						addItemManually: false
						optionKey:		"keyValue"
						optionKeyLabel:	"keyLabel"
						values:			getValuesModOptions(rowValue, factors.factorsTitles.length)

						rowComponent: ComponentsList
						{
							id:				modFourthLayer
							property string moderationParamValue: rowValue
							name:			"moderationItemList"
							addItemManually: false
							source:			modThirdLayer.moderationTypeValue == "indicators" ? factors.name : (rowValue == "covariances" ? {values: concatFactorTitles(factors.factorsTitles)} : {values: factors.factorsTitles})

							rowComponent: RowLayout
							{
								Text { text: rowLabel }
								CheckBox
								{
									name:		"includeIndividualModeration"
									checked:	modFourthLayer.moderationParamValue != "variances" && modFourthLayer.moderationParamValue != "means"
								}
							}
						}
					}
				}
			}
		}
	}

	function parameterList(type)
	{
		var typeList = rowControl(modFirstLayer, "invarianceTestConfigural", "moderationTypeList")
		return rowControl(typeList, type, "moderationParameterList")
	}

	function itemList(type, parameter)	{ return rowControl(parameterList(type), parameter, "moderationItemList") }
	function include(type, parameter, item)	{ return rowControl(itemList(type, parameter), item, "includeIndividualModeration") }

	function checkFactors(expected, message)
	{
		checkList(itemList("factors", "variances"),	expected, "Variances: " + message)
		checkList(itemList("factors", "means"),		expected, "Means: " + message)
	}

	function test_moderationTabView()
	{
		checkList(parameterList("factors"),		["variances", "means"],								"1 factor: no covariances")
		checkList(parameterList("indicators"),	["loadings", "intercepts", "residualVariances"],	"Indicators parameters")
		checkFactors(["Factor1"], "1 factor")

		// The indicators lists get the variables of the factors
		moveVariables(factors.availableVariablesList, factors.factorsItems[0], ["TestDoubles", "TestInts"])
		checkList(itemList("indicators", "loadings"), ["TestDoubles", "TestInts"], "Loadings get the variables of the factor")

		// Set a value that is not the default one, to check that it is kept
		include("factors", "variances", "Factor1").click()
		compare(include("factors", "variances", "Factor1").checked, true)

		factors.addFactor()
		checkList(parameterList("factors"), ["variances", "means", "covariances"], "2 factors: covariances added")
		checkFactors(["Factor1", "Factor2"], "the added factor is shown")
		checkList(itemList("factors", "covariances"), ["Factor 1:Factor 2"], "Covariances of the 2 factors")
		compare(include("factors", "variances", "Factor1").checked,	true,	"The value of an existing factor is kept when a factor is added")
		compare(include("factors", "variances", "Factor2").checked,	false,	"The added factor gets the default value")
		compare(include("factors", "means", "Factor2").checked,		false,	"The added factor gets the default value")

		moveVariables(factors.availableVariablesList, factors.factorsItems[1], ["TestLetters"])
		checkList(itemList("indicators", "loadings"), ["TestDoubles", "TestInts", "TestLetters"], "Loadings get the variables of all factors")

		factors.titleChanged(1, "Anxiety")
		checkList(itemList("factors", "covariances"), ["Factor 1:Anxiety"], "Covariances get the new title")
		checkFactors(["Factor1", "Factor2"], "a renamed factor keeps its row")
		compare(include("factors", "variances", "Factor1").checked, true, "The value is kept when a factor is renamed")

		factors.addFactor()
		checkList(parameterList("factors"), ["variances", "means"], "3 factors: no covariances")
		checkFactors(["Factor1", "Factor2", "Factor3"], "the third factor is shown")
		compare(include("factors", "variances", "Factor1").checked, true, "The value is kept when a third factor is added")

		factors.removeFactor()
		checkList(parameterList("factors"), ["variances", "means", "covariances"], "2 factors again: covariances added")
		checkFactors(["Factor1", "Factor2"], "the removed factor is not shown anymore")
		checkList(itemList("factors", "covariances"), ["Factor 1:Anxiety"], "Covariances of the 2 factors")
		compare(include("factors", "variances", "Factor1").checked, true, "The value is kept when a factor is removed")

		include("factors", "means", "Factor2").click()
		compare(include("factors", "means", "Factor2").checked, true)

		// Duplicate the analysis: the controls should have the same values
		var options = form.options()
		include("factors", "variances", "Factor1").click()
		include("factors", "means", "Factor2").click()
		reloadOptions(options)
		checkList(parameterList("factors"), ["variances", "means", "covariances"], "Parameters after reload")
		checkFactors(["Factor1", "Factor2"], "after reload")
		checkList(itemList("factors", "covariances"), ["Factor 1:Anxiety"], "Covariances after reload")
		compare(include("factors", "variances", "Factor1").checked,	true,	"Value restored after reload")
		compare(include("factors", "means", "Factor2").checked,		true,	"Value restored after reload")
		compare(include("factors", "means", "Factor1").checked,		false,	"Value restored after reload")

		factors.removeFactor()
		checkList(parameterList("factors"), ["variances", "means"], "1 factor: no covariances")
		checkFactors(["Factor1"], "the removed factor is not shown anymore")
		checkList(itemList("indicators", "loadings"), ["TestDoubles", "TestInts"], "Loadings without the variables of the removed factor")
		compare(include("factors", "variances", "Factor1").checked, true, "The value is kept when a factor is removed")
	}

	function waitForStableHeight(item)
	{
		// The layouts are computed asynchronously: wait until the height does not change anymore
		var previous = -1, stableCount = 0
		for (var i = 0; i < 40 && stableCount < 5; i++)
		{
			wait(25)
			stableCount = (item.height === previous) ? stableCount + 1 : 0
			previous = item.height
		}
		verify(stableCount >= 5, "The height of " + item + " should become stable")
	}

	function typeList() { return rowControl(modFirstLayer, "invarianceTestConfigural", "moderationTypeList") }

	function test_moderationTabViewHeights()
	{
		// The previous test ends with 1 factor having 2 variables: add a variable so that the indicators and the factors tabs have different heights
		moveVariables(factors.availableVariablesList, factors.factorsItems[0], ["TestNominal"])
		var second		= typeList()
		var indicators	= parameterList("indicators")
		var factorsTab	= parameterList("factors")
		tryVerify(function() { return indicators.height > factorsTab.height + 20 }, 1000, "The indicators tab (3 variables) should be higher than the factors tab (1 factor)")

		second.currentIndex = 0
		waitForStableHeight(moderationSection)
		var secondOverhead	= second.height - indicators.height		// Height of its tab bar
		var firstOverhead	= modFirstLayer.height - second.height
		var sectionHeight	= moderationSection.height

		// Switching to a tab with another height must change the height of the TabViews and of the Section containing them
		second.currentIndex = 1
		tryCompare(second,				"height", factorsTab.height + secondOverhead,	1000, "The second TabView gets the height of the factors tab")
		tryCompare(modFirstLayer,		"height", second.height + firstOverhead,		1000, "The first TabView follows the second one")
		tryVerify(function() { return moderationSection.height < sectionHeight }, 1000, "The Section gets smaller")

		second.currentIndex = 0
		tryCompare(second,				"height", indicators.height + secondOverhead,	1000, "The second TabView gets the height of the indicators tab again")
		tryCompare(modFirstLayer,		"height", second.height + firstOverhead,		1000, "The first TabView follows the second one")
		tryCompare(moderationSection,	"height", sectionHeight,						1000, "The Section gets its height back")

		// The height must also follow when the content of the current tab changes
		var indicatorsHeight = indicators.height
		moveVariables(factors.factorsItems[0], factors.availableVariablesList, ["TestNominal"])
		tryVerify(function() { return indicators.height < indicatorsHeight }, 1000, "The indicators tab gets smaller without a variable")
		tryCompare(second,				"height", indicators.height + secondOverhead,	1000, "The second TabView follows the content of its current tab")
		tryCompare(modFirstLayer,		"height", second.height + firstOverhead,		1000, "The first TabView follows the second one")

		// And when the content of a hidden tab changed, its height must be used when it is shown
		second.currentIndex = 1
		tryCompare(second, "height", factorsTab.height + secondOverhead, 1000, "The second TabView gets the height of the factors tab")
		moveVariables(factors.availableVariablesList, factors.factorsItems[0], ["TestNominal"])
		tryCompare(indicators, "height", indicatorsHeight, 1000, "The hidden indicators tab gets its height back")
		second.currentIndex = 0
		tryCompare(second,				"height", indicators.height + secondOverhead,	1000, "The second TabView gets the new height of the indicators tab")
		tryCompare(modFirstLayer,		"height", second.height + firstOverhead,		1000, "The first TabView follows the second one")
	}
}
