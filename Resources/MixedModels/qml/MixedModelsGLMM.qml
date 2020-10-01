//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3
import "./common"		as MM

Form {
	id: form

	VariablesForm
	{
		preferredHeight: 350

		AvailableVariablesList
		{
			name:				"allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"dependentVariable"
			title:				qsTr("Dependent variable")
			allowedColumns:		["scale","ordinal", "nominal"]
			singleVariable:		true
		}

		AssignedVariablesList
		{
			visible:			family.currentText == "Binomial (aggregated)"
			onVisibleChanged:	if (!visible && count > 0) itemDoubleClicked(0);
			name:				"dependentVariableAggregation"
			title:				qsTr("Number of trials")
			singleVariable:		true
			allowedColumns:		["scale","ordinal"]

			onEnabledChanged: if (!enabled && count > 0) itemDoubleClicked(0)
		}

		AssignedVariablesList
		{
			name:				"fixedVariables"
			title:				qsTr("Fixed effects variables")
			allowedColumns:		["ordinal", "nominal","scale","nominalText"]
			itemType:			"fixedFactors"
		}
		
		AssignedVariablesList
		{
			name:				"randomVariables"
			title:				qsTr("Random effects grouping factors")
			allowedColumns:		["ordinal", "nominal","nominalText"]
		}
	}

	Group
	{

		DropDown
		{
			name:				"family"
			label:				qsTr("Family")
			id:					family
			indexDefaultValue:	0
			values:
			[
				{ label: qsTr("Binomial"),				value: "binomial"},
				{ label: qsTr("Binomial (aggregated)"),	value: "binomial_agg"},
				{ label: qsTr("Gaussian"),				value: "gaussian"},
				{ label: qsTr("Gamma"),					value: "Gamma"},
				{ label: qsTr("Inverse Gaussian"),		value: "inverse.gaussian"},
				{ label: qsTr("Poisson"),				value: "poisson"}
			]

			property var familyMap:
			{
				"binomial":			["logit", "probit", "cauchit", "cloglog", "log"],
				"gaussian":			["identity", "log", "inverse"],
				"Gamma":			["identity", "log", "inverse"],
				"inverse.gaussian":	["identity", "log", "inverse"],
				"poisson":			["identity", "log", "sqrt"]
			}

			onCurrentValueChanged:
			{
				if (!familyMap[currentValue].includes(link.value))
				{
					for (var i = 0; i < link.buttons.length; i++)
						if (familyMap[currentValue].includes(link.buttons[i].parent.value))
						{
							link.buttons[i].parent.click()
							break;
						}
				}
			}
		}

		RadioButtonGroup
		{
			id:						link
			name:					"link"
			title:					qsTr("Link")
			radioButtonsOnSameRow:	true
			
			RadioButton
			{
				label:		qsTr("Logit")
				value:		"logit"
				visible:	family.familyMap[family.currentValue].includes(value)
				checked:	true
			}
			
			RadioButton
			{
				label:		qsTr("Probit")
				value:		"probit"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Cauchit")
				value:		"cauchit"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Complementary LogLog")
				value:		"cloglog"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Identity")
				value:		"identity"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Log")
				value:		"log"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Sqrt")
				value:		"sqrt"
				visible:	family.familyMap[family.currentValue].includes(value)
			}

			RadioButton
			{
				label:		qsTr("Inverse")
				value:		"inverse"
				visible:	family.familyMap[family.currentValue].includes(value)
			}
		}
	}

	MM.MixedModelsModel {}

	MM.MixedModelsOptions { allMethodOptions: false }

	MM.MixedModelsPlots {}

	Section
	{
		title:		qsTr("Estimated marginal means")
		expanded:	false

		VariablesForm
		{
			preferredHeight:	250

			AvailableVariablesList
			{
				name:	"availableModelComponentsMeans"
				title:	qsTr("Model variables")
				source: [{ name: "fixedEffects", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:		marginalMeans
				name:	"marginalMeans"
				title:	qsTr("Selected variables")
			}
		}

		CIField
		{
			name:	"marginalMeansCIwidth"
			label:	qsTr("Confidence interval")
		}

		DoubleField
		{
			id:				marginalMeansSD
			name:			"marginalMeansSD"
			label:			qsTr("SD factor covariates")
			defaultValue: 	1
			min:			0
			enabled:		marginalMeans.columnsTypes.includes("scale")
		}

		Group
		{
			columns: 2

			CheckBox
			{
				name:	"marginalMeansCompare"
				id:		marginalMeansCompare
				label:	qsTr("Compare marginal means to:")
			}

			DoubleField
			{
				enabled:	marginalMeansCompare.checked
				name:		"marginalMeansCompareTo"
			}
		}
		
		CheckBox
		{
			name: "marginalMeansResponse"
			label: qsTr("Response scale")
			checked: true
		}

		CheckBox
		{
			name:	"marginalMeansContrast"
			id:		marginalMeansContrast
			label:	qsTr("Specify contrasts")
		}

		DropDown
		{
			name:	"marginalMeansAdjustment"
			label:	qsTr("P-value adjustment")
			values:
			[
				{ label: "Holm",					value: "holm"},
				{ label: qsTr("Multivariate-t"),		value: "mvt"},
				{ label: "Scheffe",					value: "scheffe"},
				{ label: "Tukey",					value: "tukey"},
				{ label: qsTr("None"),				value: "none"},
				{ label: "Bonferroni",				value: "bonferroni"},
				{ label: "Hommel",					value: "hommel"}
			]
		}

		CustomContrastsTableView
		{
			Layout.columnSpan:	2
			visible:			marginalMeansContrast.checked
			name:				"Contrasts"
			source:				"marginalMeans"
			scaleFactor:		marginalMeansSD.value
		}
	}

	Section
	{
		title:		qsTr("Estimated trends/conditional slopes")
		expanded:	false

		VariablesForm
		{
			preferredHeight: 100

			AvailableVariablesList
			{
				name:	"availableModelComponentsTrends1"
				title:	qsTr("Continous variables")
				source: [ { name: "fixedEffects", use: "type=scale"} ]
			}

			AssignedVariablesList
			{
				singleVariable:	true
				name:			"trendsTrend"
				title:			qsTr("Trend variable")
			}
		}

		VariablesForm
		{
			preferredHeight: 250

			AvailableVariablesList
			{
				name:	"availableModelComponentsTrends2"
				title:	qsTr("Model variables")
				source:	[{ name: "fixedEffects", use: "noInteraction" }]
			}

			AssignedVariablesList
			{
				id:		trendsVariables
				name:	"trendsVariables"
				title:	qsTr("Selected variables")
			}
		}

		CIField
		{
			name:	"trendsCIwidth"
			label:	qsTr("Confidence interval")
		}

		DoubleField
		{ 
			id:				trendsSD
			name:			"trendsSD"
			label:			qsTr("SD factor covariates")
			defaultValue:	1
			min:			0
			enabled:		trendsVariables.columnsTypes.includes("scale")
		}

		Group
		{
			columns: 2

			CheckBox
			{
				name:	"trendsCompare"
				id:		trendsCompare
				label:	qsTr("Compare trends to:")
			}

			DoubleField
			{
				enabled:	trendsCompare.checked
				name:		"trendsCompareTo"
			}
		}

		CheckBox
		{
			name:	"trendsContrast"
			id:		trendsContrast
			label:	qsTr("Specify contrasts")
		}

		DropDown
		{
			name:	"trendsAdjustment"
			label:	qsTr("P-value adjustment")
			values:
			[
				{ label: "Holm",					value: "holm"},
				{ label: qsTr("Multivariate-t"),	value: "mvt"},
				{ label: "Scheffe",					value: "scheffe"},
				{ label: "Tukey",					value: "tukey"},
				{ label: qsTr("None"),				value: "none"},
				{ label: "Bonferroni",				value: "bonferroni"},
				{ label: "Hommel",					value: "hommel"}
			]
		}

		CustomContrastsTableView
		{
			Layout.columnSpan:	2
			visible:			trendsContrast.checked
			name:				"trendsContrasts"
			source:				"trendsVariables"
			scaleFactor:		trendsSD.value
		}
	}

}
