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
			allowedColumns:		["scale"]
			singleVariable:		true
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


	Section
	{
		title:			qsTr("Model")

		VariablesForm
		{
			preferredHeight:	250

			AvailableVariablesList
			{
				name:	"availableModelComponents"
				title:	qsTr("Model components")
				source:	"fixedVariables"
			}

			AssignedVariablesList
			{
				id:				fixedEffects
				name:			"fixedEffects"
				title:			qsTr("Fixed effects")
				listViewType:	JASP.Interaction
			}
		}

		ComponentsList
		{
			id:					randomEffetcs
			title:				qsTr("Random effects")
			name:				"randomEffects"
			source:				"randomVariables"
			visible:			count > 0

			rowComponent: Group
			{
				RowLayout
				{
					Layout.preferredWidth:	randomEffetcs.width
					Label { text: qsTr("Random slopes by %1").arg(rowValue); Layout.preferredWidth: parent.width / 2 }
					CheckBox { label: qsTr("Correlations"); name: "correlations"; checked: true; Layout.preferredWidth: parent.width / 2 }
				}
				ComponentsList
				{
					name:				"randomComponents"
					source:				"fixedEffects"

					rowComponent: CheckBox { name: "randomSlopes"; label: rowValue; checked: true }
				}
			}
		}
		

	}

	MM.MixedModelsBOptions {}

	MM.MixedModelsMCMCDiagnostics {}

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
		
		CheckBox
		{
			name:	"marginalMeansContrast"
			id:		marginalMeansContrast
			label:	qsTr("Specify contrasts")
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

		CheckBox
		{
			name:	"trendsContrast"
			id:		trendsContrast
			label:	qsTr("Specify contrasts")
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
