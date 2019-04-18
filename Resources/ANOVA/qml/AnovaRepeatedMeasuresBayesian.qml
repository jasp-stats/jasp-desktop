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
import QtQuick 2.8
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
	usesJaspResults: false

	CheckBox { visible: false; name: "posteriorEstimates" }
	PercentField { visible: false; name: "posteriorEstimatesCredibleIntervalInterval"   ; defaultValue: 95 }
	IntegerField { visible: false; name: "posteriorEstimatesMCMCIterations"             ; defaultValue: 1 }

	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"             ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"           ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"              ; defaultValue: 450 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"            ; defaultValue: 350 }


	VariablesForm
	{
		height: 520
		AvailableVariablesList { name: "allVariablesList" }		
		RepeatedMeasuresFactorsList
		{
			name: "repeatedMeasuresFactors"
			title: qsTr("Repeated Measures Factors")
			height: 180
		}
		AssignedVariablesList
		{
			name: "repeatedMeasuresCells"
			title: qsTr("Repeated Measures Cells")
			allowedColumns: ["scale"]
			listViewType: "RepeatedMeasures"
			source: "repeatedMeasuresFactors"
			height: 140
		}
		AssignedVariablesList
		{
			name: "betweenSubjectFactors"
			title: qsTr("Between Subject Factors")
			allowedColumns: ["ordinal", "nominal"]
			itemType: "fixedFactors"
		}
		AssignedVariablesList
		{
			name: "covariates"
			title: qsTr("Covariates")
			allowedColumns: ["scale"]
		}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Output")
		CheckBox
		{
			name: "effects"; label: qsTr("Effects")
			RadioButtonGroup
			{
				name: "effectsType"
				RadioButton { value: "allModels";		label: qsTr("Across all models"); checked: true	}
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models")			}
			}
		}
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
	}

	RadioButtonGroup
	{
		title: qsTr("Order")
		name: "bayesFactorOrder"
		RadioButton { value: "nullModelTop";	label: qsTr("Compare to null model"); checked: true	}
		RadioButton { value: "bestModelTop";	label: qsTr("Compare to best model")					}
	}

	Section
	{
		title: qsTr("Model")

		VariablesForm
		{
			height: 200

			AvailableVariablesList
			{
				name: "components"
				title: qsTr("Components")
				source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]
				width: parent.width / 4
			}
			AssignedVariablesList
			{
				name: "modelTerms"
				title: qsTr("Model terms")
				width: parent.width * 5 / 9
				listViewType: "Interaction"

				ExtraControlColumn {
					type: "CheckBox"
					name: "isNuisance"
					title: "Add to null model"
					purpose: "nuisance"					
				}
			}
		}
	}


	Section
	{
		title: qsTr("Post Hoc Tests")

		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "postHocTestsAvailable"; source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "postHocTestsVariables"; width: parent.width / 4; listViewType: "Interaction" }
		}

		Group
		{
			title: qsTr("Correction")
			CheckBox { name: "postHocTestsNullControl"; label: qsTr("Null control"); checked: true }
		}
	}

	Section
	{
		title: qsTr("Descriptives Plots")
		columns: 1

		VariablesForm
		{
			height: 150
			AvailableVariablesList { name: "descriptivePlotsVariables";	title: qsTr("Factors"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "plotHorizontalAxis";		title: qsTr("Horizontal axis");	singleVariable: true }
			AssignedVariablesList {  name: "plotSeparateLines";			title: qsTr("Separate lines");	singleVariable: true }
			AssignedVariablesList {  name: "plotSeparatePlots";			title: qsTr("Separate plots");	singleVariable: true }
		}

		TextField { name: "labelYAxis"; label: qsTr("Label y-axis"); fieldWidth: 200 }
		CheckBox
		{
			name: "plotCredibleInterval"; label: qsTr("Credible interval")
			childrenOnSameRow: true
			PercentField { name: "plotCredibleIntervalInterval"; defaultValue: 95 }
		}
	}

	Section
	{
		title: qsTr("Additional Options")

		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorFixedEffects";	label: qsTr("r scale fixed effects"); defaultValue: 0.5; fieldWidth: 50; max: 2; decimals: 1 }
			DoubleField { name: "priorRandomEffects";	label: qsTr("r scale random effects"); defaultValue: 1; fieldWidth: 50; max: 2; decimals: 1 }
			DoubleField { name: "priorCovariates";		label: qsTr("r scale covariates"); defaultValue: 0.354; fieldWidth: 50; max: 2; decimals: 1 }
		}

		RadioButtonGroup
		{
			name: "sampleMode"
			title: qsTr("Samples")
			RadioButton
			{
				value: "auto";		label: qsTr("Auto"); checked: true
			}
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField { name: "fixedSamplesNumber"; label: qsTr("No. samples"); defaultValue: 10000; fieldWidth: 50 }
			}
		}
	}
}
