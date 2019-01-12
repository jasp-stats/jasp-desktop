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
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
	id: form
	
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
		FactorsList
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
			listViewType: "MeasuresCells"
			syncModels: "repeatedMeasuresFactors"
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
	
	GridLayout
	{
		BayesFactorType {}
		
		GroupBox
		{
			title: qsTr("Output")
			CheckBox { text: qsTr("Effects"); name: "effects"; id: effectsOutput}
			RadioButtonGroup
			{
				indent: true
				enabled: effectsOutput.checked
				name: "effectsType"
				RadioButton { text: qsTr("Across all models"); name: "allModels"; checked: true}
				RadioButton { text: qsTr("Across matched models"); name: "matchedModels"}
			}
			CheckBox { text: qsTr("Descriptives"); name: "descriptives"}
		}
		
		RadioButtonGroup
		{
			title: qsTr("Order")
			name: "bayesFactorOrder"
			RadioButton { text: qsTr("Compare to null model"); name: "nullModelTop"; checked: true}
			RadioButton { text: qsTr("Compare to best model"); name: "bestModelTop"}
		}
	}
	
	
	ExpanderButton
	{
		text: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList
			{
				title: qsTr("Components")
				name: "components"
				syncModels: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]
				width: parent.width / 4
			}
			AssignedVariablesList
			{
				title: qsTr("Model terms")
				name: "modelTerms"
				listViewType: "Interaction"
				
				ExtraControlColumn {
					type: "CheckBox"
					name: "isNuisance"
					title: "Add to null model"
				}				
			}
		}
	}
	
	
	ExpanderButton
	{
		text: qsTr("Post Hoc Tests")
		
		VariablesForm
		{
			height: 200
			availableVariablesList { name: "postHocTestsAvailable"; syncModels: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "postHocTestsVariables" }
		}
		
		GroupBox
		{
			title: qsTr("Correction")
			CheckBox { text: qsTr("Null control"); name: "postHocTestsNullControl"; checked: true }
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Descriptives Plots")
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "descriptivePlotsVariables";	title: qsTr("Factors"); syncModels: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "plotHorizontalAxis";		title: qsTr("Horizontal axis");	singleItem: true }
			AssignedVariablesList {  name: "plotSeparateLines";			title: qsTr("Separate lines");	singleItem: true }
			AssignedVariablesList {  name: "plotSeparatePlots";			title: qsTr("Separate plots");	singleItem: true }
		}
		
		TextField { text: qsTr("Label y-axis") ; name: "labelYAxis"}
		RowLayout
		{
			CheckBox { text: qsTr("Credible interval"); name: "plotCredibleInterval"; id: plotCredibleInterval }
			PercentField { name: "plotCredibleIntervalInterval"; defaultValue: 95; enabled: plotCredibleInterval.checked}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Additional Options")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Prior")
				DoubleField { text: qsTr("r scale fixed effects"); name: "priorFixedEffects"; defaultValue: 0.5; fieldWidth: 50; doubleValidator {top: 2; decimals: 1} }
				DoubleField { text: qsTr("r scale random effects"); name: "priorRandomEffects"; defaultValue: 1; fieldWidth: 50; doubleValidator {top: 2; decimals: 1} }
				DoubleField { text: qsTr("r scale covariates"); name: "priorCovariates"; defaultValue: 0.354; fieldWidth: 50; doubleValidator {top: 2; decimals: 1} }
			}
			
			RadioButtonGroup
			{
				name: "sampleMode"
				RadioButton { text: qsTr("Auto"); name: "auto"; checked: true }
				RadioButton { text: qsTr("Manual"); name: "manual"; id: samplesManual }
				IntegerField
				{
					text: qsTr("No. samples")
					name: "fixedSamplesNumber"
					defaultValue: 10000
					fieldWidth: 50
					enabled: samplesManual.checked
					indent: true
				}
			}
		}
	}
}
