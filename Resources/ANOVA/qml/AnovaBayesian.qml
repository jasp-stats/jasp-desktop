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

	IntegerField { visible: false; name: "plotHeightDescriptivesPlotLegend"         ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotHeightDescriptivesPlotNoLegend"       ; defaultValue: 300 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotLegend"          ; defaultValue: 430 }
	IntegerField { visible: false; name: "plotWidthDescriptivesPlotNoLegend"        ; defaultValue: 350 }
	DoubleField  { visible: false; name: "posteriorEstimatesCredibleIntervalInterval"; defaultValue: 0.950 }
	DoubleField  { visible: false; name: "posteriorEstimatesMCMCIterations"          ; defaultValue: 10000 }
	CheckBox     { visible: false; name: "posteriorEstimates"  }
	CheckBox     { visible: false; name: "posteriorDistribution"  }

	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleVariable: true	}
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		allowedColumns: ["ordinal", "nominal"]		}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		allowedColumns: ["ordinal", "nominal"]		}
	}
	
	BayesFactorType { }

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
				RadioButton { value: "matchedModels";	label: qsTr("Across matched models")				}
			}
		}

        GroupBox
        {
            title: qsTr("Plots")
            CheckBox { text: qsTr("Model averaged posteriors"); name: "modelAveragedPosteriors"}
            CheckBox { text: qsTr("Q-Q plot of residuals") ; name: "qqPlot" }
            //CheckBox { text: qsTr("Effects"); name: "effects"; id: effectsOutput}
        }

	}
	
	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"]; width: parent.width / 4 }
			AssignedVariablesList
			{
				name: "modelTerms"
				title: qsTr("Model terms")
				listViewType: "Interaction"
				width: parent.width * 5 / 9
				
				ExtraControlColumn
				{
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
        text: qsTr("Single Model Inference")

        GridLayout
        {

            GroupBox
            {
                title: qsTr("Tables")
                CheckBox { text: qsTr("Effects"); name: "singleModelEffects"}
            }

            GroupBox
            {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Marginal posteriors"); name: "singleModelPosteriors"}
                CheckBox { text: qsTr("Q-Q plot of residuals") ; name: "singleModelqqPlot" }
            }

        }

        VariablesForm
        {
            height: 200
            //showDefaultAssignedVariablesList: false // Cannot use defaultAssignedVariablesList with an ExtraControlColumn
            listWidth: parent.width * 5 / 9

            availableVariablesList
            {
                title: qsTr("Components")
                name: "components2"
                syncModels: ["fixedFactors", "randomFactors"]
                width: parent.width / 4
            }
            AssignedVariablesList
            {
                title: qsTr("Specific model terms")
                name: "singleModelTerms"
                listViewType: "Interaction"

//                ExtraControlColumn {
//                    type: "CheckBox"
//                    name: "inModel"
//                    title: "Add to model shown"
//                }

            }
        }
    }
	Section
	{
		title: qsTr("Post Hoc Tests")
		
		VariablesForm
		{
			height: 200
			AvailableVariablesList { name: "postHocTestsAvailable"; source: "fixedFactors" }
			AssignedVariablesList {  name: "postHocTestsVariables" }
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
		
		VariablesForm
		{
			height: 140
			AvailableVariablesList { name: "descriptivePlotsVariables" ;	title: qsTr("Factors"); source: "fixedFactors" }
			AssignedVariablesList { name: "plotHorizontalAxis";				title: qsTr("Horizontal axis");	singleVariable: true }
			AssignedVariablesList { name: "plotSeparateLines";				title: qsTr("Separate lines");	singleVariable: true }
			AssignedVariablesList { name: "plotSeparatePlots";				title: qsTr("Separate plots");	singleVariable: true }
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox
			{
				name: "plotCredibleInterval"; label: qsTr("Credible interval")
				childrenOnSameRow: true
				PercentField { name: "plotCredibleIntervalInterval"; defaultValue: 95 }
			}
		}
	}
	
	Section
	{
		title: qsTr("Additional Options")
		
		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorFixedEffects";	label: qsTr("r scale fixed effects"); defaultValue: 0.5; max: 2; decimals: 1 }
			DoubleField { name: "priorRandomEffects";	label: qsTr("r scale random effects"); defaultValue: 1; max: 2; decimals: 1 }
		}

		RadioButtonGroup
		{
			name: "sampleMode"
			title: qsTr("Samples")
			RadioButton { value: "auto";	label: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedSamplesNumber"
					label: qsTr("No. samples")
					defaultValue: 10000
					fieldWidth: 50
				}
			}
		}
	}
}
