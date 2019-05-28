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
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";		title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true	}
		AssignedVariablesList { name: "fixedFactors";	title: qsTr("Fixed Factors");		suggestedColumns: ["ordinal", "nominal"]			}
		AssignedVariablesList { name: "randomFactors";	title: qsTr("Random Factors");		suggestedColumns: ["ordinal", "nominal"]			}
	}
	
	BayesFactorType { }

	Group
	{
		title: qsTr("Tables")
        CheckBox
        {
            name: "effects"; label: qsTr("Effects");
            RadioButtonGroup
            {
                name: "effectsType"
                RadioButton { value: "allModels";		label: qsTr("Across all models"); checked: true	}
                RadioButton { value: "matchedModels";	label: qsTr("Across matched models")				}
            }
        }
        CheckBox { name: "posteriorEstimates"; label: qsTr("Estimates") }
        CheckBox { name: "criTable";           label: qsTr("Model averaged R\u00B2") }
        CheckBox { name: "descriptives";       label: qsTr("Descriptives") }
        CIField {  name: "credibleInterval";   label: qsTr("Credible interval") }
    }

    RadioButtonGroup
    {
        title: qsTr("Order")
        name: "bayesFactorOrder"
        RadioButton { value: "bestModelTop"; label: qsTr("Compare to best model"); checked: true}
        RadioButton { value: "nullModelTop"; label: qsTr("Compare to null model")               }
    }

    GroupBox
    {
        title: qsTr("Plots")
        CheckBox {
            label: qsTr("Model averaged posteriors");    name: "posteriorPlot"
            RadioButtonGroup
            {
                name: "groupPosterior"
                RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); checked: true}
                RadioButton { value: "individual";	label: qsTr("Individual plot per level")                 }
            }
        }
        CheckBox { label: qsTr("Q-Q plot of residuals") ;    name: "qqPlot" }
        CheckBox { label: qsTr("Posterior R\u00B2") ;        name: "rsqPlot"}
    }

	Section
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			
			AvailableVariablesList { name: "components"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"]}
			AssignedVariablesList
			{
				name: "modelTerms"
				title: qsTr("Model Terms")
				listViewType: "Interaction"

				
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
        title: qsTr("Single Model Inference")

        VariablesForm
        {
            height: 200
            AvailableVariablesList { name: "components2"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"]}
            AssignedVariablesList
            {
				title: qsTr("Specific Model Terms")
                name: "singleModelTerms"
                listViewType: "Interaction"
            }
        }

        GridLayout
        {

            GroupBox
            {
                title: qsTr("Tables")
                CheckBox { label: qsTr("Estimates"); name: "singleModelEstimates"}
                CheckBox { label: qsTr("R\u00B2");   name: "singleModelCriTable" }
            }

            GroupBox
            {
                title: qsTr("Plots")
                CheckBox { 
                    label: qsTr("Marginal posteriors");    name: "singleModelPosteriorPlot"
                    RadioButtonGroup
                    {
                        name: "singleModelGroupPosterior"
                        RadioButton { value: "grouped";		label: qsTr("Group levels in single plot"); checked: true}
                        RadioButton { value: "individual";	label: qsTr("Individual plot per level")                 }
                    }
                }
                CheckBox { label: qsTr("Q-Q plot of residuals");  name: "singleModelqqPlot" }
                CheckBox { label: qsTr("Posterior R\u00B2") ;     name: "singleModelrsqPlot"}
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

			AvailableVariablesList { name: "descriptivePlotsVariables" ;	title: qsTr("Factors"); source: "fixedFactors" }
			AssignedVariablesList { name: "plotHorizontalAxis";				title: qsTr("Horizontal Axis");	singleVariable: true }
			AssignedVariablesList { name: "plotSeparateLines";				title: qsTr("Separate Lines");	singleVariable: true }
			AssignedVariablesList { name: "plotSeparatePlots";				title: qsTr("Separate Plots");	singleVariable: true }
		}
		
		Group
		{
			title: qsTr("Display")
			CheckBox
			{
				name: "plotCredibleInterval"; label: qsTr("Credible interval")
				childrenOnSameRow: true
				CIField { name: "plotCredibleIntervalInterval" }
			}
		}
	}
	
	Section
	{
		title: qsTr("Additional Options")
		
		Group
		{
			title: qsTr("Prior")
			DoubleField { name: "priorFixedEffects";	label: qsTr("r scale fixed effects");  defaultValue: 0.5; max: 2; decimals: 1 }
			DoubleField { name: "priorRandomEffects";	label: qsTr("r scale random effects"); defaultValue: 1;   max: 2; decimals: 1 }
		}

        RadioButtonGroup
		{
			name: "sampleModeNumAcc"
			title: qsTr("Numerical Accuracy")
            RadioButton { value: "auto";	label: qsTr("Auto"); checked: true }
			RadioButton
			{
                value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedNumAcc"
                    label: qsTr("No. samples")
					defaultValue: 1e4
					fieldWidth: 50
                    min: 100
                    max: 1e7
				}
			}
		}

        RadioButtonGroup
		{
			name: "sampleModeMCMC"
			title: qsTr("Posterior Samples")
            RadioButton { value: "auto";	label: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedMCMCSamples"
                    label: qsTr("No. samples")
					defaultValue: 1e3
					fieldWidth: 50
                    min: 100
                    max: 1e7
				}
			}
		}
	}
}
