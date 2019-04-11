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
            name: "effects"; text: qsTr("Effects");
            RadioButtonGroup
            {
                name: "effectsType"
                RadioButton { value: "allModels";		text: qsTr("Across all models"); checked: true	}
                RadioButton { value: "matchedModels";	text: qsTr("Across matched models")				}
            }
        }
        CheckBox { name: "posteriorEstimates"; text: qsTr("Estimates") }
        CheckBox { name: "descriptives";       text: qsTr("Descriptives") }
        PercentField { name: "credibleInterval";	label: qsTr("Credible interval"); defaultValue: 95 }
    }

    RadioButtonGroup
    {
        title: qsTr("Order")
        name: "bayesFactorOrder"
        RadioButton { value: "nullModelTop"; text: qsTr("Compare to null model"); checked: true	}
        RadioButton { value: "bestModelTop"; text: qsTr("Compare to best model")				}
    }

    GroupBox
    {
        title: qsTr("Plots")
        CheckBox { text: qsTr("Model averaged posteriors"); name: "posteriorPlot"}
        CheckBox { text: qsTr("Q-Q plot of residuals") ;    name: "qqPlot" }
        CheckBox { text: qsTr("Posterior R\u00B2") ;        name: "rsqPlot"}
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
				title: qsTr("Model terms")
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
        text: qsTr("Single Model Inference")

        GridLayout
        {

            GroupBox
            {
                title: qsTr("Tables")
                CheckBox { text: qsTr("Estimates"); name: "singleModelEstimates"}
            }

            GroupBox
            {
                title: qsTr("Plots")
                CheckBox { 
                    text: qsTr("Marginal posteriors");    name: "singleModelPosteriorPlot"
                    RadioButtonGroup
                    {
                        name: "groupPosterior"
                        RadioButton { value: "grouped";		text: qsTr("Group levels in single plot"); checked: true}
                        RadioButton { value: "individual";	text: qsTr("Individual plot per level")                 }
                    }
                }
                CheckBox { text: qsTr("Q-Q plot of residuals");  name: "singleModelqqPlot" }
                CheckBox { text: qsTr("Posterior R\u00B2") ;     name: "singleModelrsqPlot"}
            }

        }

        VariablesForm
        {
            height: 200
            AvailableVariablesList { name: "components2"; title: qsTr("Components"); source: ["fixedFactors", "randomFactors"]}
            AssignedVariablesList
            {
                title: qsTr("Specific model terms")
                name: "singleModelTerms"
                listViewType: "Interaction"
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
			name: "sampleModeNumAcc"
			title: qsTr("Numerical accuracy")
			RadioButton { value: "auto";	text: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	text: qsTr("Manual")
				IntegerField
				{
					name: "fixedNumAcc"
					text: qsTr("No. samples")
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
			title: qsTr("Posterior samples")
			RadioButton { value: "auto";	text: qsTr("Auto"); checked: true }
			RadioButton
			{
				value: "manual";	label: qsTr("Manual")
				IntegerField
				{
					name: "fixedMCMCSamples"
					text: qsTr("No. samples")
					defaultValue: 1e3
					fieldWidth: 50
                    min: 100
                    max: 1e7
				}
			}
		}
	}
}
