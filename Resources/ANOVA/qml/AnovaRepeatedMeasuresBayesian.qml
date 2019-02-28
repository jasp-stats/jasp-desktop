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
	usesJaspResults: true

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
        CheckBox { name: "posteriorEstimates"; text: qsTr("Estimates") }      
        CheckBox { name: "descriptives"; text: qsTr("Descriptives") }       
    }
    RadioButtonGroup
    {
        title: qsTr("Order")
        name: "bayesFactorOrder"
        RadioButton { value: "nullModelTop";	text: qsTr("Compare to null model"); checked: true	}
        RadioButton { value: "bestModelTop";	text: qsTr("Compare to best model")					}
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
	
    ExpanderButton
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
                CheckBox { text: qsTr("Marginal posteriors");    name: "singleModelPosteriorPlot"}
                CheckBox { text: qsTr("Q-Q plot of residuals");  name: "singleModelqqPlot" }
                CheckBox { text: qsTr("Posterior R\u00B2") ;     name: "singleModelrsqPlot"}
            }

<<<<<<< HEAD
	Section
=======
        }

        VariablesForm
        {
            height: 200
            AvailableVariablesList { name: "components2"; title: qsTr("Components"); source: ["repeatedMeasuresFactors", "betweenSubjectFactors", "covariates"]; width: parent.width / 4 }
            AssignedVariablesList
            {
                title: qsTr("Specific model terms")
                name: "singleModelTerms"
                listViewType: "Interaction"
            }
        }
    }
	
	ExpanderButton
>>>>>>> started on jaspResults
	{
		title: qsTr("Post Hoc Tests")

		VariablesForm
		{
			height: 200
            AvailableVariablesList { name: "postHocTestsAvailable"; source: ["repeatedMeasuresFactors", "betweenSubjectFactors"] }
			AssignedVariablesList {  name: "postHocTestsVariables"; width: parent.width / 4;}
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
                    intValidator { bottom: 100; top: 1e7 }
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
				value: "manual";	text: qsTr("Manual")
				IntegerField
				{
					name: "fixedMCMCSamples"
					text: qsTr("No. samples")
					defaultValue: 1e3
					fieldWidth: 50
                    intValidator { bottom: 100; top: 1e7 }
				}
			}
		}
	}
}
