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

Form
{
	usesJaspResults: false
	plotWidth: 480
	plotHeight: 320
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["nominal", "ordinal"]; singleVariable: true	}
		DropDown
		{
			name: "method"
			label: qsTr("Method")
			values: [
				{ label: "Enter",		value: "enter"},
				{ label: "Backward",	value: "backward"},
				{ label: "Forward",		value: "forward"},
				{ label: "Stepwise",	value: "stepwise"}
			]
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]									}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal", "ordinal"];itemType: "fixedFactors" }
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleVariable: true; debug: true	}
	}
	
	Section
	{
		title: qsTr("Model")
		
		CheckBox { name: "includeIntercept"; label: qsTr("Include intercept"); checked: true }
		
		VariablesForm
		{
			height: 200
			
			AvailableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				source: ['covariates', 'factors']
				width: parent.width / 4
			}
			AssignedVariablesList
			{
				name: "modelTerms"
				title: qsTr("Model terms")
				width: parent.width * 5 / 9
				listViewType: "Interaction"
				
				ExtraControlColumn
				{
					type: "CheckBox"
					name: "isNuisance"
					title: qsTr("Add to null model")
					purpose: "nuisance"					
				}
			}
		}
		
	}
	
	Section
	{
		title: qsTr("Statistics")
		
		Group
		{
			title: qsTr("Descriptives")
			CheckBox { name: "factorDescriptivesOpt"; label: qsTr("Factor descriptives") }
		}

		Group
		{
			title: qsTr("Performance Diagnostics")
			CheckBox
			{
				name: "confusionMatrixOpt";	label: qsTr("Confusion matrix")
				CheckBox { name: "confusionMatrixProportions";	label: qsTr("Proportions") }
			}
		}

		Group
		{
			title: qsTr("Regression Coefficients")
            CheckBox { name: "coeffEstimates";	label: qsTr("Estimates"); checked: true
                CheckBox
                {
                    name: "coeffEstimatesBootstrapping"; label: qsTr("From")
                    childrenOnSameRow: true
                    IntegerField
                    {
                        name: "coeffEstimatesBootstrappingReplicates"
                        defaultValue: 5000
                        fieldWidth: 50
                        min: 100
                        afterLabel: qsTr("bootstraps")
                    }
                }

            }
			CheckBox { name: "stdCoeff";		label: qsTr("Standardized coefficients")			}
			CheckBox { name: "oddsRatios";		label: qsTr("Odds ratios")						}
			CheckBox
			{
				name: "coeffCI";				label: qsTr("Confidence intervals")
				CIField {	name: "coeffCIInterval"; label: "Interval" }
				CheckBox {		name: "coeffCIOR";		label: qsTr("Odds ratio scale")		}
			}
			CheckBox { name: "robustSEOpt";		label: qsTr("Robust standard errors")		}
			CheckBox { name: "VovkSellkeMPR";	label: qsTr("Vovk-Sellke maximum p-ratio")	}
		}

        Group
        {
            title: qsTr("Performance metrics")
            CheckBox { name: "AUC";			label: qsTr("AUC")					}
            CheckBox { name: "Sens";		label: qsTr("Sensitivity / Recall")	}
            CheckBox { name: "Spec";		label: qsTr("Specificity")			}
            CheckBox { name: "Prec";		label: qsTr("Precision")				}
            CheckBox { name: "Fmsr";		label: qsTr("F-measure")				}
            CheckBox { name: "BrierScr";	label: qsTr("Brier score")			}
            CheckBox { name: "Hmsr";		label: qsTr("H-measure")				}
        }

        Group
        {   title: qsTr("Residuals")
            CheckBox
            {
                name: "casewiseDiagnostics";	label: qsTr("Casewise diagnostics")
                RadioButtonGroup
                {
                    name: "casewiseDiagnosticsType"
                    RadioButton
                    {
                        value: "residualZ"; label: qsTr("Standard residual >"); checked: true
                        childrenOnSameRow: true
                        IntegerField { name: "casewiseDiagnosticsResidualZ"; defaultValue: 3	}
                    }
                    RadioButton
                    {
                        value: "cooksDistance";	label: qsTr("Cook's distance >")
                        childrenOnSameRow: true
                        IntegerField { name: "casewiseDiagnosticsCooksDistance";	defaultValue: 0	}
                    }
                    RadioButton { value: "allCases"; label: qsTr("All")										}
                }
            }
        }

	}
	
	Section
	{
		title: qsTr("Plots")
		
		Group
		{
			title: qsTr("Inferential plots")
			CheckBox
			{
				name: "estimatesPlotsOpt"; label: qsTr("Display conditional estimates plots")
				CIField {	name: "estimatesPlotsCI";	label: qsTr("Confidence interval") }
				CheckBox {		name: "showPoints";			label: qsTr("Show data points")						}
			}
		}

		Group
		{
			title: qsTr("Residual plots")
			CheckBox { name: "predictedPlotOpt";		label: qsTr("Predicted - residual plot")			}
			CheckBox { name: "predictorPlotsOpt";		label: qsTr("Predictor - residual plots")		}
			CheckBox { name: "squaredPearsonPlotOpt";	label: qsTr("Squared Pearson residuals plot")	}
		}

		RadioButtonGroup
		{
			name: "residualType"
			title: qsTr("Residual type")
			RadioButton { value: "deviance";	label: qsTr("Deviance");	checked: true   }
			RadioButton { value: "pearson";		label: qsTr("Pearson")					}
		}
	}
}
