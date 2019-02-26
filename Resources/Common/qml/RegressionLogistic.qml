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
	
	ExpanderButton
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
				}
			}
		}
		
	}
	
	ExpanderButton
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
			CheckBox { name: "coeffEstimates";	label: qsTr("Estimates"); checked: true			}
			CheckBox { name: "stdCoeff";		label: qsTr("Standardized coefficients")			}
			CheckBox { name: "oddsRatios";		label: qsTr("Odds ratios")						}
			CheckBox
			{
				name: "coeffCI";				label: qsTr("Confidence intervals")
				PercentField {	name: "coeffCIInterval"; label: "Interval"; defaultValue: 95	}
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
	}
	
	ExpanderButton
	{
		title: qsTr("Plots")
		
		Group
		{
			title: qsTr("Inferential plots")
			CheckBox
			{
				name: "estimatesPlotsOpt"; label: qsTr("Display conditional estimates plots")
				PercentField {	name: "estimatesPlotsCI";	label: qsTr("Confidence interval"); defaultValue: 95 }
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
