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
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["nominal", "ordinal"]; singleItem: true	}
		DropDown
		{
			name: "method"
			text: qsTr("Method")
			model: ListModel
			{
				ListElement { title: "Enter";		value: "enter"		}
				ListElement { title: "Backward";	value: "backward"	}
				ListElement { title: "Forward";		value: "forward"	}
				ListElement { title: "Stepwise";	value: "stepwise"	}
			}
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]									}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal", "ordinal"];itemType: "fixedFactors" }
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleItem: true; debug: true	}
	}
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		CheckBox { name: "includeIntercept"; text: qsTr("Include intercept"); checked: true }
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList
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
			CheckBox { name: "factorDescriptivesOpt"; text: qsTr("Factor descriptives") }
		}

		Group
		{
			title: qsTr("Performance Diagnostics")
			CheckBox
			{
				name: "confusionMatrixOpt";	text: qsTr("Confusion matrix")
				CheckBox { name: "confusionMatrixProportions";	text: qsTr("Proportions") }
			}
		}

		Group
		{
			title: qsTr("Regression Coefficients")
			CheckBox { name: "coeffEstimates";	text: qsTr("Estimates"); checked: true			}
			CheckBox { name: "stdCoeff";		text: qsTr("Standardized coefficients")			}
			CheckBox { name: "oddsRatios";		text: qsTr("Odds ratios")						}
			CheckBox
			{
				name: "coeffCI";				text: qsTr("Confidence intervals")
				PercentField {	name: "coeffCIInterval"; text: "Interval"; defaultValue: 95	}
				CheckBox {		name: "coeffCIOR";		text: qsTr("Odds ratio scale")		}
			}
			CheckBox { name: "robustSEOpt";		text: qsTr("Robust standard errors")		}
			CheckBox { name: "VovkSellkeMPR";	text: qsTr("Vovk-Sellke maximum p-ratio")	}
		}

		Group
		{
			title: qsTr("Performance metrics")
			CheckBox { name: "AUC";			text: qsTr("AUC")					}
			CheckBox { name: "Sens";		text: qsTr("Sensitivity / Recall")	}
			CheckBox { name: "Spec";		text: qsTr("Specificity")			}
			CheckBox { name: "Prec";		text: qsTr("Precision")				}
			CheckBox { name: "Fmsr";		text: qsTr("F-measure")				}
			CheckBox { name: "BrierScr";	text: qsTr("Brier score")			}
			CheckBox { name: "Hmsr";		text: qsTr("H-measure")				}
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
				name: "estimatesPlotsOpt"; text: qsTr("Display conditional estimates plots")
				PercentField {	name: "estimatesPlotsCI";	text: qsTr("Confidence interval"); defaultValue: 95 }
				CheckBox {		name: "showPoints";			text: qsTr("Show data points")						}
			}
		}

		Group
		{
			title: qsTr("Residual plots")
			CheckBox { name: "predictedPlotOpt";		text: qsTr("Predicted - residual plot")			}
			CheckBox { name: "predictorPlotsOpt";		text: qsTr("Predictor - residual plots")		}
			CheckBox { name: "squaredPearsonPlotOpt";	text: qsTr("Squared Pearson residuals plot")	}
		}

		RadioButtonGroup
		{
			name: "residualType"
			title: qsTr("Residual type")
			RadioButton { value: "deviance";	text: qsTr("Deviance");	checked: true   }
			RadioButton { value: "pearson";		text: qsTr("Pearson")					}
		}
	}
}
