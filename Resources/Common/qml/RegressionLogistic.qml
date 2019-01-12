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
	id: form
	plotWidth: 480
	plotHeight: 320
	
	VariablesForm
	{
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["nominal", "ordinal"]; singleItem: true	}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]									}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal", "ordinal"];itemType: "fixedFactors" }
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleItem: true; debug: true	}
	}
	
	ComboBox
	{
		text: qsTr("Method")
		name: "method"
		model: ListModel
		{
			ListElement { key: "Enter"; value: "enter" }
			ListElement { key: "Backward"; value: "backward" }
			ListElement { key: "Forward"; value: "forward" }
			ListElement { key: "Stepwise"; value: "stepwise" }
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Model")
		
		CheckBox { text: qsTr("Include intercept"); name: "includeIntercept"; checked: true }
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			
			availableVariablesList
			{
				width: parent.width / 4
				name: "availableTerms"
				title: qsTr("Components")
				syncModels: ['covariates', 'factors']
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
		text: qsTr("Statistics")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Descriptives")
				CheckBox { text: qsTr("Factor descriptives"); name: "factorDescriptivesOpt" }
			}
			
			GroupBox
			{
				title: qsTr("Performance Diagnostics")
				CheckBox { text: qsTr("Confusion matrix"); name: "confusionMatrixOpt"; id: confusionMatrixOpt }
				CheckBox { text: qsTr("Proportions"); name: "confusionMatrixProportions"; enabled: confusionMatrixOpt.checked; indent: true }
			}
			
			GroupBox
			{
				title: qsTr("Regression Coefficients")
				CheckBox { text: qsTr("Estimates")                  ; name: "coeffEstimates"; checked: true }
				CheckBox { text: qsTr("Standardized coefficients")  ; name: "stdCoeff" }
				CheckBox { text: qsTr("Odds ratios")                ; name: "oddsRatios" }
				CheckBox { text: qsTr("Confidence intervals")       ; name: "coeffCI" ; id: coeffCI }
				GroupBox
				{
					enabled: coeffCI.checked
					indent: true
					PercentField { text: "Interval"; name: "coeffCIInterval"; defaultValue: 95 }
					CheckBox { text: qsTr("Odds ratio scale")           ; name: "coeffCIOR" }
				}
				CheckBox { text: qsTr("Robust standard errors")     ; name: "robustSEOpt" }
				CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR" }
			}
			
			GroupBox
			{
				title: qsTr("Performance metrics")
				CheckBox { text: qsTr("AUC")                    ; name: "AUC"       }
				CheckBox { text: qsTr("Sensitivity / Recall")   ; name: "Sens"      }
				CheckBox { text: qsTr("Specificity")            ; name: "Spec"      }
				CheckBox { text: qsTr("Precision")              ; name: "Prec"      }
				CheckBox { text: qsTr("F-measure")              ; name: "Fmsr"      }
				CheckBox { text: qsTr("Brier score")            ; name: "BrierScr"  }
				CheckBox { text: qsTr("H-measure")              ; name: "Hmsr"      }
			}
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Plots")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Inferential plots")
				CheckBox { text: qsTr("Display conditional estimates plots"); name: "estimatesPlotsOpt"; id: estimatesPlotsOpt }
				GroupBox
				{
					enabled: estimatesPlotsOpt.checked
					indent: true
					PercentField { text: qsTr("Confidence interval"); name: "estimatesPlotsCI"; defaultValue: 95 }
					CheckBox { text: qsTr("Show data points"); name: "showPoints" }
				}
			}
			
			GroupBox
			{
				title: qsTr("Residual plots")
				CheckBox { text: qsTr("Predicted - residual plot")      ; name: "predictedPlotOpt"      }
				CheckBox { text: qsTr("Predictor - residual plots")     ; name: "predictorPlotsOpt"     }
				CheckBox { text: qsTr("Squared Pearson residuals plot") ; name: "squaredPearsonPlotOpt" }
			}
			
			RadioButtonGroup
			{
				title: qsTr("Residual type")
				name: "residualType"
				RadioButton { text: qsTr("Deviance")   ; name: "deviance" ; checked: true   }
				RadioButton { text: qsTr("Pearson")    ; name: "pearson"                    }
			}
		}
	}
}
