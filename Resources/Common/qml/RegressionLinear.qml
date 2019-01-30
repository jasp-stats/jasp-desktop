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
import JASP.Theme 1.0

Form
{
	usesJaspResults: false
	
	VariablesForm
	{
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	allowedColumns: ["scale"]; singleItem: true;		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]							}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal", "ordinal"]; debug: true	}
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleItem: true		}
	}
	
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
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			availableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates', 'factors']
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
		
		GroupBox
		{
			title: qsTr("Regression Coefficients")
			
			GridLayout
			{
				GroupBox
				{
					CheckBox { name: "regressionCoefficientsEstimates"; text: qsTr("Estimates"); checked: true; id: regressionCoefficientsEstimates }
					RowLayout
					{
						enabled: regressionCoefficientsEstimates.checked
						Layout.leftMargin: Theme.indentationLength
						CheckBox { name: "regressionCoefficientsBootstrapping"; text: qsTr("From") }
						IntegerField
						{
							name: "regressionCoefficientsBootstrappingReplicates"
							defaultValue: 5000
							fieldWidth: 50
							intValidator.bottom: 100
							afterLabel.text: qsTr("bootstraps")
						}
					}
					
					RowLayout
					{
						CheckBox { name: "regressionCoefficientsConfidenceIntervals"; text: qsTr("Confidence intervals") }
						PercentField { name: "regressionCoefficientsConfidenceIntervalsInterval"; defaultValue: 95 }
					}
					CheckBox { name: "regressionCoefficientsCovarianceMatrix"; text: qsTr("Covariance matrix") }
				}
				
				GroupBox
				{
					CheckBox { name: "modelFit";					text: qsTr("Model fit");  checked: true		}
					CheckBox { name: "rSquaredChange";				text: qsTr("R squared change")				}
					CheckBox { name: "descriptives";				text: qsTr("Descriptives")					}
					CheckBox { name: "partAndPartialCorrelations";	text: qsTr("Part and partial correlations")	}
					CheckBox { name: "collinearityDiagnostics";		text: qsTr("Collinearity diagnostics")		}
				}
			}
		}
		
		GroupBox
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsDurbinWatson";			text: qsTr("Dublin-Watson")												}
			CheckBox { name: "residualsCasewiseDiagnostics";	text: qsTr("Casewise diagnostics"); id: residualsCasewiseDiagnostics	}
			RadioButtonGroup
			{
				name: "residualsCasewiseDiagnosticsType"
				enabled: residualsCasewiseDiagnostics.checked
				indent: true
				RowLayout
				{
					RadioButton { value: "outliersOutside";	text: qsTr("Standard residual >");  checked: true	}
					IntegerField { name: "residualsCasewiseDiagnosticsOutliersOutside"; defaultValue: 3			}
				}
				RowLayout
				{
					RadioButton { value: "cooksDistance";	text: qsTr("Cook's distance >")						}
					IntegerField { name: "residualsCasewiseDiagnosticsCooksDistance";	defaultValue: 0			}
				}
				RadioButton { value: "allCases"; text: qsTr("All")												}
			}
		}
		
	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		
		CheckBox { name: "VovkSellkeMPR"; text: qsTr("Vovk-Sellke maximum p-ratio") }
		
		RadioButtonGroup
		{
			name: "steppingMethodCriteriaType"
			title: qsTr("Stepping Method Criteria")
			RadioButton { value: "usePValue"; text: qsTr("Use p value"); checked: true; id: usePValue	}
			RowLayout
			{
				enabled: usePValue.checked
				Layout.leftMargin: Theme.indentationLength
				DoubleField { name: "steppingMethodCriteriaPEntry";		text: qsTr("Entry");	defaultValue: 0.05; doubleValidator { top: 1; decimals: 3} }
				DoubleField { name: "steppingMethodCriteriaPRemoval";	text: qsTr("Removal");	defaultValue: 0.1; doubleValidator { top: 1; decimals: 3} }
			}
			RadioButton { value: "useFValue"; text: qsTr("Use F value"); id: useFValue					}
			RowLayout
			{
				enabled: useFValue.checked
				Layout.leftMargin: Theme.indentationLength
				DoubleField { name: "steppingMethodCriteriaFEntry";		text: qsTr("Entry");	defaultValue: 3.84; doubleValidator.decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaFRemoval";	text: qsTr("Removal");	defaultValue: 2.71; doubleValidator.decimals: 3 }
			}
		}
		
		CheckBox { name: "includeConstant";	text: qsTr("Include constant in equation"); checked: true }
		
		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			debug: true
			RadioButton { value: "excludeCasesListwise"; text: qsTr("Exclude cases listwise"); checked: true	}
			RadioButton { value: "excludeCasesPairwise"; text: qsTr("Exclude cases pairwise")					}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Plots")
		
		GroupBox
		{
			title: qsTr("Residuals Plots")
			CheckBox { name: "plotResidualsDependent";	text: qsTr("Residuals vs. dependent")								}
			CheckBox { name: "plotResidualsCovariates";	text: qsTr("Residuals vs. covariates")								}
			CheckBox { name: "plotResidualsPredicted";	text: qsTr("Residuals vs. predicted")								}
			CheckBox { name: "plotResidualsHistogram";	text: qsTr("Residuals vs. histogram"); id: plotResidualsHistogram	}
			CheckBox { name: "plotResidualsHistogramStandardized";	text: qsTr("Standardized residuals"); enabled: plotResidualsHistogram.checked; indent: true }
			CheckBox { name: "plotResidualsQQ";			text: qsTr("Q-Q plot standardized residuals")						}
			CheckBox { name: "plotsPartialRegression";	text: qsTr("Partial plots")											}
		}
	}
}
