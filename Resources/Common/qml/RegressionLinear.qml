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
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			allowedColumns: ["scale"]							}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors");				allowedColumns: ["nominal", "ordinal"]; debug: true	}
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); allowedColumns: ["scale"]; singleItem: true		}
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
		
		Group
		{
			title: qsTr("Regression Coefficients")
			columns: 2
			Layout.columnSpan: 2
			Group
			{
				CheckBox
				{
					name: "regressionCoefficientsEstimates"; text: qsTr("Estimates"); checked: true
					CheckBox
					{
						name: "regressionCoefficientsBootstrapping"; text: qsTr("From")
						childrenOnSameRow: true
						IntegerField
						{
							name: "regressionCoefficientsBootstrappingReplicates"
							defaultValue: 5000
							fieldWidth: 50
							min: 100
							afterLabel.text: qsTr("bootstraps")
						}
					}
				}

				CheckBox
				{
					name: "regressionCoefficientsConfidenceIntervals"; text: qsTr("Confidence intervals")
					childrenOnSameRow: true
					PercentField { name: "regressionCoefficientsConfidenceIntervalsInterval"; defaultValue: 95 }
				}
				CheckBox { name: "regressionCoefficientsCovarianceMatrix"; text: qsTr("Covariance matrix") }
			}

			Group
			{
				CheckBox { name: "modelFit";					text: qsTr("Model fit");  checked: true		}
				CheckBox { name: "rSquaredChange";				text: qsTr("R squared change")				}
				CheckBox { name: "descriptives";				text: qsTr("Descriptives")					}
				CheckBox { name: "partAndPartialCorrelations";	text: qsTr("Part and partial correlations")	}
				CheckBox { name: "collinearityDiagnostics";		text: qsTr("Collinearity diagnostics")		}
			}
		}
		
		Group
		{
			title: qsTr("Residuals")
			CheckBox { name: "residualsDurbinWatson";	text: qsTr("Dublin-Watson") }
			CheckBox
			{
				name: "residualsCasewiseDiagnostics";	text: qsTr("Casewise diagnostics")
				RadioButtonGroup
				{
					name: "residualsCasewiseDiagnosticsType"
					RadioButton
					{
						value: "outliersOutside"; text: qsTr("Standard residual >"); checked: true
						childrenOnSameRow: true
						IntegerField { name: "residualsCasewiseDiagnosticsOutliersOutside"; defaultValue: 3	}
					}
					RadioButton
					{
						value: "cooksDistance";	text: qsTr("Cook's distance >")
						childrenOnSameRow: true
						IntegerField { name: "residualsCasewiseDiagnosticsCooksDistance";	defaultValue: 0	}
					}
					RadioButton { value: "allCases"; text: qsTr("All")										}
				}
			}
		}
		
	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		columns: 1
		
		RadioButtonGroup
		{
			name: "steppingMethodCriteriaType"
			title: qsTr("Stepping Method Criteria")
			RadioButton
			{
				value: "usePValue"; text: qsTr("Use p value"); checked: true
				columns: 2
				DoubleField { name: "steppingMethodCriteriaPEntry";		text: qsTr("Entry");	fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaPRemoval";	text: qsTr("Removal");	fieldWidth: 60; defaultValue: 0.1; max: 1; decimals: 3	}
			}
			RadioButton
			{
				value: "useFValue"; text: qsTr("Use F value")
				columns: 2
				DoubleField { name: "steppingMethodCriteriaFEntry";		text: qsTr("Entry");	fieldWidth: 60; defaultValue: 3.84; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaFRemoval";	text: qsTr("Removal");	fieldWidth: 60; defaultValue: 2.71; decimals: 3 }
			}
		}
		
		Group
		{
			CheckBox { name: "includeConstant";	text: qsTr("Include constant in equation"); checked: true }
			CheckBox { name: "VovkSellkeMPR"; text: qsTr("Vovk-Sellke maximum p-ratio") }
		}


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
		
		Group
		{
			title: qsTr("Residuals Plots")
			CheckBox { name: "plotResidualsDependent";	text: qsTr("Residuals vs. dependent")					}
			CheckBox { name: "plotResidualsCovariates";	text: qsTr("Residuals vs. covariates")					}
			CheckBox { name: "plotResidualsPredicted";	text: qsTr("Residuals vs. predicted")					}
			CheckBox
			{
				name: "plotResidualsHistogram";	text: qsTr("Residuals vs. histogram")
				CheckBox { name: "plotResidualsHistogramStandardized";	text: qsTr("Standardized residuals")	}
			}
			CheckBox { name: "plotResidualsQQ";			text: qsTr("Q-Q plot standardized residuals")			}
			CheckBox { name: "plotsPartialRegression";	text: qsTr("Partial plots")								}
		}
	}
}
