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
	
	VariablesForm
	{
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "dependent";	title: qsTr("Dependent Variable");	suggestedColumns: ["scale"]; singleVariable: true;		}
		DropDown
		{
			name: "method"
			label: qsTr("Method")
			values: [
				{ label: qsTr("Enter"),		value: "enter"},
				{ label: qsTr("Backward"),	value: "backward"},
				{ label: qsTr("Forward"),	value: "forward"},
				{ label: qsTr("Stepwise"),	value: "stepwise"}
			]			
		}
		AssignedVariablesList { name: "covariates";	title: qsTr("Covariates");			suggestedColumns: ["scale"]								}
        AssignedVariablesList { name: "factors";	title: qsTr("Factors");				suggestedColumns: ["ordinal", "nominal"]; debug: true	}
		AssignedVariablesList { name: "wlsWeights";	title: qsTr("WLS Weights (optional)"); suggestedColumns: ["scale"]; singleVariable: true	}
	}
	
	
	Section
	{
		title: qsTr("Model")
		
		CheckBox { name: "includeConstant"; label: qsTr("Include intercept"); checked: true }
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList
			{
				name: "availableTerms"
				title: qsTr("Components")
				width: parent.width / 4
				source: ['covariates', 'factors']
			}
			ModelTermsList { width: parent.width * 5 / 9 }
		}
		
	}
	
	Section
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
					name: "regressionCoefficientsEstimates"
					label: qsTr("Estimates")
					checked: true
					onClicked: { if (!checked && bootstrapping.checked) bootstrapping.click() }
					CheckBox
					{
						id: bootstrapping
						name: "regressionCoefficientsBootstrapping"
						label: qsTr("From")
						childrenOnSameRow: true
						IntegerField
						{
							name: "regressionCoefficientsBootstrappingReplicates"
							defaultValue: 5000
							fieldWidth: 50
							min: 100
							afterLabel: qsTr("bootstraps")
						}
					}
				}

				CheckBox
				{
					name: "regressionCoefficientsConfidenceIntervals"; label: qsTr("Confidence intervals")
					childrenOnSameRow: true
					CIField { name: "regressionCoefficientsConfidenceIntervalsInterval" }
				}
				CheckBox { name: "regressionCoefficientsCovarianceMatrix"; label: qsTr("Covariance matrix") }
				CheckBox { name: "VovkSellkeMPR"; label: qsTr("Vovk-Sellke maximum p-ratio") }
			}

			Group
			{
				CheckBox { name: "modelFit";					label: qsTr("Model fit");  checked: true		}
				CheckBox { name: "rSquaredChange";				label: qsTr("R squared change")				}
				CheckBox { name: "descriptives";				label: qsTr("Descriptives")					}
				CheckBox { name: "partAndPartialCorrelations";	label: qsTr("Part and partial correlations")	}
				CheckBox { name: "collinearityDiagnostics";		label: qsTr("Collinearity diagnostics")		}
			}
		}
		
		Group
		{
			title: qsTr("Residuals")
            CheckBox { name: "residualsStatistics";     label: qsTr("Statistics")    }
            CheckBox { name: "residualsDurbinWatson";	label: qsTr("Durbin-Watson") }
			CheckBox
			{
				name: "residualsCasewiseDiagnostics";	label: qsTr("Casewise diagnostics")
				RadioButtonGroup
				{
					name: "residualsCasewiseDiagnosticsType"
					RadioButton
					{
						value: "outliersOutside"; label: qsTr("Standard residual >"); checked: true
						childrenOnSameRow: true
                        DoubleField { name: "residualsCasewiseDiagnosticsOutliersOutside"; defaultValue: 3	}
					}
					RadioButton
					{
						value: "cooksDistance";	label: qsTr("Cook's distance >")
						childrenOnSameRow: true
                        DoubleField { name: "residualsCasewiseDiagnosticsCooksDistance";	defaultValue: 1	}
					}
					RadioButton { value: "allCases"; label: qsTr("All")										}
				}
			}
		}
		
	}
	
	Section
	{
		title: qsTr("Method Specification")
		columns: 1
		
		RadioButtonGroup
		{
			name: "steppingMethodCriteriaType"
			title: qsTr("Stepping Method Criteria")
			RadioButton
			{
				value: "usePValue"; label: qsTr("Use p value"); checked: true
				columns: 2
				DoubleField { name: "steppingMethodCriteriaPEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 0.05; max: 1; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaPRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 0.1; max: 1; decimals: 3	}
			}
			RadioButton
			{
				value: "useFValue"; label: qsTr("Use F value")
				columns: 2
				DoubleField { name: "steppingMethodCriteriaFEntry";		label: qsTr("Entry");	fieldWidth: 60; defaultValue: 3.84; decimals: 3 }
				DoubleField { name: "steppingMethodCriteriaFRemoval";	label: qsTr("Removal");	fieldWidth: 60; defaultValue: 2.71; decimals: 3 }
			}
		}
		
		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			debug: true
			RadioButton { value: "excludeCasesListwise"; label: qsTr("Exclude cases listwise"); checked: true	}
			RadioButton { value: "excludeCasesPairwise"; label: qsTr("Exclude cases pairwise")					}
		}
	}
	
	Section
	{
		title: qsTr("Plots")
		
		Group
		{
			title: qsTr("Residuals Plots")
			CheckBox { name: "plotResidualsDependent";	label: qsTr("Residuals vs. dependent")					}
			CheckBox { name: "plotResidualsCovariates";	label: qsTr("Residuals vs. covariates")					}
			CheckBox { name: "plotResidualsPredicted";	label: qsTr("Residuals vs. predicted")					}
			CheckBox
			{
				name: "plotResidualsHistogram";	label: qsTr("Residuals vs. histogram")
                CheckBox { name: "plotResidualsHistogramStandardized";	label: qsTr("Standardized residuals"); checked: true	}
			}
			CheckBox { name: "plotResidualsQQ";			label: qsTr("Q-Q plot standardized residuals")			}
            CheckBox
            {
                name: "plotsPartialRegression";	label: qsTr("Partial plots")
                CheckBox
                {
                    name: "plotsPartialConfidenceIntervals";   label: qsTr("Confidence intervals")
                    childrenOnSameRow: true
                    CIField { name: "plotsPartialConfidenceIntervalsInterval"; }
                }
                CheckBox
                {
                    name: "plotsPartialPredictionIntervals";   label: qsTr("Prediction intervals")
                    childrenOnSameRow: true
                    CIField { name: "plotsPartialPredictionIntervalsInterval"; }
                }
            }
		}
	}
}
