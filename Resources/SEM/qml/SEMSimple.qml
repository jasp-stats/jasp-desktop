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
	
	TextArea
	{
		name: "model"
		textType: "lavaan"
	}
	
	RadioButtonGroup 
	{
		title: qsTr("Data")
		name: "Data"
		RadioButton { value: "raw"; text: qsTr("raw"); checked: true }
		RowLayout
		{
			RadioButton { value: "varcov"; text: qsTr("Variance-covariance matrix"); id: varcovOption }
			IntegerField { name: "SampleSize"; text: qsTr("Sample Size")
				defaultValue: 0
				enabled: varcovOption.checked
			}
		}
	}
	
	
	ExpanderButton
	{
		title: qsTr("Statistics")
		
		GridLayout
		{
			RadioButtonGroup
			{
				title: qsTr("Error Calculation")
				name: "errorCalculation"
				RadioButton { value: "standard";	text: qsTr("Standard"); checked: true		}
				RadioButton { value: "robust";		text: qsTr("Robust")						}
				RadioButton { value: "bootstrap";	text: qsTr("Bootstrap"); id: boostrapOption	}
				IntegerField
				{
					name: "errorCalculationBootstrapSamples"
					text: qsTr("Bootstrap samples")
					fieldWidth: 60
					defaultValue: 1000
					validator: IntValidator { bottom: 1 }
					enabled: boostrapOption.checked
					indent: true
				}
			}
			
			GroupBox
			{
				CheckBox { name: "outputAdditionalFitMeasures";				text: qsTr("Additional fit measures")				}
				CheckBox { name: "outputFittedCovarianceCorrelations";		text: qsTr("Fitted covariances / correlations")		}
				CheckBox { name: "outputObservedCovarianceCorrelations";	text: qsTr("Observed covariances / correlations")	}
				CheckBox { name: "outputResidualCovarianceCorrelations";	text: qsTr("Residual covariances / correlations")	}
				CheckBox { name: "outputMardiasCoefficients";				text: qsTr("Mardia's coefficient")					}
				CheckBox { name: "outputModificationIndices";				text: qsTr("Modification indices"); id: outputModificationIndices }
				CheckBox { name: "outputModificationIndicesHideLowIndices";	text: qsTr("Hide low indices")
					enabled: outputModificationIndices.checked
					indent: true; id: lowIndices }
				CheckBox { name: "outputModificationIndicesHideLowIndicesThreshold"; text: qsTr("Threshold")                             ; 
					enabled: outputModificationIndices.checked && lowIndices.checked
					indent: true }
			}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Options")
		
		GridLayout
		{
			columns: 3
			GroupBox
			{
				title: qsTr("Grouping variable")
				DropDown { name: "groupingVariable"; showVariableTypeIcon: true; addEmptyValue: true} // No model or syncModels: it takes all variables per default
				GroupBox
				{
					title: qsTr("Equality Constraits")					
					CheckBox { name: "eq_loadings";				text: qsTr("Loadings")				}
					CheckBox { name: "eq_intercepts";			text: qsTr("Intercepts")			}
					CheckBox { name: "eq_residuals";			text: qsTr("Residuals")				}
					CheckBox { name: "eq_residualcovariances";	text: qsTr("Residual covariances")	}
					CheckBox { name: "eq_means";				text: qsTr("Means")					}
					CheckBox { name: "eq_thresholds";			text: qsTr("Threashold")			}
					CheckBox { name: "eq_regressions";			text: qsTr("Regressions")			}
					CheckBox { name: "eq_variances";			text: qsTr("Latent Variances")		}
					CheckBox { name: "eq_lvcovariances";		text: qsTr("Latent Covariances")	}
				}
			}
			
			RadioButtonGroup
			{
				name: "estimator"
				title: qsTr("Estimator")
				RadioButton { value: "automatic";	text: qsTr("Auto"); checked: true	}
				RadioButton { value: "ML";			text: qsTr("ML")					}
				RadioButton { value: "GLS";			text: qsTr("GLS")					}
				RadioButton { value: "WLS";			text: qsTr("WLS")					}
				RadioButton { value: "ULS";			text: qsTr("ULS")					}
				RadioButton { value: "DWLS";		text: qsTr("DWLS")					}
			}
			
			GroupBox
			{
				title: qsTr("Model Options")
				CheckBox { name: "includeMeanStructure";		text: qsTr("Include mean structure")					}
				CheckBox { name: "assumeFactorsUncorrelated";	text: qsTr("Assume factors uncorrelated")				}
				CheckBox { name: "fixExogenousCovariates";		text: qsTr("Fix exogenous covariates"); checked: true	}
				
				DropDown
				{
					name: "factorStandardisation"
					text: qsTr("Factor Scaling")
					model: ListModel
					{
						ListElement { title: "Factor Loadings"    ; value: "factorLoadings"		}
						ListElement { title: "Residual Variance"  ; value: "residualVariance"	}
						ListElement { title: "None"               ; value: "none"				}
					}
				}
			}
			
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Advanced")
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Options")
				CheckBox { name: "fixManifestInterceptsToZero";	text: qsTr("Fix manifest intercepts to zero")					}
				CheckBox { name: "fixLatentInterceptsToZero";	text: qsTr("Fix latent intercepts to zero");	checked: true	}
				CheckBox { name: "omitResidualSingleIndicator";	text: qsTr("Omit residual single indicator");	checked: true	}
				CheckBox { name: "residualVariances";			text: qsTr("Residual variances");				checked: true	}
				CheckBox { name: "correlateExogenousLatents";	text: qsTr("Correlate exogenous latents");		checked: true	}
				CheckBox { name: "addThresholds";				text: qsTr("Add thresholdds");					checked: true	}
				CheckBox { name: "addScalingParameters";		text: qsTr("Add scalings parameters");			checked: true	}
				CheckBox { name: "correlateDependentVariables";	text: qsTr("Correlate dependent variables");	checked: true	}
			}
			
			GroupBox
			{
				RadioButtonGroup
				{
					name: "emulation"
					title: qsTr("Emulation")
					RadioButton { value: "none";	text: qsTr("None");	checked: true	}
					RadioButton { value: "Mplus";	text: qsTr("Mplus")					}
					RadioButton { value: "EQS";		text: qsTr("EQS")					}
				}
				
				DropDown
				{
					name: "modelName"
					text: qsTr("Model Name")
					values: ["Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 8", "Model 9", "Model 10"]
				}
			}
		}
	}
}
