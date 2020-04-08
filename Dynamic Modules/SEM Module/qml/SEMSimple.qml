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
	columns: 1
	
	TextArea
	{
		name: "model"
		textType: "lavaan" 
	}
	
	RadioButtonGroup 
	{
		title: qsTr("Data")
		name: "Data"
		columns: 2
		RadioButton { value: "raw"; label: qsTr("raw"); checked: true }
		RadioButton
		{
			value: "varcov"; label: qsTr("Variance-covariance matrix")
			IntegerField { name: "SampleSize"; label: qsTr("Sample size"); defaultValue: 0 }
		}
	}
	
	
	Section
	{
		title: qsTr("Statistics")
		
		RadioButtonGroup
		{
			title: qsTr("Error Calculation")
			name: "errorCalculation"
			RadioButton { value: "standard";	label: qsTr("Standard"); checked: true		}
			RadioButton { value: "robust";		label: qsTr("Robust")						}
			RadioButton
			{
				value: "bootstrap";	label: qsTr("Bootstrap")
				IntegerField
				{
					name: "errorCalculationBootstrapSamples"
					label: qsTr("Bootstrap samples")
					fieldWidth: 60
					defaultValue: 1000
					min: 1
				}
			}
		}

		Group
		{
			CheckBox { name: "outputAdditionalFitMeasures";				label: qsTr("Additional fit measures")				}
			CheckBox { name: "outputFittedCovarianceCorrelations";		label: qsTr("Fitted covariances / correlations")	}
			CheckBox { name: "outputObservedCovarianceCorrelations";	label: qsTr("Observed covariances / correlations")	}
			CheckBox { name: "outputResidualCovarianceCorrelations";	label: qsTr("Residual covariances / correlations")	}
			CheckBox { name: "outputMardiasCoefficients";				label: qsTr("Mardia's coefficient")					}
			CheckBox
			{
				name: "outputModificationIndices";						label: qsTr("Modification indices")
				CheckBox
				{
					name: "outputModificationIndicesHideLowIndices";	label: qsTr("Hide low indices")
					IntegerField { name: "outputModificationIndicesHideLowIndicesThreshold"; label: qsTr("Threshold"); defaultValue: 10 }
				}
			}
		}
	}
	
	Section
	{
		title: qsTr("Options")
		columns: 3

		Group
		{
			title: qsTr("Grouping Variable")
			DropDown { name: "groupingVariable"; showVariableTypeIcon: true; addEmptyValue: true} // No model or source: it takes all variables per default
			Group
			{
				title: qsTr("Equality Constraints")
				CheckBox { name: "eq_loadings";				label: qsTr("Loadings")				}
				CheckBox { name: "eq_intercepts";			label: qsTr("Intercepts")			}
				CheckBox { name: "eq_residuals";			label: qsTr("Residuals")			}
				CheckBox { name: "eq_residualcovariances";	label: qsTr("Residual covariances")	}
				CheckBox { name: "eq_means";				label: qsTr("Means")				}
				CheckBox { name: "eq_thresholds";			label: qsTr("Thresholds")			}
				CheckBox { name: "eq_regressions";			label: qsTr("Regressions")			}
				CheckBox { name: "eq_variances";			label: qsTr("Latent variances")		}
				CheckBox { name: "eq_lvcovariances";		label: qsTr("Latent covariances")	}
			}
		}

		RadioButtonGroup
		{
			name: "estimator"
			title: qsTr("Estimator")
			RadioButton { value: "automatic";	label: qsTr("Auto"); checked: true	}
			RadioButton { value: "ML";			label: qsTr("ML")					}
			RadioButton { value: "GLS";			label: qsTr("GLS")					}
			RadioButton { value: "WLS";			label: qsTr("WLS")					}
			RadioButton { value: "ULS";			label: qsTr("ULS")					}
			RadioButton { value: "DWLS";		label: qsTr("DWLS")					}
		}

		Group
		{
			title: qsTr("Model Options")
			CheckBox { name: "includeMeanStructure";		label: qsTr("Include mean structure")					}
			CheckBox { name: "assumeFactorsUncorrelated";	label: qsTr("Assume factors uncorrelated")				}
			CheckBox { name: "fixExogenousCovariates";		label: qsTr("Fix exogenous covariates"); checked: true	}

			DropDown
			{
				name: "factorStandardisation"
				label: qsTr("Factor scaling")
				values:
				[
					{ label: qsTr("Factor loadings")    , value: "factorLoadings"	},
					{ label: qsTr("Residual variance")  , value: "residualVariance"	},
					{ label: qsTr("None")               , value: "none"				}
				]
			}
		}
	}
	
	Section
	{
		title: qsTr("Advanced")
		Group
		{
			title: qsTr("Options")
			columns: 2
			Layout.columnSpan: 2
			CheckBox { name: "fixManifestInterceptsToZero";	label: qsTr("Fix manifest intercepts to zero")					}
			CheckBox { name: "fixLatentInterceptsToZero";	label: qsTr("Fix latent intercepts to zero");	checked: true	}
			CheckBox { name: "omitResidualSingleIndicator";	label: qsTr("Omit residual single indicator");	checked: true	}
			CheckBox { name: "residualVariances";			label: qsTr("Residual variances");				checked: true	}
			CheckBox { name: "correlateExogenousLatents";	label: qsTr("Correlate exogenous latents");		checked: true	}
			CheckBox { name: "addThresholds";				label: qsTr("Add thresholds");					checked: true	}
			CheckBox { name: "addScalingParameters";		label: qsTr("Add scalings parameters");			checked: true	}
			CheckBox { name: "correlateDependentVariables";	label: qsTr("Correlate dependent variables");	checked: true	}
		}

		RadioButtonGroup
		{
			name: "emulation"
			title: qsTr("Emulation")
			RadioButton { value: "none";	label: qsTr("None"); checked: true	}
			RadioButton { value: "Mplus";	label: qsTr("Mplus")				}
			RadioButton { value: "EQS";		label: qsTr("EQS")					}
		}

		Group
		{
			title: qsTr("Model Name")
			DropDown
			{
				name: "modelName"
				values: ["Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 8", "Model 9", "Model 10"]
			}
		}
	}
}
