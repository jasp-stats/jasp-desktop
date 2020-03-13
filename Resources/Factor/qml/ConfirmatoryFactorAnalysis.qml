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
import QtQuick          2.8
import QtQuick.Layouts  1.3
import JASP.Controls    1.0

import "." as SEM

Form
{

	SEM.FactorsForm
	{
		id: factors
		name: "factors"
		initNumberFactors: 1
	}

	Section
	{
		title: qsTr("Second-Order Factor")
		debug: false

		VariablesForm
		{
			id: secondorder
			AvailableVariablesList
			{
				name: "availableFactors"
				source: [{ name: "factors", use: "title" }]
				showVariableTypeIcon: false
				preferredHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
			}
			AssignedVariablesList
			{
				title: qsTr("Second-Order")
				name:  "secondOrder"
				suggestedColumns: []
			}
			preferredHeight: jaspTheme.defaultVariablesFormHeight / 3
		}

		//		SEM.FactorsForm
		//		{
		//            id: secondorder
		//            name: "secondOrder"
		//            implicitHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
		//            allowAll: true
		//			availableVariablesList
		//			{
		//                name: "availableFactors"
		//				source: [{ name: "factors", use: "title" }]
		//                showVariableTypeIcon: false
		//                preferredHeight: jaspTheme.defaultVariablesFormHeight / 3 - 10
		//            }
		//            initNumberFactors: 1
		//        }
	}

	Section
	{
		title: qsTr("Model Options")
		columns: 1

		GroupBox
		{
			title: qsTr("Model Options")
			CheckBox { label: qsTr("Include mean structure")      ; name: "includemeanstructure"   ; id: meanstructure }
			CheckBox { label: qsTr("Assume factors uncorrelated") ; name: "uncorrelatedFactors"    }
			CheckBox { label: qsTr("Fix exogenous covariates")    ; name: "fixExogenousCovariates" ; checked: true ; visible: false }
			ComboBox
			{
				label: qsTr("Factor Scaling")
				name: "identify"
				values: [
					{ label: qsTr("Factor variances"),	value: "factor"  },
					{ label: qsTr("Marker variable"),	value: "marker"  },
					{ label: qsTr("Effects coding"),	value: "effects" }
				]
			}
		}

		GroupBox
		{
			title: qsTr("Residual Covariances")
			VariablesForm
			{
				id: rescov
				preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
				AvailableVariablesList {name: "observedvars";	syncModels: factors.name	}
				AssignedPairsVariablesList { name: "rescov" }
			}
		}
	}

	Section
	{
		title: qsTr("Additional Output")
		GroupBox
		{
			CheckBox { label: qsTr("Additional fit measures")   ; name: "additionalfits"   }
			CheckBox { label: qsTr("R-Squared")                 ; name: "rsquared"         }
		}
		GroupBox
		{
			CheckBox { label: qsTr("Implied covariance matrix")  ; name: "impliedCov" }
			CheckBox { label: qsTr("Residual covariance matrix") ; name: "residCov"   }
			CheckBox {
				label: qsTr("Modification indices")
				name: "modIndices"
				DoubleField {
					label: qsTr("Cutoff")
					name: "miCutoff"
					min: 0
					defaultValue: 3.84
				}
			}
			CheckBox { label: qsTr("Show lavaan syntax")         ; name: "showSyntax" }
		}
	}

	Section
	{
		text: qsTr("Multigroup CFA")
		DropDown
		{
			label: qsTr("Grouping variable") ;
			name: "groupvar";
			showVariableTypeIcon: true;
			addEmptyValue: true;
		} // No model or syncModels: it takes all variables per default
		DropDown
		{
			label: qsTr("Invariance testing")
			name: "invariance"
			model: ListModel {
				ListElement { key: qsTr("Configural") ; value: "configural"  }
				ListElement { key: qsTr("Metric")     ; value: "metric"  }
				ListElement { key: qsTr("Scalar")     ; value: "scalar" }
				ListElement { key: qsTr("Strict")     ; value: "strict" }
			}
		}
	}

	Section
	{
		text: qsTr("Plots")
		GroupBox
		{
			title: qsTr("Plots")
			CheckBox { text: qsTr("Misfit plot")     ; name: "misfitplot" }
			CheckBox
			{
				text: qsTr("Model plot")
				name: "pathplot"
				CheckBox { text: qsTr("Show parameters") ; name: "plotpars"  }
				CheckBox { text: qsTr("Show means")      ; name: "plotmeans"; enabled: meanstructure.checked }
			}
		}
	}

	Section
	{
		text: qsTr("Advanced")
		RadioButtonGroup
		{
			title: qsTr("Emulation")
			name: "mimic"
			RadioButton { text: qsTr("None")  ; name: "lavaan"  ; checked: true }
			RadioButton { text: qsTr("Mplus") ; name: "Mplus" }
			RadioButton { text: qsTr("EQS")   ; name: "EQS"   }
		}

		GroupBox
		{
			title: qsTr("Error calculation")
			CIField { text: qsTr("CI width"); name: "ciWidth" }
			RadioButtonGroup
			{
				title: qsTr("Method")
				name: "se"
				RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
				RadioButton { text: qsTr("Robust")    ; name: "robust" }
				RadioButton {
					text: qsTr("Bootstrap CI")
					name: "bootstrap"
					IntegerField {
						text: qsTr("Bootstrap samples")
						name: "bootstrapNumber"
						defaultValue: 1000
						min: 100
						max: 1000000
					}
				}
			}
		}

		RadioButtonGroup
		{
			title: qsTr("Estimator")
			name: "estimator"
			RadioButton { text: qsTr("Auto") ; name: "default"; checked: true }
			RadioButton { text: qsTr("ML")   ; name: "ML"       }
			RadioButton { text: qsTr("GLS")  ; name: "GLS"      }
			RadioButton { text: qsTr("WLS")  ; name: "WLS"      }
			RadioButton { text: qsTr("ULS")  ; name: "ULS"      }
			RadioButton { text: qsTr("DWLS") ; name: "DWLS"     }
		}

		RadioButtonGroup
		{
			title: qsTr("Standardization")
			name: "std"
			RadioButton { text: qsTr("None")    ; name: "none"; checked: true }
			RadioButton { text: qsTr("Latents") ; name: "lv"  }
			RadioButton { text: qsTr("All")     ; name: "all" }
			RadioButton { text: qsTr("No X")    ; name: "nox" }
		}

		GroupBox
		{
			title: qsTr("Options")
			debug: true
			CheckBox { text: qsTr("Fix manifest intercepts to zero") ; name: "fixManifestInterceptsToZero" }
			CheckBox { text: qsTr("Fix latent intercepts to zero")   ; name: "fixLatentInterceptsToZero"   ; checked: true }
			CheckBox { text: qsTr("Omit residual single indicator")  ; name: "omitResidualSingleIndicator" ; checked: true }
			CheckBox { text: qsTr("Residual variances")              ; name: "residualVariances"           ; checked: true }
			CheckBox { text: qsTr("Correlate exogenous latents")     ; name: "correlateExogenousLatents"   ; checked: true }
			CheckBox { text: qsTr("Add thresholds")                 ; name: "addThresholds"               ; checked: true }
			CheckBox { text: qsTr("Add scalings parameters")         ; name: "addScalingParameters"        ; checked: true }
			CheckBox { text: qsTr("Correlate dependent variables")   ; name: "correlateDependentVariables" ; checked: true }
		}
	}
}


