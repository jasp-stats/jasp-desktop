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
import "." as SEM

Form
{
        
	SEM.FactorsForm
	{
        id: factors
		name: "factors"
		initNumberFactors: 1
    }        
    
	ExpanderButton
	{
		text: qsTr("Second-order factors")
		VariablesForm
		{
            id: secondorder
			availableVariablesList
			{
                name: "availableFactors"
				source: ["factors"]
                showVariableTypeIcon: false
            }
            AssignedVariablesList {
                title: qsTr("Second-order factors")
                name: "SecondOrder"
                showVariableTypeIcon: false
            }
        }
    }

	ExpanderButton
	{
        text: qsTr("Model options")
		GridLayout
		{
			GroupBox
			{
                title: qsTr("Additional fit measures")
                CheckBox { text: qsTr("AIC")   ; name: "aic"   }
                CheckBox { text: qsTr("BIC")   ; name: "bic"   }
                CheckBox { text: qsTr("SRMR")  ; name: "srmr"  }
                CheckBox { text: qsTr("TLI")   ; name: "tli"   }
                CheckBox { text: qsTr("CFI")   ; name: "cfi"   }
                CheckBox { text: qsTr("RMSEA") ; name: "rmsea" }
            }
			GroupBox
			{
                title: qsTr("Model Options")
                CheckBox { text: qsTr("Include mean structure")      ; name: "includemeanstructure"   ; id: meanstructure }
                CheckBox { text: qsTr("Assume factors uncorrelated") ; name: "uncorrelatedFactors"    }
                CheckBox { text: qsTr("Fix exogenous covariates")    ; name: "fixExogenousCovariates" ; checked: true }
				ComboBox
				{
                    label.text: qsTr("Factor Scaling")
                    name: "identify"
                    model: ListModel {
                        ListElement { key: "Factor loadings" ; value: "factor"  }
                        ListElement { key: "Marker variable" ; value: "marker"  }
                        ListElement { key: "Effects coding"  ; value: "effects" }
                    }
                }
            }
        }
        
        VariablesForm {
            id: rescov
            availableVariablesList {
				name: "observedvars"
				syncModels: factors.name
            }
            AssignedVariablesList {
                title: qsTr("Residual Covariances")
                name: "rescov"
                listViewType: "AssignedPairs"
            }
            height: 150
        }
    }
    
    ExpanderButton {
        text: qsTr("Additional output")
        CheckBox { text: qsTr("Implied covariance matrix")  ; name: "impliedCov" }
        CheckBox { text: qsTr("Residual covariance matrix") ; name: "residCov"   }
        CheckBox { text: qsTr("Modification indices") ; name: "modIndices" ; id: modIndices }
        TextField {
            text: qsTr("Cutoff")
            name: "miCutoff"
            fieldWidth: 60
            value: "3.84"
            validator: IntValidator { bottom: 0 }
            Layout.leftMargin: 20
            enabled: modIndices.checked
        }
        CheckBox { text: qsTr("Show lavaan syntax")         ; name: "showSyntax" }
    }

    ExpanderButton {
        text: qsTr("Multigroup CFA")
        ComboBox { 
            label.text: qsTr("Grouping variable") ; 
            name: "groupvar"; 
            showVariableTypeIcon: true; 
            addEmptyValue: true 
        } // No model or syncModels: it takes all variables per default
        ComboBox {
            label.text: qsTr("Invariance testing")
            name: "invariance"
            model: ListModel {
                ListElement { key: qsTr("Configural") ; value: "configural"  }
                ListElement { key: qsTr("Metric")     ; value: "metric"  }
                ListElement { key: qsTr("Scalar")     ; value: "scalar" }
                ListElement { key: qsTr("Strict")     ; value: "strict" }
            }
        }
        
    }

    ExpanderButton {
        text: qsTr("Plots")
        GroupBox {
            title: "Plots"
            CheckBox { text: qsTr("Misfit plot")     ; name: "misfitplot" }
            CheckBox { text: qsTr("Model plot")      ; name: "pathplot"   ; id: pathPlot }
            CheckBox { text: qsTr("Show parameters") ; name: "plotpars"   ; enabled: pathPlot.checked ; Layout.leftMargin: 20 }
            CheckBox { text: qsTr("Show means")      ; name: "plotmeans"  ; enabled: pathPlot.checked & meanstructure.checked ; Layout.leftMargin: 20 }
        }   
    }
    
    ExpanderButton {
        text: qsTr("Advanced")
        GridLayout {
            GroupBox {
                title: qsTr("Options")
                CheckBox { text: qsTr("Fix manifest intercepts to zero") ; name: "fixManifestInterceptsToZero" }
                CheckBox { text: qsTr("Fix latent intercepts to zero")   ; name: "fixLatentInterceptsToZero"   ; checked: true }
                CheckBox { text: qsTr("Omit residual single indicator")  ; name: "omitResidualSingleIndicator" ; checked: true }
                CheckBox { text: qsTr("Residual variances")              ; name: "residualVariances"           ; checked: true }
                CheckBox { text: qsTr("Correlate exogenous latents")     ; name: "correlateExogenousLatents"   ; checked: true }
                CheckBox { text: qsTr("Add thresholdds")                 ; name: "addThresholds"               ; checked: true }
                CheckBox { text: qsTr("Add scalings parameters")         ; name: "addScalingParameters"        ; checked: true }
                CheckBox { text: qsTr("Correlate dependent variables")   ; name: "correlateDependentVariables" ; checked: true }
            }

            GroupBox {
                Layout.fillWidth: true
                RadioButtonGroup {
                    title: qsTr("Emulation")
                    name: "mimic"
                    RadioButton { text: qsTr("None")  ; name: "lavaan"  ; checked: true }
                    RadioButton { text: qsTr("Mplus") ; name: "Mplus" }
                    RadioButton { text: qsTr("EQS")   ; name: "EQS"   }
                }
            }

            RadioButtonGroup {
                title: qsTr("Error Calculation")
                name: "se"
                RadioButton { text: qsTr("Standard")  ; name: "standard" ; checked: true }
                RadioButton { text: qsTr("Robust")    ; name: "robust" }
                RadioButton { text: qsTr("Bootstrap") ; name: "bootstrap"; id: bootstrapOption }
                TextField   {
                    text: qsTr("Bootstrap samples")
                    name: "bootstrapNumber"
                    fieldWidth: 60
                    value: "1000"
                    validator: IntValidator { bottom: 1 }
                    enabled: bootstrapOption.checked
                    Layout.leftMargin: 20
                }
            }

            RadioButtonGroup {
                title: qsTr("Estimator")
                name: "estimator"
                RadioButton { text: qsTr("Auto") ; name: "default"; checked: true }
                RadioButton { text: qsTr("ML")   ; name: "ML"       }
                RadioButton { text: qsTr("GLS")  ; name: "GLS"      }
                RadioButton { text: qsTr("WLS")  ; name: "WLS"      }
                RadioButton { text: qsTr("ULS")  ; name: "ULS"      }
                RadioButton { text: qsTr("DWLS") ; name: "DWLS"     }
            }

            RadioButtonGroup {
                title: qsTr("Standardization")
                name: "std"
                RadioButton { text: qsTr("None")    ; name: "none"; checked: true }
                RadioButton { text: qsTr("Latents") ; name: "lv"  }
                RadioButton { text: qsTr("All")     ; name: "all" }
                RadioButton { text: qsTr("No X")    ; name: "nox" }
            }
        }
    }
}


