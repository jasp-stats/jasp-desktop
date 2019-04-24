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
import JASP.Theme		1.0


Form
{
    VariablesForm
    {
        AvailableVariablesList
        {
            name: "availableVariables"
        }
        AssignedVariablesList
        {
            title: "Predictors"
            name:  "predictor"
            allowedColumns: []
        }
        AssignedVariablesList
        {
            title: "Mediators"
            name:  "mediators"
            allowedColumns: []
        }
        AssignedVariablesList
        {
            title: "Outcome"
            name:  "dependent"
            allowedColumns: []
        }
        AssignedVariablesList
        {
            title: "Confounders"
            name:  "confounds"
            allowedColumns: []
        }
    }


	Section
	{
        title: qsTr("Model options")
        ColumnLayout {
            GridLayout
            {
                GroupBox
                {
                    title: qsTr("Model Options")
                    CheckBox { label: qsTr("Include mean structure")      ; name: "includemeanstructure"   ; id: meanstructure }
                    CheckBox { label: qsTr("Fix exogenous covariates")    ; name: "fixExogenousCovariates" ; checked: true ; visible: false }
                }
            }
        }
    }
    
	Section {
        title: qsTr("Additional output")
            GroupBox
            {
                title: qsTr("Additional fit measures")
                CheckBox { label: qsTr("AIC")   ; name: "aic"   }
                CheckBox { label: qsTr("BIC")   ; name: "bic"   }
                CheckBox { label: qsTr("SRMR")  ; name: "srmr"  }
                CheckBox { label: qsTr("TLI")   ; name: "tli"   }
                CheckBox { label: qsTr("CFI")   ; name: "cfi"   }
                CheckBox { label: qsTr("RMSEA") ; name: "rmsea" }
            }
            GroupBox
            {
                CheckBox { label: qsTr("Show lavaan syntax")         ; name: "showSyntax" }
            }

    }

	Section {
        text: qsTr("Plots")
        GroupBox {
            title: "Plots"
            CheckBox { text: qsTr("Model plot")      ; name: "pathplot"   ; id: pathPlot }
            CheckBox { text: qsTr("Show parameters") ; name: "plotpars"   ; enabled: pathPlot.checked ; }
            CheckBox { text: qsTr("Show means")      ; name: "plotmeans"  ; enabled: pathPlot.checked & meanstructure.checked ; }
        }   
    }
    
	Section {
        text: qsTr("Advanced")
        GridLayout {
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
                RadioButton { 
                    text: qsTr("Bootstrap") 
                    name: "bootstrap" 
                    IntegerField {
                        text: qsTr("Bootstrap samples")
                        name: "bootstrapNumber"
                        defaultValue: 1000
                        min: 1
                        max: 1000000
                    }
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

            GroupBox {
                title: qsTr("Options")
                debug: true
                CheckBox { text: qsTr("Fix manifest intercepts to zero") ; name: "fixManifestInterceptsToZero" }
                CheckBox { text: qsTr("Fix latent intercepts to zero")   ; name: "fixLatentInterceptsToZero"   ; checked: true }
                CheckBox { text: qsTr("Omit residual single indicator")  ; name: "omitResidualSingleIndicator" ; checked: true }
                CheckBox { text: qsTr("Residual variances")              ; name: "residualVariances"           ; checked: true }
                CheckBox { text: qsTr("Correlate exogenous latents")     ; name: "correlateExogenousLatents"   ; checked: true }
                CheckBox { text: qsTr("Add thresholdds")                 ; name: "addThresholds"               ; checked: true }
                CheckBox { text: qsTr("Add scalings parameters")         ; name: "addScalingParameters"        ; checked: true }
                CheckBox { text: qsTr("Correlate dependent variables")   ; name: "correlateDependentVariables" ; checked: true }
            }

        }
    }
}


