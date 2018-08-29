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

Form {
    id: form
    
    TextArea {
        name: "model"
        textType: "lavaan"
    }

    ButtonGroup {
        title: qsTr("Data")
        name: "Data"
        RadioButton { name: "raw"; text: qsTr("raw"); checked: true }
        RowLayout {
            RadioButton { name: "varcov"; text: qsTr("Variance-covariance matrix"); id: varcovOption}
            TextField { name: "SampleSize"; label.text: qsTr("Sample Size"); inputType: "integer";
                value: "0"
                validator: IntValidator { bottom: 0 }
                enabled: varcovOption.checked
            }
        }
    }


    ExpanderButton {
        text: qsTr("Statistics")

        GridLayout {
            ButtonGroup {
                title: qsTr("Error Calculation")
                name: "errorCalculation"
                RadioButton { text: qsTr("Standard")   ; name: "standard" ; checked: true    }
                RadioButton { text: qsTr("Robust")     ; name: "robust"       }
                RadioButton { text: qsTr("Bootstrap")  ; name: "bootstrap"; id: boostrapOption  }
                TextField   { label.text: qsTr("Bootstrap samples") ; name: "errorCalculationBootstrapSamples"
                    textWidth: 60
                    text: "1000"
                    validator: IntValidator { bottom: 1 }
                    enabled: boostrapOption.checked
                    Layout.leftMargin: 20
                }
            }
            
            GroupBox {
                CheckBox {  text: qsTr("Additional fit measures")               ; name: "outputAdditionalFitMeasures"           }
                CheckBox {  text: qsTr("Fitted covariances / correlations")     ; name: "outputFittedCovarianceCorrelations"    }
                CheckBox {  text: qsTr("Observed covariances / correlations")   ; name: "outputObservedCovarianceCorrelations"  }
                CheckBox {  text: qsTr("Residual covariances / correlations")   ; name: "outputResidualCovarianceCorrelations"  }
                CheckBox {  text: qsTr("Mardia's coefficient")                  ; name: "outputMardiasCoefficients"             }
                CheckBox {  text: qsTr("Modification indices")                  ; name: "outputModificationIndices"             ; id: outputModificationIndices }
                CheckBox {  text: qsTr("Hide low indices")                      ; name: "outputModificationIndicesHideLowIndices"
                            enabled: outputModificationIndices.checked
                            Layout.leftMargin: 20; id: lowIndices }
                CheckBox {  text: qsTr("Threshold")                             ; name: "outputModificationIndicesHideLowIndicesThreshold"
                            enabled: outputModificationIndices.checked && lowIndices.checked
                            Layout.leftMargin: 40 }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Options")

        GridLayout {
            columns: 3
            GroupBox {
                title: qsTr("Grouping variable")
                ComboBox { name: "groupingVariable"; showVariableTypeIcon: true; addEmptyValue: true} // No model or syncModels: it takes all variables per default
                GroupBox {
                    title: qsTr("Equality Constraits")
                    
                    CheckBox { text: qsTr("Loadings")           ; name: "eq_loadings" }
                    CheckBox { text: qsTr("Intercepts")         ; name: "eq_intercepts" }
                    CheckBox { text: qsTr("Residuals")          ; name: "eq_residuals" }
                    CheckBox { text: qsTr("Residual covariances") ; name: "eq_residualcovariances" }
                    CheckBox { text: qsTr("Means")              ; name: "eq_means" }
                    CheckBox { text: qsTr("Threashold")         ; name: "eq_thresholds" }
                    CheckBox { text: qsTr("Regressions")        ; name: "eq_regressions" }
                    CheckBox { text: qsTr("Latent Variances")   ; name: "eq_variances" }
                    CheckBox { text: qsTr("Latent Covariances") ; name: "eq_lvcovariances" }
                }
            }

            ButtonGroup {
                title: qsTr("Estimator")
                name: "estimator"
                RadioButton { text: qsTr("Auto")    ; name: "automatic"; checked: true }
                RadioButton { text: qsTr("ML")      ; name: "ML" }
                RadioButton { text: qsTr("GLS")     ; name: "GLS" }
                RadioButton { text: qsTr("WLS")     ; name: "WLS" }
                RadioButton { text: qsTr("ULS")     ; name: "ULS" }
                RadioButton { text: qsTr("DWLS")    ; name: "DWLS" }
            }
            
            GroupBox {
                title: qsTr("Model Options")
                CheckBox {  text: qsTr("Include mean structure")        ; name: "includeMeanStructure"          }
                CheckBox {  text: qsTr("Assume factors uncorrelated")   ; name: "assumeFactorsUncorrelated"     }
                CheckBox {  text: qsTr("Fix exogenous covariates")      ; name: "fixExogenousCovariates" ; checked: true }
                
                ComboBox {
                    label.text: qsTr("Factor Scaling")
                    name: "factorStandardisation"
                    model: ListModel {
                        ListElement { key: "Factor Loadings"    ; value: "factorLoadings" }
                        ListElement { key: "Residual Variance"  ; value: "residualVariance" }
                        ListElement { key: "None"               ; value: "none" }
                    }
                }
            }
            
        }
    }
    
    ExpanderButton {
        text: qsTr("Advanced")
        GridLayout {
            GroupBox {
                title: qsTr("Options")
                CheckBox {  text: qsTr("Fix manifest intercepts to zero")   ; name: "fixManifestInterceptsToZero"   }
                CheckBox {  text: qsTr("Fix latent intercepts to zero")     ; name: "fixLatentInterceptsToZero"     ; checked: true }
                CheckBox {  text: qsTr("Omit residual single indicator")    ; name: "omitResidualSingleIndicator"   ; checked: true }
                CheckBox {  text: qsTr("Residual variances")                ; name: "residualVariances"             ; checked: true }
                CheckBox {  text: qsTr("Correlate exogenous latents")       ; name: "correlateExogenousLatents"     ; checked: true }
                CheckBox {  text: qsTr("Add thresholdds")                   ; name: "addThresholds"                 ; checked: true }
                CheckBox {  text: qsTr("Add scalings parameters")           ; name: "addScalingParameters"          ; checked: true }
                CheckBox {  text: qsTr("Correlate dependent variables")     ; name: "correlateDependentVariables"   ; checked: true }
            }
            
            GroupBox {
                ButtonGroup {
                    title: qsTr("Emulation")
                    name: "emulation"
                    RadioButton { text: qsTr("None")    ; name: "none"; checked: true}
                    RadioButton { text: qsTr("Mplus")   ; name: "Mplus" }
                    RadioButton { text: qsTr("EQS")     ; name: "EQS" }
                }
                
                ComboBox {
                    name: "modelName"
                    label.text: qsTr("Model Name")
                    model: ["Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8", "Model 8", "Model 9", "Model 10"]
                }
            }
        }
    }
}
