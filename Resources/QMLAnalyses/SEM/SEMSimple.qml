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
    
    VariablesForm {
        id: variablesForm
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
                ComboBox { name: "groupingVariable"; showVariableTypeIcon: true } // No model or syncModels: it takes all variables per default
            }

            GroupBox {
                title: qsTr("Central Tendency")

                CheckBox {  text: qsTr("Mean")      ; name: "mean";  checked: true}
                CheckBox {  text: qsTr("Median")    ; name: "median"              }
                CheckBox {  text: qsTr("Mode")      ; name: "mode"                }
                CheckBox {  text: qsTr("Sum")       ; name: "sum"                 }
            }
            
            GroupBox {
                title: qsTr("Dispersion")
                CheckBox {  text: qsTr("Std.deviation") ; name: "standardDeviation"       }
                CheckBox {  text: qsTr("Minimum")       ; name: "minimum"; checked: true  }
                CheckBox {  text: qsTr("Variance")      ; name: "variance"                }
                CheckBox {  text: qsTr("Maximum")       ; name: "maximum"                 }
                CheckBox {  text: qsTr("Range")         ; name: "range"                   }
                CheckBox {  text: qsTr("S. E. mean")    ; name: "standardErrorMean"       }
            }
            GroupBox {
                title: qsTr("Distribution")
                CheckBox {  text: qsTr("Skewness")      ; name: "skewness"                }
                CheckBox {  text: qsTr("Kurtosis")      ; name: "kurtosis"                }
            }
        }
    }
    
    ExpanderButton {
        text: qsTr("Advanced")
        GridLayout {
            ButtonGroup {
                name: "chartType"
                title: qsTr("Chart Type")
                RadioButton {   text: qsTr("None")          ; name: "_1noCharts"    }
                RadioButton {   text: qsTr("Bar charts")    ; name: "_2barCharts"   }
                RadioButton {   text: qsTr("Pie Charts")    ; name: "_3pieCharts"   }
                RadioButton {   text: qsTr("Histograms")    ; name: "_4histograms"  }                
            }
            
            ButtonGroup {
                name: "chartValues"
                title: qsTr("Chart Values")
                RadioButton {   text: qsTr("Frequencies")   ; name: "_1frequencies" }
                RadioButton {   text: qsTr("Percentages")   ; name: "_2percentages" }                
            }
        }
    }
}
