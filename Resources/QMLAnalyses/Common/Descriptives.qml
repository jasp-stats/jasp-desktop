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
    usesJaspResults: true

    VariablesForm {
        defaultAssignedVariablesList.title: qsTr("Variables")
        AssignedVariablesList {
            name: "splitby"
            title: qsTr("Split")
            singleItem: true
            allowedColumns: ["ordinal", "nominal"]
        }
    }

    CheckBox {text: qsTr("Frequency tables (nominal and ordinal variables)"); name: "frequencyTables"}

    ExpanderButton {
        text: qsTr("Plots")

        GroupBox {
            CheckBox {  text: qsTr("Distribution plots")    ; name: "plotVariables"               }
            CheckBox {  text: qsTr("Correlation plots")     ; name: "plotCorrelationMatrix"       }
            CheckBox {  text: qsTr("Boxplots")              ; name: "splitPlots"; id: splitPlots  }
            
            GroupBox {
                Layout.leftMargin: 20
                enabled: splitPlots.checked
                CheckBox {  text: qsTr("Label Outliers")    ; name: "splitPlotOutlierLabel"       }
                CheckBox {  text: qsTr("Color")             ; name: "splitPlotColour"             }
                CheckBox {  text: qsTr("Boxplot Element")   ; name: "splitPlotBoxplot"; checked: true }
                CheckBox {  text: qsTr("Violin Element")    ; name: "splitPlotViolin"                 }
                CheckBox {  text: qsTr("Jitter Element")    ; name: "splitPlotJitter"                 }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Statistics")

        GridLayout {
            GroupBox {
                title: qsTr("Percentile Values")

                GridLayout {
                    rowSpacing: 3
                    columnSpacing: 1
                    columns: 3
    
                    CheckBox {
                        Layout.columnSpan: 3
                        name: "percentileValuesQuartiles"
                        text: qsTr("Quartiles")
                    }
    
                    CheckBox {
                        id: percentileValuesEqualGroups
                        name: "percentileValuesEqualGroups"
                        text: qsTr("Cut points for: ")
                    }
                    TextField {
                        name: "percentileValuesEqualGroupsNo"
                        inputType: "integer"
                        validator: RegExpValidator {regExp: /[1-9][0-9]?[0]?/}
                        text: "4"
                        enabled: percentileValuesEqualGroups.checked
                    }
                    Label {  text: qsTr(" equal groups") }
                    CheckBox {
                        id: percentileValuesPercentiles
                        text: qsTr("Percentiles:")
                        name: "percentileValuesPercentiles"
                    }
                    TextField {
                        id: percentileValuesPercentilesPercentiles
                        inputType: "integerArray"
                        Layout.columnSpan: 2
                        name: "percentileValuesPercentilesPercentiles"
                        width: 100
                        enabled: percentileValuesPercentiles.checked
                    }
                }
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
        text: qsTr("Charts")
        debug: true
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
