import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Form {
    id: form

    VariablesForm {
        formHeight: 200
        defaultAssignedVariablesList {
            title: qsTr("Variables")
            allowedColumns: ["scale"]
        }
        AssignedVariablesList {
            name: "groupingVariable"
            title: qsTr("Grouping Variable")
            singleItem: true
            allowedColumns: ["ordinal", "nominal"]
        }

    }
    
    GridLayout {
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Tests")
                CheckBox {  text: qsTr("Student")                   ; name: "students"          ; checked: true}
                CheckBox {  text: qsTr("Welch")                     ; name: "welchs"  }
                CheckBox {  text: qsTr("Mann-Whitney")              ; name: "mannWhitneyU"  }
            }
            
            ButtonGroup {
                title: qsTr("Hypothesis")
                name: "hypothesis"
                RadioButton {   text: qsTr("Group 1 ≠ Group 2") ; name: "groupsNotEqual"       ; checked: true}
                RadioButton {   text: qsTr("Group 1 > Group 2") ; name: "groupOneGreater"    }
                RadioButton {   text: qsTr("Group 1 < Group 2") ; name: "groupTwoGreater"    }
            }
            
            GroupBox {
                title: qsTr("Assumption checks")
                CheckBox {  text: qsTr("Normality")                 ; name: "normalityTests"   }
                CheckBox {  text: qsTr("Equality of variances")     ; name: "equalityOfVariancesTests"   }
            }
        }            
            
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Additional Statistics")
                CheckBox {  text: qsTr("Location parameter")                        ; name: "meanDifference"; id: locationParameter }
                Row {
                    Layout.leftMargin: 15
                    enabled : locationParameter.checked
                    CheckBox {  text: qsTr("Confidence interval")                   ; name: "meanDiffConfidenceIntervalCheckbox"; id: locParConfidenceInterval }
                    PercentField { enabled: locParConfidenceInterval.checked        ; name: "meanDiffConfidenceIntervalPercent"  ; defaultValue: 95 }
                }
                CheckBox {  text: qsTr("Effect Size")                               ; name: "effectSize"; id: effectSize }
                Row {
                    Layout.leftMargin: 15
                    enabled : effectSize.checked
                    CheckBox {  text: qsTr("Confidence interval")                   ; name: "effSizeConfidenceIntervalCheckbox"; id: effectSizeConfidenceInterval }
                    PercentField { enabled: effectSizeConfidenceInterval.checked    ; name: "effSizeConfidenceIntervalPercent" ; defaultValue: 95 }
                }
                CheckBox {  text: qsTr("Descriptives")                              ; name: "descriptives"                        }
                CheckBox {  text: qsTr("Descriptives plots")                        ; name: "descriptivesPlots"; id: descriptivePlots  }
                PercentField { label.text: qsTr("Confidence interval")                   ; name: "descriptivesPlotsConfidenceInterval"; defaultValue: 95; Layout.leftMargin: 20; enabled: descriptivePlots.checked}
                CheckBox {  text: qsTr("Vovk-Sellke mazimum p-ratio")               ; name: "VovkSellkeMPR"                        }
            }
            
            ButtonGroup {
                title: qsTr("Missing Values")
                name: "missingValues"
                RadioButton {   text: qsTr("Exclude cases analysis by analysis")    ; name: "excludeAnalysisByAnalysis" ; checked: true }
                RadioButton {   text: qsTr("Exclude cases listwise")                ; name: "excludeListwise"    }
            }         
        }
    }
    
}
