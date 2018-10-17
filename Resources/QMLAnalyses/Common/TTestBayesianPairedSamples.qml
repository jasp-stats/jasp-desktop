import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0

Form {
    id: form

    VariablesForm {
        height: 200
        defaultAssignedVariablesList {
            name: "pairs"
            title: qsTr("Variables")
            allowedColumns: ["scale"]
            listViewType: "AssignedPairs"
        }
    }
    
    GridLayout {
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Tests")
                CheckBox {  text: qsTr("Student")                   ; name: "students"          ; checked: true}
                CheckBox {  text: qsTr("Wilcoxon signed-rank")      ; name: "wilcoxonSignedRank"  }
            }
            
            ButtonGroup {
                title: qsTr("Hypothesis")
                name: "hypothesis"
                RadioButton {   text: qsTr("Measure 1 ≠ Measure 2") ; name: "groupsNotEqual"       ; checked: true}
                RadioButton {   text: qsTr("Measure 1 > Measure 2") ; name: "groupOneGreater"    }
                RadioButton {   text: qsTr("Measure 1 < Measure 2") ; name: "groupTwoGreater"    }
            }
            
            GroupBox {
                title: qsTr("Assumption checks")
                CheckBox {  text: qsTr("Normality")                 ; name: "normalityTests"   }
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
