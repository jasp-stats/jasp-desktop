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
        height: 200
        defaultAssignedVariablesList {
            title: qsTr("Variables")
            allowedColumns: ["scale"]
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Tests")
                CheckBox {  text: qsTr("Student")                   ; name: "students"          ; checked: true}
                CheckBox {  text: qsTr("Wilcoxon signed-rank")      ; name: "mannWhitneyU"  }
                CheckBox {  text: qsTr("Z Test")                    ; name: "zTest"  ; id: zTest}
            }

            GridLayout {
                Label { text: qsTr("Test value:") }                             TextField { text: "0" ; name: "testValue"; inputType: "number"}
                Label { text: qsTr("Std. deviation:"); visible: zTest.checked } TextField { text: "1.0" ; name: "stddev"; inputType: "number"; visible: zTest.checked}
            }

            ButtonGroup {
                title: qsTr("Alt. Hypothesis")
                name: "hypothesis"
                RadioButton {   text: qsTr("≠ Test value") ; name: "notEqualToTestValue"       ; checked: true}
                RadioButton {   text: qsTr("> Test value") ; name: "greaterThanTestValue"    }
                RadioButton {   text: qsTr("< Test value") ; name: "lessThanTestValue"    }
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
