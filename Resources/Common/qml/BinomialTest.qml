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
import JASP.Widgets 1.0


Form {
    id: form

    plotHeight: 240
    plotWidth:  320

    VariablesForm {
        height: 200
        defaultAssignedVariablesList {
            title: qsTr("Variables")
            allowedColumns: ["nominal", "ordinal"]
        }
    }

    GridLayout {
        Label { text: qsTr("Test value:") } TextField { text: "0.5" ; name: "testValue"; inputType: "number" }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            ButtonGroup {
                title: qsTr("Hypothesis")               ; name: "hypothesis"

                RadioButton { text: qsTr("≠ Test value"); name: "notEqualToTestValue" ; checked: true }
                RadioButton { text: qsTr("> Test value"); name: "greaterThanTestValue"                }
                RadioButton { text: qsTr("< Test value"); name: "lessThanTestValue"                   }
            }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Additional Statistics")
                CheckBox { text: qsTr("Confidence interval")        ; name: "confidenceInterval"        ; id: confidenceInterval                                                            }
                PercentField { label.text: qsTr("Interval")         ; name: "confidenceIntervalInterval"; defaultValue: 50     ; Layout.leftMargin: 23; enabled: confidenceInterval.checked }
                CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio"); name: "VovkSellkeMPR"                                                                                                 }
            }

            GroupBox {
                title        : qsTr("Plots")
                CheckBox     {       text: qsTr("Descriptives plots") ; name: "descriptivesPlots"                  ; id: descriptivePlots                                                       }
                PercentField { label.text: qsTr("Confidence interval"); name: "descriptivesPlotsConfidenceInterval"; defaultValue: 95; Layout.leftMargin: 23; enabled: descriptivePlots.checked }
            }
        }
    }
}
