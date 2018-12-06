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

    GridLayout {
        ColumnLayout {
            spacing: 15
            GridLayout {
                Label { text: qsTr("Successes")  } TextField { text: ""    ; name: "successes" ; inputType: "integer"; validator: IntValidator { bottom: 0 } }
                Label { text: qsTr("Failures")   } TextField { text: ""    ; name: "failures"  ; inputType: "integer"; validator: IntValidator { bottom: 0 } }
                Label { text: qsTr("Test value") } TextField { text: "0.5" ; name: "testValue" ; inputType: "number" ; validator: DoubleValidator { bottom: 0; top: 1 } }
            }
        }
    }

    Divider { }

    GridLayout {
        ColumnLayout {
            spacing: 15

            ButtonGroup {
                title: qsTr("Hypothesis")
                name: "hypothesis"

                RadioButton { text: qsTr("\u2260 Test value") ; name: "notEqualToTestValue" ; checked: true }
                RadioButton { text: qsTr("> Test value")      ; name: "greaterThanTestValue"                }
                RadioButton { text: qsTr("< Test value")      ; name: "lessThanTestValue"                   }
            }

            BayesFactorType { }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Prior and posterior") ; name: "plotPriorAndPosterior"               ; id: plotPriorAndPosterior }
                CheckBox { text: qsTr("Additional info")     ; name: "plotPriorAndPosteriorAdditionalInfo" ; Layout.leftMargin: 20; checked: true; enabled: plotPriorAndPosterior.checked}
            }

            GroupBox {
                title: qsTr("Prior")
                GridLayout {
                    Label { text: qsTr("Beta prior: parameter a") } TextField { text: "1" ; name: "betaPriorParamA" ; inputType: "number"; validator: DoubleValidator { bottom: 0 } }
                    Label { text: qsTr("Beta prior: parameter b") } TextField { text: "1" ; name: "betaPriorParamB" ; inputType: "number"; validator: DoubleValidator { bottom: 0 } }
                }
            }
        }
    }
}
