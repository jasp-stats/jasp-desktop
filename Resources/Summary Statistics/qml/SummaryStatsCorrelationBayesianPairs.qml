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
                Label { text: qsTr("Sample size") } TextField { text: "" ; name: "sampleSize" ; inputType: "integer"; validator: IntValidator { bottom: 2 } }
            }
        }
    }

    Divider { }

    GridLayout {
        ColumnLayout {
            spacing: 15

            ButtonGroup {
                title: qsTr("Correlation Coefficient") ; name: "correlationCoefficient"

                Row {
                    RadioButton { text: qsTr("Pearson's rho") ; name: "pearsonRho"; id: pearsonRho; checked: true}
                    TextField   { name: "pearsonRhoValue"; inputType: "number"; text: "0"; visible: pearsonRho.checked; validator: DoubleValidator { bottom: -1; top: 1 } }
                }
                Row {
                    RadioButton { text: qsTr("Kendall's tau-b") ; name: "kendallTau"; id: kendallTau }
                    TextField   { name: "kendallTauValue"; inputType: "number"; text: "0"; visible: kendallTau.checked; validator: DoubleValidator { bottom: -1; top: 1 } }
                }
            }
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            ButtonGroup {
                title: qsTr("Hypothesis")
                name: "hypothesis"

                RadioButton { text: qsTr("Correlated")            ; name: "correlated" ; checked: true }
                RadioButton { text: qsTr("Correlated positively") ; name: "correlatedPositively"       }
                RadioButton { text: qsTr("Correlated negatively") ; name: "correlatedNegatively"       }
            }

            BayesFactorType { }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Prior and posterior")           ; name: "plotPriorAndPosterior"                  ; id: plotPriorAndPosterior }
                CheckBox { text: qsTr("Additional info")               ; name: "plotPriorAndPosteriorAdditionalInfo"    ; Layout.leftMargin: 20; checked: true; enabled: plotPriorAndPosterior.checked}

                CheckBox { text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
                CheckBox { text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: plotBayesFactorRobustness.checked}
            }

            GroupBox {
                title: qsTr("Prior")
                GridLayout {
                    Label { text: qsTr("Stretched beta prior width") } TextField { text: "1" ; name: "priorWidth" ; inputType: "number"; validator: DoubleValidator {bottom: 0; top: 2} }
                }
            }
        }
    }
}
