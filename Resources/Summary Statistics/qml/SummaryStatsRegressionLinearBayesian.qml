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
                Label { text: qsTr("Sample size") } TextField { text: "" ; name: "sampleSize" ; inputType: "integer"; validator: IntValidator { bottom: 3 } }
            }
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Null model")
                GridLayout {
                    Label { text: qsTr("Number of covariates") } TextField { text: "" ; name: "numberOfCovariatesNull" ; inputType: "integer"; validator: IntValidator { bottom: 0 } }
                    Label { text: qsTr("R-squared")            } TextField { text: "" ; name: "unadjustedRSquaredNull" ; inputType: "number" ; validator: DoubleValidator { bottom: 0; top: 0.9999 } }
                }
            }
        }
        ColumnLayout {
            spacing: 15
            GroupBox {
                title: qsTr("Alternative model")
                GridLayout {
                    Label { text: qsTr("Number of covariates") } TextField { text: "" ; name: "numberOfCovariatesAlternative" ; inputType: "integer"; validator: IntValidator { bottom: 1 } }
                    Label { text: qsTr("R-squared")            } TextField { text: "" ; name: "unadjustedRSquaredAlternative" ; inputType: "number" ; validator: DoubleValidator { bottom: 0; top: 0.9999 } }
                }
            }
        }

    }

    Divider { }

    GridLayout {
        ColumnLayout {
            spacing: 15
            BayesFactorType { }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Bayes factor robustness check") ; name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
                CheckBox { text: qsTr("Additional info")               ; name: "plotBayesFactorRobustnessAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: plotBayesFactorRobustness.checked}
            }
        }
    }

    ExpanderButton {
        text: qsTr("Advanced Options")

        GroupBox {
            title: qsTr("Prior")
            GridLayout {
                Label { text: qsTr("r scale covariates") } TextField { text: "0.3536" ; name: "priorWidth" ; inputType: "number"; validator: DoubleValidator {bottom: 0; top: 2} }
            }
        }
    }
}
