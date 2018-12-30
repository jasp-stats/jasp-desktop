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
            title         : qsTr("Variables")
            name          : "pairs"
            allowedColumns: ["scale"]
            listViewType  : "AssignedPairs"
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Correlation Coefficients")

                CheckBox { text: qsTr("Pearson's rho")  ; name: "pearson"     ; checked: true }
                CheckBox { text: qsTr("Kendall's tau-b"); name: "kendallsTauB"                }
            }

            ButtonGroup {
                title: qsTr("Hypothesis")
                name : "hypothesis"

                RadioButton { text: qsTr("Correlated")           ; name: "correlated"          ; checked: true }
                RadioButton { text: qsTr("Correlated positively"); name: "correlatedPositively"                }
                RadioButton { text: qsTr("Correlated negatively"); name: "correlatedNegatively"                }
            }

            BayesFactorType { }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                CheckBox { text: qsTr("Credible intervals"); name: "credibleInterval" }
            }

            GroupBox {
                title: qsTr("Plots")

                CheckBox { text: qsTr("Scatter plot")                 ; name: "plotScatter"; }

                CheckBox { text: qsTr("Prior and posterior")          ; name: "plotPriorAndPosterior"                  ; id: plotPriorAndPosterior }
                CheckBox { text: qsTr("Additional info")              ; name: "plotPriorAndPosteriorAdditionalInfo"    ; Layout.leftMargin: 20; checked: true; enabled: plotPriorAndPosterior.checked }

                CheckBox { text: qsTr("Bayes factor robustness check"); name: "plotBayesFactorRobustness"              ; id: plotBayesFactorRobustness }
                CheckBox { text: qsTr("Additional info")              ; name: "plotBayesFactorRobustnessAdditionalInfo"; Layout.leftMargin: 20; checked: true; enabled: plotBayesFactorRobustness.checked }

                CheckBox { text: qsTr("Sequential analysis")          ; name: "plotSequentialAnalysis"                 ; id: plotSequentialAnalysis }
            }

            GroupBox {
                title: qsTr("Prior")

                GridLayout {
                    Label     { text: qsTr("Stretched beta prior width")           }
                    TextField { text: "1"; name: "priorWidth"; inputType: "number" }
                }
            }
        }
    }

    ExpanderButton {
        text: qsTr("Options")

        GridLayout {

            ButtonGroup {
                title: qsTr("Missing values")
                name: "missingValues"
                RadioButton { text: qsTr("Exclude cases analysis by analysis"); name: "excludeAnalysisByAnalysis"; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")            ; name: "excludeListwise"                }
            }
        }
    }
}
