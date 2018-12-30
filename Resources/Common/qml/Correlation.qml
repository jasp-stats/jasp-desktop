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
            allowedColumns: ["scale", "ordinal"]
        }
    }

    GridLayout {
        ColumnLayout {
            spacing: 15

            GroupBox {
                title: qsTr("Correlation Coefficients")

                CheckBox { text: qsTr("Pearson")        ; name: "pearson"     ; checked: true }
                CheckBox { text: qsTr("Spearman")       ; name: "spearman"                    }
                CheckBox { text: qsTr("Kendall's tau-b"); name: "kendallsTauB"                }
            }

            ButtonGroup {
                title: qsTr("Hypothesis")                        ; name: "hypothesis"

                RadioButton { text: qsTr("Correlated")           ; name: "correlated"          ; checked: true }
                RadioButton { text: qsTr("Correlated positively"); name: "correlatedPositively"                }
                RadioButton { text: qsTr("Correlated negatively"); name: "correlatedNegatively"                }
            }
        }

        ColumnLayout {
            spacing: 15

            GroupBox {
                // title: qsTr("Additional Statistics")

                CheckBox     { text: qsTr("Display pairwise table")       ; name: "displayPairwise"                                                                                                }
                CheckBox     { text: qsTr("Report significance")          ; name: "reportSignificance"         ; checked: true                                                                     }
                CheckBox     { text: qsTr("Flag significant correlations"); name: "flagSignificant"                                                                                                }
                CheckBox     { text: qsTr("Confidence intervals")         ; name: "confidenceIntervals"        ; id: confidenceInterval                                                            }
                PercentField { label.text: qsTr("Interval")               ; name: "confidenceIntervalsInterval"; defaultValue: 95     ; Layout.leftMargin: 23; enabled: confidenceInterval.checked }
                CheckBox     { text: qsTr("Vovk-Sellke maximum p-ratio")  ; name: "VovkSellkeMPR"                                                                                                  }
            }

            GroupBox {
                title        : qsTr("Plots")

                CheckBox { text: qsTr("Correlation matrix")     ; name: "plotCorrelationMatrix"; id: plotCorrelationMatrix                                         }
                CheckBox { text: qsTr("Densities for variables"); name: "plotDensities"        ; Layout.leftMargin: 20    ; enabled: plotCorrelationMatrix.checked }
                CheckBox { text: qsTr("Statistics")             ; name: "plotStatistics"       ; Layout.leftMargin: 20    ; enabled: plotCorrelationMatrix.checked }
            }
        }
    }
}
