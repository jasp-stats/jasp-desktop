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
        defaultAssignedVariablesList.allowedColumns: ["ordinal", "scale"]
    }

    GroupBox {
        title: qsTr("Correlation Coefficients")

        GridLayout {
            GroupBox {
                CheckBox { text: qsTr("Pearson"); name: "pearson"; checked: true }
                CheckBox { text: qsTr("Spearman"); name: "spearman" }
                CheckBox { text: qsTr("Kendall's tau-b"); name: "kendallsTauB" }
            }

            GroupBox {
                CheckBox { text: qsTr("Display pairwise table")         ; name: "displayPairwise" }
                CheckBox { text: qsTr("Report significance")            ; name: "reportSignificance"; checked: true }
                CheckBox { text: qsTr("Flag significant correlations")  ; name: "flagSignificant" }
                CheckBox { text: qsTr("Confidence intervals")           ; name: "confidenceIntervals"; id: confidenceIntervals }
                PercentField { text: qsTr("Interval")                   ; name: "confidenceIntervalsInterval"; defaultValue: 95; enabled:  confidenceIntervals.checked; indent: true}
                CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio")    ; name: "VovkSellkeMPR" }
            }
        }
    }

    GridLayout {
        RadioButtonGroup {
            title: qsTr("Hypothesis")
            name: "hypothesis"
            RadioButton { text: qsTr("Correlated")  ; name: "correlated" ; checked: true }
            RadioButton { text: qsTr("Correlated positively")  ; name: "correlatedPositively" }
            RadioButton { text: qsTr("Correlated negatively")  ; name: "correlatedNegatively" }
        }

        GroupBox {
            title: qsTr("Plots")
            CheckBox { text: qsTr("Display pairwise table")         ; name: "plotCorrelationMatrix"; id: plotCorrelationMatrix }
            GroupBox {
                enabled: plotCorrelationMatrix.checked
                indent: true
                CheckBox { text: qsTr("Densities for variables")    ; name: "plotDensities" }
                CheckBox { text: qsTr("Statistics")                 ; name: "plotStatistics" }
            }

        }
    }

    ExpanderButton {
        text: qsTr("Options")
        debug: true

        GroupBox {
            title: qsTr("Statistics")
            CheckBox { text: qsTr("Means and standard deviations")                 ; name: "meansAndStdDev" }
            CheckBox { text: qsTr("Cross-product deviations and covariances")      ; name: "crossProducts" }
        }

        RadioButtonGroup {
            title: qsTr("Missing Values")
            name: "missingValues"
            RadioButton { text: qsTr("Exclude cases pairwise"); name: "excludePairwise"; checked: true }
            RadioButton { text: qsTr("Exclude cases listwise"); name: "excludeListwise" }
        }
    }
}
