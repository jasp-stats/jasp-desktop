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
    
    VariablesForm {
        defaultAssignedVariablesList {
            name: "pairs"
            allowedColumns: ["scale"]
            listViewType: "AssignedPairs"
        }
    }

    GridLayout {
        ColumnLayout {
            RadioButtonGroup {
                name: "corcoefficient"
                title: qsTr("Correlation Coefficients")
                RadioButton { text: qsTr("Pearson's rho")  ; name: "Pearson" ; checked: true }
                RadioButton { text: qsTr("Kendall's tau-b"); name: "Kendall" }
            }

            RadioButtonGroup {
                title: qsTr("Hypothesis")
                name: "hypothesis"
                RadioButton { text: qsTr("Correlated")  ; name: "correlated" ; checked: true }
                RadioButton { text: qsTr("Correlated positively")  ; name: "correlatedPositively" }
                RadioButton { text: qsTr("Correlated negatively")  ; name: "correlatedNegatively" }
            }

            BayesFactorType {}

            RadioButtonGroup {
                title: qsTr("Missing Values")
                name: "missingValues"
                RadioButton { text: qsTr("Exclude cases analysis by analysis")  ; name: "excludeAnalysisByAnalysis"; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise")              ; name: "excludeListwise" }
            }

        }

        ColumnLayout {

            GroupBox {
                CheckBox { text: qsTr("Credible intervals") ; name: "credibleInterval"; id: credibleInterval }
                PercentField { text: qsTr("Interval") ; name: "ciValue"; defaultValue: 95; enabled:  credibleInterval.checked; indent: true; debug: true}
            }

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Scatterplot")          ; name: "plotScatter" }
                CheckBox { text: qsTr("Prior and posterior")  ; name: "plotPriorAndPosterior"; id: plotPriorAndPosterior }
                CheckBox { text: qsTr("Additional info")      ; name: "plotPriorAndPosteriorAdditionalInfo"; enabled: plotPriorAndPosterior.checked; indent: true }
                CheckBox { text: qsTr("Bayes factor robustness check")  ; name: "plotBayesFactorRobustness"; id: plotBayesFactorRobustness }
                CheckBox { text: qsTr("Additional info")      ; name: "plotBayesFactorRobustnessAdditionalInfo"; enabled: plotBayesFactorRobustness.checked; indent: true }
                CheckBox { text: qsTr("Sequential analysis")  ; name: "plotSequentialAnalysis"; id: plotSequentialAnalysis }
                CheckBox { text: qsTr("Robustness check")     ; name: "plotSequentialAnalysisRobustness"; enabled: plotSequentialAnalysis.checked; indent: true; debug: true }
            }

            GroupBox {
                title: qsTr("Prior")
                DoubleField { text: qsTr("Stretched beta prior width"); name: "priorWidth"; defaultValue: 1.0; doubleValidator { top: 2; decimals: 1 } }
            }
        }

    }

}
