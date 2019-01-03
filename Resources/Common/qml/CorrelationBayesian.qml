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
import JASP.Theme 1.0

Form {
    id: form
    
    VariablesForm {
        defaultAssignedVariablesList.allowedColumns: ["ordinal", "scale"]
    }

    GroupBox {
        title: qsTr("Correlation Coefficients")
        spacing: Theme.rowGridSpacing

        GridLayout {
            GroupBox {
                CheckBox { text: qsTr("Pearson's rho")  ; name: "pearson" ; checked: true }
                CheckBox { text: qsTr("Spearman")       ; name: "spearman" ; debug: true }
                CheckBox { text: qsTr("Kendall's tau-b"); name: "kendallsTauB" }
            }

            GroupBox {
                CheckBox { text: qsTr("Report Bayes factors")           ; name: "reportBayesFactors"; checked: true }
                CheckBox { text: qsTr("Flag supported correlations")    ; name: "flagSupported" }
                CheckBox { text: qsTr("Credible intervals")             ; name: "credibleInterval"; id: credibleInterval }
                PercentField { text: qsTr("Interval")                   ; name: "ciValue"; defaultValue: 95; enabled:  credibleInterval.checked; indent: true}
            }
        }
    }

    GridLayout {
        ButtonGroup {
            title: qsTr("Hypothesis")
            name: "hypothesis"
            RadioButton { text: qsTr("Correlated")  ; name: "correlated" ; checked: true }
            RadioButton { text: qsTr("Correlated positively")  ; name: "correlatedPositively" }
            RadioButton { text: qsTr("Correlated negatively")  ; name: "correlatedNegatively" }
        }

        GroupBox {
            title: qsTr("Plots")
            CheckBox { text: qsTr("Correlation matrix")         ; name: "plotCorrelationMatrix"; id: plotCorrelationMatrix }
            GroupBox {
                enabled: plotCorrelationMatrix.checked
                indent: true
                CheckBox { text: qsTr("Densities for variables") ; name: "plotDensitiesForVariables" }
                CheckBox { text: qsTr("Posteriors under H\u2081")      ; name: "plotPosteriors" }
            }
        }

        BayesFactorType {}

        DoubleField { text: qsTr("Stretched beta prior width"); name: "priorWidth"; defaultValue: 1.0; doubleValidator { top: 2; decimals: 1 } }
    }

    ExpanderButton {
        text: qsTr("Options")

        ButtonGroup {
            title: qsTr("Missing Values")
            name: "missingValues"
            RadioButton { text: qsTr("Exclude cases pairwise"); name: "excludePairwise"; checked: true }
            RadioButton { text: qsTr("Exclude cases listwise"); name: "excludeListwise" }
        }
    }
}
