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

    CheckBox { name: "simulatepval"; checked: false; visible: false }

    VariablesForm {
        height: 170
        marginBetweenVariablesLists: 15
        defaultAssignedVariablesList {
            name: "factor"
            title: qsTr("Factor")
            singleItem: true
            allowedColumns: ["ordinal", "nominal"]
        }
        AssignedVariablesList {
            name: "counts"
            title: qsTr("Counts")
            singleItem: true
            allowedColumns: ["ordinal", "scale"]
        }
        AssignedVariablesList {
            name: "exProbVar"
            title: qsTr("Expected Counts")
            singleItem: true
            allowedColumns: ["ordinal", "scale"]
        }
    }

    ButtonGroup {
        id: hypothesisGroup
        title: qsTr("Alt. Hypothesis")
        name: "hypothesis"
        RadioButton { text: qsTr("Multinomial test"); name: "multinomialTest"; checked: true }
        RadioButton { text: qsTr("χ² test"); name: "expectedProbs"; id: expectedProbs }

        Chi2TestTableView {
            name: "tableWidget"
            width: form.availableWidth - hypothesisGroup.leftPadding
            visible: expectedProbs.checked
            syncModels: "factor"
        }
    }


    GridLayout {
        GroupBox {
            title: qsTr("Additional Statistics")
            CheckBox { text: qsTr("Descriptives"); name: "descriptives"; id: descriptives}
            RowLayout {
                Layout.leftMargin: Theme.indentationLength
                enabled: descriptives.checked
                CheckBox { text: qsTr("Confidence interval"); name: "confidenceInterval" }
                PercentField { name: "confidenceIntervalInterval"; value: "95" }
            }
            CheckBox { text: qsTr("Vovk-Dellke maximum p-ratio"); name: "VovkSellkeMPR" }
        }
        ColumnLayout {
            ButtonGroup {
                name: "countProp"
                title: qsTr("Display")
                RadioButton { text: qsTr("Counts"); name: "descCounts"; checked: true }
                RadioButton { text: qsTr("Proportions"); name: "descProps" }
            }

            GroupBox {
                title: qsTr("Plots")
                CheckBox { text: qsTr("Descriptives plot"); name: "descriptivesPlot"; id: descriptivesPlot }
                PercentField { label.text: qsTr("Confidence interval"); name: "descriptivesPlotConfidenceInterval"; text: "95"; enabled: descriptivesPlot.checked }
            }
        }
    }
}
