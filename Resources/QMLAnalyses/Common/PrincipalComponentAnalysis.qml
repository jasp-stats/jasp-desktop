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

    CheckBox { name: "incl_GoF"; checked: true; visible: false }
    CheckBox { name: "incl_fitIndices"; checked: false; visible: false }
    CheckBox { name: "incl_loadings"; checked: true; visible: false }
    TextField { name: "plotHeightPathDiagram"; value: "0"; inputType: "integer"; visible: false }
    TextField { name: "plotHeightScreePlot"; value: "300"; inputType: "integer"; visible: false }
    TextField { name: "plotWidthPathDiagram"; value: "480"; inputType: "integer"; visible: false }
    TextField { name: "plotWidthScreePlot"; value: "300"; inputType: "integer"; visible: false }

    VariablesForm {
        defaultAssignedVariablesList {
            title: qsTr("Variables")
            allowedColumns: ["scale"]
        }
    }

    GridLayout {
        ButtonGroup {
            title: qsTr("Number of Factors")
            name: "factorMethod"
            RadioButton { text: qsTr("Parallel Analysis"); name: "parallelAnalysis"; checked: true }
            RadioButton { text: qsTr("Eigenvalues"); name: "eigenValues"; id: eigenvalues }
            TextField {
                label.text: qsTr("Eigenvalues above");
                name: "eigenValuesBox";
                value: "1"
                inputType: "number";
                Layout.leftMargin: 15;
                enabled: eigenvalues.checked
                validator: DoubleValidator { bottom: 0; decimals: 1 }
            }
            RadioButton { text: qsTr("Manual"); name: "manual"; id: manual }
            TextField {
                label.text: qsTr("Number of Factors")
                name: "numberOfFactors"
                value: "1"
                inputType: "integer"
                validator: IntValidator { bottom: 1}
                Layout.leftMargin: 15
                enabled: manual.checked}
        }

        ButtonGroup {
            title: qsTr("Rotation")
            name: "rotationMethod"
            RadioButton { text: qsTr("Orthogonal"); name: "orthogonal"; id: rotationOrthogonal}
            ComboBox { name: "orthogonalSelector"; model: ["none", "varimax", "quartimax","bentlerT","equamax","varimin"]; enabled: rotationOrthogonal.checked }
            RadioButton { text: qsTr("Oblique"); name: "oblique"; checked: true; id: rotationOblique }
            ComboBox { name: "obliqueSelector"; model: [ "promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster" ]; enabled: rotationOblique.checked }
        }
    }

    ExpanderButton {
        text: qsTr("Output Options")

        GridLayout {
            Slider {
                title: "Highlight"
                name: "highlightText"
                value: 0.4
                from: 0
                to: 1
                orientation: Qt.Vertical
            }

            Slider {
                title: "Highlight2"
                name: "highlightText2"
                value: 0.4
                from: 0
                to: 1
            }
            GroupBox {
                title: qsTr("Includes tables")
                CheckBox { text: qsTr("Factor correlations"); name: "incl_correlations" }
                CheckBox { text: qsTr("Path diagram"); name: "incl_pathDiagram" }
                CheckBox { text: qsTr("Scree plot"); name: "incl_screePlot" }
            }

            ButtonGroup {
                title: qsTr("Missing values")
                name: "missingValues"
                RadioButton { text: qsTr("Exclude cases pairwise"); name: "pairwise"; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise"); name: "listwise"}
            }
        }
    }
}
