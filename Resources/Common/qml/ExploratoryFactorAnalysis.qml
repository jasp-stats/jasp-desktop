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

Form
{
	usesJaspResults: false

    CheckBox { name: "incl_GoF"; checked: true; visible: false }
    CheckBox { name: "incl_loadings"; checked: true; visible: false }
    IntegerField { name: "plotHeightPathDiagram"; defaultValue: 0; visible: false }
    IntegerField { name: "plotHeightScreePlot"  ; defaultValue: 300; visible: false }
    IntegerField { name: "plotWidthPathDiagram" ; defaultValue: 480; visible: false }
    IntegerField { name: "plotWidthScreePlot"   ; defaultValue: 300; visible: false }

    VariablesForm
	{
		AssignedVariablesList { name: "variables"; title: qsTr("Included Variables"); allowedColumns: ["scale"] }
    }

    GridLayout
	{
        RadioButtonGroup
		{
            title: qsTr("Number of Factors")
            name: "factorMethod"
            RadioButton { text: qsTr("Parallel Analysis"); name: "parallelAnalysis"; checked: true }
            RadioButton { text: qsTr("Eigenvalues"); name: "eigenValues"; id: eigenvalues }
            DoubleField
			{
                text: qsTr("Eigenvalues above");
                name: "eigenValuesBox";
                defaultValue: 0
                indent: true;
                enabled: eigenvalues.checked
                doubleValidator.decimals: 1
            }
            RadioButton { text: qsTr("Manual"); name: "manual"; id: manual }
            IntegerField
			{
                text: qsTr("Number of Factors")
                name: "numberOfFactors"
                defaultValue: 1
                intValidator.bottom: 1
                indent: true
                enabled: manual.checked
            }
        }

        RadioButtonGroup
		{
            title: qsTr("Rotation")
            name: "rotationMethod"
            RadioButton { text: qsTr("Orthogonal"); name: "orthogonal"; id: rotationOrthogonal}
            ComboBox { name: "orthogonalSelector"; model: ["none", "varimax", "quartimax","bentlerT","equamax","varimin"]; enabled: rotationOrthogonal.checked }
            RadioButton { text: qsTr("Oblique"); name: "oblique"; checked: true; id: rotationOblique }
            ComboBox { name: "obliqueSelector"; model: [ "promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster" ]; enabled: rotationOblique.checked }
        }
    }

    ExpanderButton
	{
        text: qsTr("Output Options")

        GridLayout
		{
            Slider {
                title: "Highlight"
                name: "highlightText"
                value: 0.4
                from: 0
                to: 1
                orientation: Qt.Vertical
            }

            GroupBox
			{
                title: qsTr("Includes tables")
                CheckBox { text: qsTr("Factor correlations"); name: "incl_correlations" }
                CheckBox { text: qsTr("Additional fit indices"); name: "incl_fitIndices" }
                CheckBox { text: qsTr("Path diagram"); name: "incl_pathDiagram" }
                CheckBox { text: qsTr("Scree plot"); name: "incl_screePlot" }
            }

            RadioButtonGroup
			{
                title: qsTr("Missing values")
                name: "missingValues"
                RadioButton { text: qsTr("Exclude cases pairwise"); name: "pairwise"; checked: true }
                RadioButton { text: qsTr("Exclude cases listwise"); name: "listwise"}
            }
        }
    }
}
