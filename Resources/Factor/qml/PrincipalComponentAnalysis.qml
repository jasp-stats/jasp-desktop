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
	usesJaspResults: true

	CheckBox { name: "incl_GoF"; checked: true; visible: false }
	CheckBox { name: "incl_fitIndices"; checked: false; visible: false }
	CheckBox { name: "incl_loadings"; checked: true; visible: false }
	IntegerField { name: "plotHeightPathDiagram"; defaultValue: 0; visible: false }
	IntegerField { name: "plotHeightScreePlot"  ; defaultValue: 300; visible: false }
	IntegerField { name: "plotWidthPathDiagram" ; defaultValue: 480; visible: false }
	IntegerField { name: "plotWidthScreePlot"   ; defaultValue: 300; visible: false }

	VariablesForm
	{
		height: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList  { id: variables; name: "variables"; title: qsTr("Variables"); suggestedColumns: ["scale"] }
	}


	RadioButtonGroup
	{
		name: "factorMethod"
		title: qsTr("Number of Components")
		RadioButton
		{
			value: "parallelAnalysis"; label: qsTr("Parallel analysis"); checked: true
		}
		RadioButton
		{
			value: "eigenValues"; label: qsTr("Eigenvalues")
			DoubleField { name: "eigenValuesBox"; label: qsTr("Eigenvalues above"); defaultValue: 1; decimals: 1 }
		}
		RadioButton
		{
			value: "manual"; label: qsTr("Manual")
			IntegerField { name: "numberOfFactors"; label: qsTr("Number of components"); defaultValue: 1; min: 1 }
		}
	}

    Group
    {
        RadioButtonGroup
        {
            name: "rotationMethod"
            title: qsTr("Rotation")
            RadioButton
            {
                value	: "orthogonal"
                label	: qsTr("Orthogonal")
                DropDown { name: "orthogonalSelector"; values: ["none", "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT"] }
            }
            RadioButton
            {
                value	: "oblique"
                label	: qsTr("Oblique")
                checked	: true
                DropDown { name: "obliqueSelector"; values: [ "promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster", "geominQ" ] }
            }
        }

        RadioButtonGroup
        {
            name: "basedOn"
            title: qsTr("Base decomposition on")
            RadioButton
            {
                value: "correlation"
                label: qsTr("Correlation matrix")
                checked: true
            }
            RadioButton
            {
                value: "covariance"
                label: qsTr("Covariance matrix")
            }
        }
    }

	Section
	{
		title: qsTr("Output Options")

		Slider
		{
			name: "highlightText"
			title: "Highlight"
			value: 0.4
		}

		Group
		{
			title: qsTr("Includes Tables")
			CheckBox { name: "incl_correlations";	label: qsTr("Component correlations")	}
			CheckBox { name: "incl_pathDiagram";	label: qsTr("Path diagram")				}
			CheckBox { name: "incl_screePlot";		label: qsTr("Scree plot")				}
		}

		RadioButtonGroup
		{
			name: "missingValues"
			title: qsTr("Missing Values")
			RadioButton { value: "pairwise";		label: qsTr("Exclude cases pairwise"); checked: true	}
			RadioButton { value: "listwise";		label: qsTr("Exclude cases listwise")					}
		}

		CheckBox 
		{
			debug: true
            id: addPC
            name: "addPC"
            text: qsTr("Add PC scores to data")
            enabled: variables.count > 1

            ComputedColumnField { 
                name: 		"PCPrefix"
                text: 		"Prefix: "
                fieldWidth: 120
                visible:    addPC.checked
            }
        }
	}
}
