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
	CheckBox { name: "incl_fitIndices"; checked: false; visible: false }
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
			name: "factorMethod"
			title: qsTr("Number of Factors")
			RadioButton { value: "parallelAnalysis";	text: qsTr("Parallel Analysis");  checked: true	}
			RadioButton { value: "eigenValues";			text: qsTr("Eigenvalues");  id: eigenvalues		}
			DoubleField
			{
				name: "eigenValuesBox";
				text: qsTr("Eigenvalues above");
				defaultValue: 1
				indent: true;
				enabled: eigenvalues.checked
				doubleValidator.decimals: 1
			}
			RadioButton { value: "manual";				text: qsTr("Manual"); id: manual				}
			IntegerField
			{
				name: "numberOfFactors"
				text: qsTr("Number of Factors")
				defaultValue: 1
				intValidator.bottom: 1
				indent: true
				enabled: manual.checked
			}
		}
		
		RadioButtonGroup
		{
			name: "rotationMethod"
			title: qsTr("Rotation")
			RadioButton { value: "orthogonal";	text: qsTr("Orthogonal");  id: rotationOrthogonal }
			DropDown { name: "orthogonalSelector"; values: ["none", "varimax", "quartimax","bentlerT","equamax","varimin"]; enabled: rotationOrthogonal.checked }
			RadioButton { value: "oblique";		text: qsTr("Oblique");  checked: true; id: rotationOblique }
			DropDown { name: "obliqueSelector"; values: [ "promax", "oblimin", "simplimax", "bentlerQ", "biquartimin", "cluster" ]; enabled: rotationOblique.checked }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Output Options")
		
		GridLayout
		{
			Slider
			{
				name: "highlightText"
				title: "Highlight"
				value: 0.4
				from: 0
				to: 1
				orientation: Qt.Vertical
			}
			
			GroupBox
			{
				title: qsTr("Includes tables")
				CheckBox { name: "incl_correlations";	text: qsTr("Factor correlations")	}
				CheckBox { name: "incl_pathDiagram";	text: qsTr("Path diagram")			}
				CheckBox { name: "incl_screePlot";		text: qsTr("Scree plot")			}
			}
			
			RadioButtonGroup
			{
				name: "missingValues"
				title: qsTr("Missing values")
				RadioButton { value: "pairwise";		text: qsTr("Exclude cases pairwise"); checked: true	}
				RadioButton { value: "listwise";		text: qsTr("Exclude cases listwise")				}
			}
		}
	}
}
