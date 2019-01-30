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
	
	VariablesForm
	{
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); allowedColumns: ["ordinal", "nominal"] }
	}
	
	GridLayout
	{
		GroupBox
		{
			title: qsTr("Scale Statistics")
			CheckBox { name: "mcDonaldScale";					text: qsTr("McDonald's ω");	checked: true	}
			CheckBox { name: "alphaScale";						text: qsTr("Cronbach's α");	id: alphaScale	}
			RadioButtonGroup
			{
				name: "alphaScaleStandardized"
				indent: true
				enabled: alphaScale.checked
				RadioButton { value: "_1unstandardized";		text: qsTr("Unstandardized");	checked: true }
				RadioButton { value: "_2standardized";			text: qsTr("Standardized")					}
			}
			CheckBox { name: "gutmannScale";					text: qsTr("Gutmann's λ6")					}
			CheckBox { name: "glbScale";						text: qsTr("Greatest lower bound")			}
			CheckBox { name: "averageInterItemCor";				text: qsTr("Average interitem correlation")	}
			CheckBox { name: "meanScale";						text: qsTr("Mean")							}
			CheckBox { name: "sdScale";							text: qsTr("Standard deviation")			}
		}
		
		GroupBox
		{
			title: qsTr("Individual Item Statistics")
			CheckBox { name: "mcDonaldItem";					text: qsTr("McDonald's ω  (if item dropped)")	}
			CheckBox { name: "alphaItem";						text: qsTr("Cronbach's α (if item dropped)")	}
			CheckBox { name: "gutmannItem";						text: qsTr("Gutmann's λ6 (if item dropped)")	}
			CheckBox { name: "meanItem";						text: qsTr("Mean")								}
			CheckBox { name: "sdItem";							text: qsTr("Standard deviation")				}
			CheckBox { name: "itemRestCor";						text: qsTr("Item-rest correlation")				}
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Reverse-Scaled Items")
		
		VariablesForm
		{
			height: 150
			availableVariablesList { name: "normalScaledItems";	 title: qsTr("Normal-Scaled Items"); source: "variables" }
			AssignedVariablesList {  name: "reverseScaledItems"; title: qsTr("Reverse-Scaled Items") }
		}
	}
	
	ExpanderButton
	{
		title: qsTr("Advanced Options")
		
		GridLayout
		{
			RadioButtonGroup {
				title: qsTr("Missing Values")
				name: "missingValues"
				RadioButton { value: "excludeCasesListwise"; text: qsTr("Exclude cases listwise"); checked: true	}
				RadioButton { value: "excludeCasesPairwise"; text: qsTr("Exclude cases pairwise")					}
			}
			
			GroupBox
			{
				title: qsTr("Confidence Interval")
				CheckBox {		name: "confAlpha";		text: qsTr("Cronbach's α analytical"); id: confAlpha }    
				PercentField {	name: "confAlphaLevel"; text: qsTr("Confidence"); defaultValue: 95; with1Decimal: true; indent: true; enabled: confAlpha.checked }
			}            
		}
	}
}
