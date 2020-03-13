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

import QtQuick			2.8
import JASP.Controls	1.0


Form
{
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); suggestedColumns: ["ordinal", "scale"] }
	}
	
	Group
	{
		title: qsTr("Scale Statistics")
		CheckBox { name: "mcDonaldScale";					label: qsTr("McDonald's ω");	checked: true	}
		CheckBox
		{
			id: chronbach
			name: "alphaScale";								label: qsTr("Cronbach's α")
			RadioButtonGroup
			{
				name: "alphaScaleStandardized"
				RadioButton { value: "_1unstandardized";	label: qsTr("Unstandardized"); checked: true; id: alphaUnstandardized	}
				RadioButton { value: "_2standardized";		label: qsTr("Standardized");											}
			}
		}
		CheckBox { name: "gutmannScale";					label: qsTr("Gutmann's λ6")					}
		CheckBox { name: "glbScale";						label: qsTr("Greatest lower bound")			}
		CheckBox { name: "averageInterItemCor";				label: qsTr("Average interitem correlation")	}
		CheckBox { name: "meanScale";						label: qsTr("Mean")							}
		CheckBox { name: "sdScale";							label: qsTr("Standard deviation")			}
	}

	Group
	{
		title: qsTr("Individual Item Statistics")
		CheckBox { name: "mcDonaldItem";					label: qsTr("McDonald's ω (if item dropped)")	}
		CheckBox { name: "alphaItem";						label: qsTr("Cronbach's α (if item dropped)")	}
		CheckBox { name: "gutmannItem";						label: qsTr("Gutmann's λ6 (if item dropped)")	}
		CheckBox { name: "meanItem";						label: qsTr("Mean")								}
		CheckBox { name: "sdItem";							label: qsTr("Standard deviation")				}
		CheckBox { name: "itemRestCor";						label: qsTr("Item-rest correlation")				}
	}

	Section
	{
		title: qsTr("Reverse-Scaled Items")
		
		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			AvailableVariablesList { name: "normalScaledItems";	 title: qsTr("Normal-Scaled Items"); source: "variables" }
			AssignedVariablesList {  name: "reverseScaledItems"; title: qsTr("Reverse-Scaled Items") }
		}
	}
	
	Section
	{
		title: qsTr("Advanced Options")
		
		RadioButtonGroup {
			title: qsTr("Missing Values")
			name: "missingValues"
			RadioButton { value: "excludeCasesListwise"; label: qsTr("Exclude cases listwise"); checked: true	}
			RadioButton { value: "excludeCasesPairwise"; label: qsTr("Exclude cases pairwise")					}
		}

		Group
		{
			title: qsTr("Confidence Interval")
			enabled: chronbach.checked && alphaUnstandardized.checked
			CheckBox {
				name: "confAlpha"; label: qsTr("Cronbach's α analytical")
				CIField { name: "confAlphaLevel"; label: qsTr("Confidence"); decimals: 1 }
			}
		}
	}
}
