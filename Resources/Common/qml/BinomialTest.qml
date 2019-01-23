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
	
	VariablesForm
	{
		AssignedVariablesList { name: "variables"; allowedColumns: ["ordinal", "nominal"] }
	}
	
	DoubleField { name: "testValue"; text: qsTr("Test value: "); defaultValue: 0.5 ; doubleValidator { top: 1; decimals: 2 } }    
	
	GridLayout 
	{
		GroupBox 
		{
			title: qsTr("Additional Statisics")
			CheckBox     { name: "confidenceInterval";			text: qsTr("Confidence interval"); id: confidenceInterval }
			PercentField { name: "confidenceIntervalInterval";	text: qsTr("Interval"); enabled: confidenceInterval.checked; defaultValue: 95; indent: true }
			CheckBox     { name: "VovkSellkeMPR";				text: qsTr("Vovk-Sellke maximum p-ratio");    }
		}
		
		RadioButtonGroup 
		{
			title: qsTr("Hypothesis")
			name: "hypothesis"
			RadioButton { value: "notEqualToTestValue";		text: qsTr("≠ Test value"); checked: true	}
			RadioButton { value: "greaterThanTestValue";	text: qsTr("> Test value")					}
			RadioButton { value: "lessThanTestValue";		text: qsTr("< Test value")					}
		}
		
		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox     { name: "descriptivesPlots";					text: qsTr("Descriptive plots"); id: descriptivesPlots }
			PercentField { name: "descriptivesPlotsConfidenceInterval"; text: qsTr("Confidence Interval"); enabled: descriptivesPlots.checked; defaultValue: 95; indent: true }
		}
	}
	
	CheckBox { name: "frequencyTables"; text: qsTr("Frequency tables (nominal and ordinal variables)") }
	
}
