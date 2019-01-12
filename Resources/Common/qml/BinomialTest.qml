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
	id: form
	
	VariablesForm
	{
		AssignedVariablesList { name: "variables"; allowedColumns: ["ordinal", "nominal"] }
	}
	
	DoubleField { text: qsTr("Test value: ") ; name: "testValue" ; defaultValue: 0.5 ; doubleValidator { top: 1; decimals: 2 } }    
	
	GridLayout 
	{
		GroupBox 
		{
			title: qsTr("Additional Statisics")
			CheckBox     { text: qsTr("Confidence interval");           name: "confidenceInterval";         id: confidenceInterval }
			PercentField { text: qsTr("Interval");                      name: "confidenceIntervalInterval"; enabled: confidenceInterval.checked; defaultValue: 95; indent: true }
			CheckBox     { text: qsTr("Vovk-Sellke maximum p-ratio");   name: "VovkSellkeMPR" }
		}
		
		RadioButtonGroup 
		{
			title: qsTr("Hypothesis")
			name: "hypothesis"
			RadioButton { text: qsTr("≠ Test value"); name: "notEqualToTestValue" ; checked: true  }
			RadioButton { text: qsTr("> Test value"); name: "greaterThanTestValue"                 }
			RadioButton { text: qsTr("< Test value"); name: "lessThanTestValue"                    }
		}
		
		GroupBox 
		{
			title: qsTr("Plots")
			CheckBox     { text: qsTr("Descriptive plots");     name: "descriptivesPlots"; id: descriptivesPlots }
			PercentField { text: qsTr("Confidence Interval");   name: "descriptivesPlotsConfidenceInterval"; enabled: descriptivesPlots.checked; defaultValue: 95; indent: true }
		}
	}
	
	CheckBox { text: qsTr("Frequency tables (nominal and ordinal variables)"); name: "frequencyTables"}
	
}
