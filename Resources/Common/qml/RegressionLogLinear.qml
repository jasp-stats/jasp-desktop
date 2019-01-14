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
		AssignedVariablesList { name: "counts";		title: qsTr("Counts (optional)"); singleItem: true	}
		AssignedVariablesList { name: "factors";	title: qsTr("Factors"); itemType: "fixedFactors"; allowedColumns: ["ordinal", "nominal"] }			
	}
	
	ExpanderButton
	{
		title: qsTr("Model")
		
		VariablesForm
		{
			height: 200
			listWidth: parent.width * 5 / 9
			availableVariablesList { name: "availableTerms"; title: qsTr("Components"); width: parent.width / 4; syncModels: ['factors'] }
			AssignedVariablesList {  name: "modelTerms";	 title: qsTr("Model terms"); listViewType: "Interaction" }
		}
	}
	
	ExpanderButton
	{
		text: qsTr("Statistics")
		
		GridLayout
		{
			GroupBox
			{
				title: qsTr("Regression Coefficient")
				CheckBox { text: qsTr("Estimates")            ; name: "regressionCoefficientsEstimates" }
				CheckBox { text: qsTr("Confidence intervals") ; name: "regressionCoefficientsConfidenceIntervals"; id: interval }
				PercentField { text: qsTr("Interval")         ; name: "regressionCoefficientsConfidenceIntervalsInterval" ; defaultValue: 95 ; enabled: interval.checked; indent: true }
			}
			CheckBox { text: qsTr("Vovk-Sellke maximum p-ratio") ; name: "VovkSellkeMPR" }
		}
	}
}
