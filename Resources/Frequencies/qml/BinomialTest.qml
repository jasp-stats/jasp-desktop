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
import JASP.Theme 1.0

Form
{
	usesJaspResults: true

	VariablesForm
	{
		height: Theme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); suggestedColumns: ["ordinal", "nominal"] }
	}

	DoubleField { name: "testValue"; label: qsTr("Test value: "); defaultValue: 0.5 ; max: 1; decimals: 2; Layout.columnSpan: 2 }

	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "hypothesis"
		RadioButton { value: "notEqualToTestValue";		label: qsTr("≠ Test value"); checked: true	}
		RadioButton { value: "greaterThanTestValue";	label: qsTr("> Test value")					}
		RadioButton { value: "lessThanTestValue";		label: qsTr("< Test value")					}
	}

	Group
	{
		title: qsTr("Additional Statisics")
		CheckBox
		{
			name: "confidenceInterval";	label: qsTr("Confidence interval")
			CIField { name: "confidenceIntervalInterval";	label: qsTr("Interval") }
		}
		CheckBox { name: "VovkSellkeMPR"; label: qsTr("Vovk-Sellke maximum p-ratio") }
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "descriptivesPlots";					label: qsTr("Descriptive plots")
			CIField { name: "descriptivesPlotsConfidenceInterval"; label: qsTr("Confidence interval") }
		}
	}

}
