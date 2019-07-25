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
import JASP.Widgets 1.0
import JASP.Theme 1.0

Form
{
    usesJaspResults: true
	
	VariablesForm
	{
		height: Theme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; suggestedColumns: ["ordinal", "nominal"] }
	}
	
	DoubleField { label: qsTr("Test value: "); name: "testValue"; defaultValue: 0.5 ; max: 1; decimals: 2; Layout.columnSpan: 2 }
	
	RadioButtonGroup
	{
		title: qsTr("Alt. Hypothesis")
		name: "hypothesis"
		RadioButton { value: "notEqualToTestValue";		label: qsTr("≠ Test value"); checked: true	}
		RadioButton { value: "greaterThanTestValue";	label: qsTr("> Test value")					}
		RadioButton { value: "lessThanTestValue";		label: qsTr("< Test value")					}
	}

	Group {
		title: qsTr("Plots")
		CheckBox
		{
			name: "plotPriorAndPosterior";				label: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo"; label: qsTr("Additional info"); checked: true }
		}
		CheckBox { name: "plotSequentialAnalysis";		label: qsTr("Sequential analysis") }
        CheckBox
        {
            name: "descriptivesPlots";					label: qsTr("Descriptive plots")
            CIField { name: "descriptivesPlotsCredibleInterval"; label: qsTr("Credible interval") }
        }
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "priorA"; label: qsTr("Beta prior: parameter a"); defaultValue: 1; min: 0.1; max: 9999; decimals: 1 }
		DoubleField { name: "priorB"; label: qsTr("Beta prior: parameter b"); defaultValue: 1; min: 0.1; max: 9999; decimals: 1 }
	}
}
