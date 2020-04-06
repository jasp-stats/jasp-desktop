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

import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts  1.3

Form
{
    usesJaspResults: true
    plotHeight: 300
    plotWidth:  350

    VariablesForm
    {
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList  { name: "variables"; title: qsTr("Variables"); suggestedColumns: ["scale"] }
    }

    Group
    {
        DoubleField { name: "mu";	text: qsTr("Test value");		defaultValue: 0}

        RadioButtonGroup
        {
           name: "equivalenceRegion"
           title: qsTr("Equivalence Region")
           GridLayout
           {
              columns: 3
              rowSpacing: jaspTheme.rowGroupSpacing
              columnSpacing: 0

              RadioButton { value: "region"; checked: true; id: region }
              DoubleField { name: "lowerbound"; label: qsTr("from")	; max: upperbound.value; defaultValue: -0.5; id: lowerbound; negativeValues: true; enabled: region.checked; inclusive: JASP.None}
              DoubleField { name: "upperbound"; label: qsTr("to")	; min: lowerbound.value; defaultValue: 0.5;  id: upperbound; negativeValues: true; enabled: region.checked; Layout.leftMargin: jaspTheme.columnGroupSpacing; inclusive: JASP.None}

              RadioButton { value: "lower"; id: lower }
              Label		  { text: qsTr("from %1").arg(" -∞ "); enabled: lower.checked}
              DoubleField { name: "lower_max"; label: qsTr("to"); id: lower_max; defaultValue: 0.5; negativeValues: true; enabled: lower.checked; Layout.leftMargin: jaspTheme.columnGroupSpacing; inclusive: JASP.None}

              RadioButton { value: "upper"; id: upper }
              DoubleField { name: "upper_min"; label: qsTr("from"); id: upper_min; defaultValue: -0.5; negativeValues: true; enabled: upper.checked}
              Label		  { text: qsTr("to %1").arg(" ∞ "); Layout.leftMargin: jaspTheme.columnGroupSpacing; enabled: upper.checked}
            }
        }
    }

    Group
    {
        title: qsTr("Additional Statistics")
        CheckBox { name: "descriptives";					text: qsTr("Descriptives")	}
        CheckBox { name: "massPriorPosterior";              text: qsTr("Prior and posterior mass") }
    }

    Group
    {
        title: qsTr("Plots")
        CheckBox
        {
            name: "priorandposterior";		                        label: qsTr("Prior and posterior")
            CheckBox { name: "priorandposteriorAdditionalInfo";		label: qsTr("Additional info"); checked: true }
        }

        CheckBox
        {
             name: "plotSequentialAnalysis";		label: qsTr("Sequential analysis")
             CheckBox { name: "plotSequentialAnalysisRobustness";		label: qsTr("Robustness check") }
        }
    }

    RadioButtonGroup
    {
        name: "missingValues"
        title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	label: qsTr("Exclude cases per dependent variable"); checked: true	}
        RadioButton { value: "excludeListwise";				label: qsTr("Exclude cases listwise")							}
    }

    SubjectivePriors { }
}
