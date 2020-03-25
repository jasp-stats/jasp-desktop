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

Form
{
    plotHeight: 300
    plotWidth:  350

    VariablesForm
    {
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        AvailableVariablesList { name: "allVariablesList" }
        AssignedVariablesList { name: "variables";			title: qsTr("Variables");			suggestedColumns: ["scale"]	}
        AssignedVariablesList { name: "groupingVariable";	title: qsTr("Grouping Variable");	suggestedColumns: ["ordinal", "nominal"]; singleVariable: true}
    }

    Group
    {
        title: qsTr("Equivalence Region")
        columns: 2
        DoubleField { name: "lowerbound";	text: qsTr("Lower bound");		defaultValue: -0.5; negativeValues: true; max: upperbound.value; inclusive: JASP.None ; id: lowerbound }
        DoubleField { name: "upperbound";	text: qsTr("Upper bound");      defaultValue: 0.5; negativeValues: true; min: lowerbound.value; inclusive: JASP.None ;id: upperbound }
    }

    Group
    {
        title: qsTr("Additional Statistics")
        CheckBox { name: "descriptives";					text: qsTr("Descriptives")	}
        CheckBox { name: "densityPriorPosterior";              text: qsTr("Prior and Posterior density") }
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

    SubjectivePriors{ }
}
