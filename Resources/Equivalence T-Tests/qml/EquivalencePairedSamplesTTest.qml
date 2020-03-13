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
import QtQuick.Layouts  1.3 as L

Form
{
    usesJaspResults: true
    plotHeight: 300
    plotWidth:  350

    VariablesForm
    {
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
        AvailableVariablesList { name: "allVariablesList" }
        AssignedPairsVariablesList { name: "pairs"; title: qsTr("Variable pairs"); suggestedColumns: ["scale"] }
    }

    Group
    {
        title: qsTr("Equivalence Region")
        columns: 2
        alignTextFields: false
        DoubleField { name: "lowerbound";	text: qsTr("Lower bound");		defaultValue: -0.5; negativeValues: true; max: upperbound.value; inclusive: JASP.None ; id: lowerbound }
        DoubleField { name: "upperbound";	text: qsTr("Upper bound");      defaultValue: 0.5;  negativeValues: true; min: lowerbound.value; inclusive: JASP.None ;id: upperbound }

        DropDown
        {
            L.Layout.columnSpan: 2
            name: "boundstype"
			label: qsTr("Bounds specification in")
            indexDefaultValue: 1
			values:
			[
				{ value: "cohensD", label: qsTr("Cohen's d")	},
				{ value: "raw",     label: qsTr("Raw")			}
			]
		}

        DoubleField { name: "alpha";        text: qsTr("Alpha level");                  defaultValue: 0.05; max: 0.49; min: 0}
    }


    Group
    {
        title: qsTr("Additional Statistics")
        CheckBox { name: "descriptives";					text: qsTr("Descriptives")	}
        CheckBox { name: "equivalenceboundsplot";           text: qsTr("Equivalence bounds plot")	}
    }

    RadioButtonGroup
    {
        name: "missingValues"
        title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	label: qsTr("Exclude cases per dependent variable"); checked: true	}
        RadioButton { value: "excludeListwise";				label: qsTr("Exclude cases listwise")							}
    }
}
