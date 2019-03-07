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
	usesJaspResults	: true

	CheckBox { name: "simulatepval"; checked: false; visible: false }

	VariablesForm
	{
		height	: 170
		marginBetweenVariablesLists	: 15
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "factor";		title: qsTr("Factor");			singleVariable: true; allowedColumns: ["ordinal", "nominal"] }
		AssignedVariablesList { name: "counts";		title: qsTr("Counts");			singleVariable: true; allowedColumns: ["ordinal", "scale"] }
		AssignedVariablesList { name: "exProbVar";	title: qsTr("Expected Counts"); singleVariable: true; allowedColumns: ["ordinal", "scale"] }
	}

	RadioButtonGroup
	{
		id		: hypothesisGroup
		name	: "hypothesis"
		title	: qsTr("Alt. Hypothesis")
		Layout.columnSpan: 2

		RadioButton { value: "multinomialTest";	label: qsTr("Equal proportions");	 checked: true				}
		RadioButton { value: "expectedProbs";	label: qsTr("Expected proportions"); id: expectedProbs			}

		Chi2TestTableView
		{
			name: "tableWidget"
			width: form.availableWidth - hypothesisGroup.leftPadding
			visible: expectedProbs.checked
			source: "factor"
		}
	}

	ColumnLayout
	{
		BayesFactorType { }

		Group
		{
			title	: qsTr("Additional Statistics")
			CheckBox
			{
				name	: "descriptives"; label: qsTr("Descriptives")
				CheckBox
				{
					name				: "confidenceInterval"; label: qsTr("Confidence interval")
					childrenOnSameRow	: true
					PercentField { name: "confidenceIntervalInterval"; defaultValue: 95	}
				}
			}
		}
	}

	ColumnLayout
	{
		RadioButtonGroup
		{
			name	: "countProp"
			title	: qsTr("Display")
			RadioButton { value: "descCounts";	label: qsTr("Counts"); checked: true	}
			RadioButton { value: "descProps";	label: qsTr("Proportions")				}
		}

		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "descriptivesPlot"; label: qsTr("Descriptives plot")
				PercentField { name: "descriptivesPlotConfidenceInterval"; label: qsTr("Confidence interval"); defaultValue: 95 }
			}
		}
	}


	ExpanderButton
	{
		title	: qsTr("Prior")

		Chi2TestTableView
		{
			name	: "priorCounts"
			width	: form.availableWidth - hypothesisGroup.leftPadding
			source	: "factor"

			showAddButton		: false
			showDeleteButton	: false
			tableType			: "PriorCounts"
			itemType			: "integer"
		}
	}
}
