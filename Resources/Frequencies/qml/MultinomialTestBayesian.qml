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
	VariablesForm
	{
		height	: 190 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15
		AvailableVariablesList {				name: "allVariablesList" }
		AssignedVariablesList {					name: "factor";		title: qsTr("Factor");			singleVariable: true; suggestedColumns: ["ordinal", "nominal"]	}
        AssignedVariablesList {					name: "counts";		title: qsTr("Counts");			singleVariable: true; suggestedColumns: ["scale", "ordinal"]	}
        AssignedVariablesList {	id: exProbVar;	name: "exProbVar";	title: qsTr("Expected Counts"); singleVariable: true; suggestedColumns: ["scale", "ordinal"]	}
	}

	RadioButtonGroup
	{
		id		: hypothesisGroup
		name	: "hypothesis"
		title	: qsTr("Test Values")
		enabled	: exProbVar.count == 0

		Layout.columnSpan: 2

		RadioButton {	value: "multinomialTest";	label: qsTr("Equal proportions");	 checked: true				}
		RadioButton {	value: "expectedProbs";		label: qsTr("Expected proportions"); id: expectedProbs			}

		Chi2TestTableView
		{
			name	: "tableWidget"
			width	: form.availableWidth - hypothesisGroup.leftPadding
			visible	: expectedProbs.checked
			source	: "factor"
			maxNumHypotheses	: 5
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
				name	: "descriptives"
				label	: qsTr("Descriptives")
				CheckBox
				{
					name				: "credibleInterval"; label: qsTr("Credible interval")
					childrenOnSameRow	: true

					CIField { name: "credibleIntervalInterval" }
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

			RadioButton { value: "descCounts";	label: qsTr("Counts");	checked: true	}
			RadioButton { value: "descProps";	label: qsTr("Proportions")				}
		}

		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "descriptivesPlot"
				label	: qsTr("Descriptives plot")

				CIField { name: "descriptivesPlotCredibleInterval"; label: qsTr("Credible interval") }
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
