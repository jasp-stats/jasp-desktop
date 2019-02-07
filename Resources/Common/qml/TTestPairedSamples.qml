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
	plotHeight: 300
	plotWidth:  350
	
	CheckBox { name: "welchs"; checked: false; visible: false }
	
	VariablesForm
	{
		height: 200
		AssignedVariablesList { name: "pairs"; title: qsTr("Variables"); allowedColumns: ["scale"]; listViewType: "AssignedPairs" }
	}
	
	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "students";			text: qsTr("Student"); checked: true	}
		CheckBox { name: "wilcoxonSignedRank";	text: qsTr("Wilcoxon signed-rank")		}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference";	text: qsTr("Location parameter")
			CheckBox
			{
				name: "meanDiffConfidenceIntervalCheckbox";	text: qsTr("Confidence interval")
				childrenOnSameRow: true
				PercentField { name: "meanDiffConfidenceIntervalPercent"; defaultValue: 95 }
			}
		}

		CheckBox
		{
			name: "effectSize";	text: qsTr("Effect Size")
			CheckBox
			{
				name: "effSizeConfidenceIntervalCheckbox";	text: qsTr("Confidence interval")
				childrenOnSameRow: true
				PercentField { name: "effSizeConfidenceIntervalPercent"; defaultValue: 95 }
			}
		}
		CheckBox { name: "descriptives";					text: qsTr("Descriptives")											}
		CheckBox
		{
			name: "descriptivesPlots";						text: qsTr("Descriptives plots")
			PercentField { name: "descriptivesPlotsConfidenceInterval";	text: qsTr("Confidence interval"); defaultValue: 95 }
		}
		CheckBox { name: "VovkSellkeMPR";					text: qsTr("Vovk-Sellke mazimum p-ratio")						}
	}

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "groupsNotEqual";	text: qsTr("Measure 1 ≠ Measure 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	text: qsTr("Measure 1 > Measure 2");				}
		RadioButton { value: "groupTwoGreater";	text: qsTr("Measure 1 < Measure 2");				}
	}

	Group
	{
		title: qsTr("Assumption checks")
		CheckBox { name: "normalityTests";		text: qsTr("Normality") }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";			text: qsTr("Exclude cases analysis by analysis"); checked: true		}
		RadioButton { value: "excludeListwise";						text: qsTr("Exclude cases listwise")								}
	}

}
