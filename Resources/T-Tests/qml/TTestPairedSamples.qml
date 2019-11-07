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
		height: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }
		AssignedVariablesList { name: "pairs"; title: qsTr("Variable pairs"); suggestedColumns: ["scale"]; listViewType: "Pairs" }
	}
	
	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "students";			label: qsTr("Student"); checked: true	}
		CheckBox { name: "wilcoxonSignedRank";	label: qsTr("Wilcoxon signed-rank")		}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
			name: "meanDifference";	label: qsTr("Location parameter")
			CheckBox
			{
				name: "meanDiffConfidenceIntervalCheckbox";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "meanDiffConfidenceIntervalPercent" }
			}
		}

		CheckBox
		{
			name: "effectSize";	label: qsTr("Effect Size")
			CheckBox
			{
				name: "effSizeConfidenceIntervalCheckbox";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "effSizeConfidenceIntervalPercent" }
			}
		}
		CheckBox { name: "descriptives";					label: qsTr("Descriptives")											}
		CheckBox
		{
			name: "descriptivesPlots";						label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotsConfidenceInterval";	label: qsTr("Confidence interval") }
		}
		CheckBox { name: "VovkSellkeMPR";					label: qsTr("Vovk-Sellke maximum p-ratio")						}
	}

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "groupsNotEqual";	label: qsTr("Measure 1 ≠ Measure 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	label: qsTr("Measure 1 > Measure 2");				}
		RadioButton { value: "groupTwoGreater";	label: qsTr("Measure 1 < Measure 2");				}
	}

	Group
	{
		title: qsTr("Assumption Checks")
		CheckBox { name: "normalityTests";		label: qsTr("Normality") }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";			label: qsTr("Exclude cases analysis by analysis"); checked: true		}
		RadioButton { value: "excludeListwise";						label: qsTr("Exclude cases listwise")								}
	}

}
