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
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); suggestedColumns: ["scale"] }
	}
	
	Group
	{
		title: qsTr("Tests")
		CheckBox { name: "students";		label: qsTr("Student"); checked: true	}
		CheckBox { name: "mannWhitneyU";	label: qsTr("Wilcoxon signed-rank")		}
		CheckBox { name: "zTest";			label: qsTr("Z Test"); id: zTest		}
		DoubleField { name: "testValue";	label: qsTr("Test value:");		defaultValue: 0;	negativeValues: true	}
		DoubleField { name: "stddev";		label: qsTr("Std. deviation:");	defaultValue: 1.0;	enabled: zTest.checked	}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		Layout.rowSpan: 2
		CheckBox
		{
            name: "meanDifference";			label: qsTr("Location estimate")
			CheckBox
			{
				name: "meanDiffConfidenceIntervalCheckbox";	label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "meanDiffConfidenceIntervalPercent" }
			}
		}

		CheckBox
		{
			name: "effectSize";				label: qsTr("Effect Size")
			CheckBox
			{
				name: "effSizeConfidenceIntervalCheckbox"; label: qsTr("Confidence interval")
				childrenOnSameRow: true
				CIField { name: "effSizeConfidenceIntervalPercent" }
			}
		}
		CheckBox { name: "descriptives";	label: qsTr("Descriptives") }
		CheckBox
		{
			name: "descriptivesPlots";		label: qsTr("Descriptives plots")
			CIField { name: "descriptivesPlotsConfidenceInterval";	label: qsTr("Confidence interval") }
		}
		CheckBox { name: "VovkSellkeMPR";	label: qsTr("Vovk-Sellke maximum p-ratio") }
	}

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "notEqualToTestValue";		label: qsTr("≠ Test value"); checked: true	}
		RadioButton { value: "greaterThanTestValue";	label: qsTr("> Test value")					}
		RadioButton { value: "lessThanTestValue";		label: qsTr("< Test value")					}
	}

	Group
	{
		title: qsTr("Assumption checks")
		CheckBox { name: "normalityTests"; label: qsTr("Normality") }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	label: qsTr("Exclude cases per dependent variable"); checked: true	}
		RadioButton { value: "excludeListwise";				label: qsTr("Exclude cases listwise")								}
	}
}
