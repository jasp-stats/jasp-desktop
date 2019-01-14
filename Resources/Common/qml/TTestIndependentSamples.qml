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
	usesJaspResults: false
	plotHeight: 300
	plotWidth:  350
	
	VariablesForm
	{
		height: 200
		AssignedVariablesList { name: "variables";			title: qsTr("Variables");			allowedColumns: ["scale"]	}
		AssignedVariablesList { name: "groupingVariable";	title: qsTr("Grouping Variable");	allowedColumns: ["ordinal", "nominal"]; singleItem: true }
	}
	
	GridLayout
	{
		ColumnLayout
		{
			spacing: 15
			GroupBox
			{
				title: qsTr("Tests")
				CheckBox { name: "students";		text: qsTr("Student"); checked: true	}
				CheckBox { name: "welchs";			text: qsTr("Welch")						}
				CheckBox { name: "mannWhitneyU";	text: qsTr("Mann-Whitney")				}
			}
			
			RadioButtonGroup
			{
				name: "hypothesis"
				title: qsTr("Alt. Hypothesis")
				RadioButton { value: "groupsNotEqual";	text: qsTr("Group 1 ≠ Group 2"); checked: true	}
				RadioButton { value: "groupOneGreater";	text: qsTr("Group 1 > Group 2")					}
				RadioButton { value: "groupTwoGreater";	text: qsTr("Group 1 < Group 2")					}
			}
			
			GroupBox
			{
				title: qsTr("Assumption checks")
				CheckBox { name: "normalityTests";				text: qsTr("Normality")					}
				CheckBox { name: "equalityOfVariancesTests";	text: qsTr("Equality of variances")		}
			}
		}
		
		ColumnLayout
		{
			spacing: 15
			GroupBox
			{
				title: qsTr("Additional Statistics")
				CheckBox { name: "meanDifference"; text: qsTr("Location parameter"); id: locationParameter }
				Row
				{
					Layout.leftMargin: Theme.indentationLength
					enabled : locationParameter.checked
					CheckBox { name: "meanDiffConfidenceIntervalCheckbox"; text: qsTr("Confidence interval"); id: locParConfidenceInterval }
					PercentField { name: "descriptivesMeanDiffConfidenceIntervalPercent"; enabled: locParConfidenceInterval.checked; defaultValue: 95 }
				}
				CheckBox { name: "effectSize"; text: qsTr("Effect Size"); id: effectSize }
				Row
				{
					Layout.leftMargin: Theme.indentationLength
					enabled : effectSize.checked
					CheckBox { name: "effSizeConfidenceIntervalCheckbox"; text: qsTr("Confidence interval"); id: effectSizeConfidenceInterval }
					PercentField { name: "descriptivesEffectSizeConfidenceIntervalPercent"; enabled: effectSizeConfidenceInterval.checked; defaultValue: 95 }
				}
				CheckBox { name: "descriptives";		text: qsTr("Descriptives")								}
				CheckBox { name: "descriptivesPlots";	text: qsTr("Descriptives plots"); id: descriptivePlots	}
				PercentField { name: "descriptivesPlotsConfidenceInterval"; text: qsTr("Confidence interval"); defaultValue: 95; indent: true; enabled: descriptivePlots.checked}
				CheckBox { name: "VovkSellkeMPR";		text: qsTr("Vovk-Sellke mazimum p-ratio")				}
			}
			
			RadioButtonGroup
			{
				name: "missingValues"
				title: qsTr("Missing Values")
				RadioButton { value: "excludeAnalysisByAnalysis";	text: qsTr("Exclude cases analysis by analysis"); checked: true	}
				RadioButton { value: "excludeListwise";				text: qsTr("Exclude cases listwise")							}
			}
		}
	}
	
}
