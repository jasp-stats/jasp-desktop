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
	id: form
	
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
				CheckBox {  text: qsTr("Student")                   ; name: "students"          ; checked: true}
				CheckBox {  text: qsTr("Welch")                     ; name: "welchs"  }
				CheckBox {  text: qsTr("Mann-Whitney")              ; name: "mannWhitneyU"  }
			}
			
			RadioButtonGroup
			{
				name: "hypothesis"
				title: qsTr("Alt. Hypothesis")
				RadioButton {   text: qsTr("Group 1 ≠ Group 2") ; name: "groupsNotEqual"       ; checked: true}
				RadioButton {   text: qsTr("Group 1 > Group 2") ; name: "groupOneGreater"    }
				RadioButton {   text: qsTr("Group 1 < Group 2") ; name: "groupTwoGreater"    }
			}
			
			GroupBox
			{
				title: qsTr("Assumption checks")
				CheckBox {  text: qsTr("Normality")                 ; name: "normalityTests"   }
				CheckBox {  text: qsTr("Equality of variances")     ; name: "equalityOfVariancesTests"   }
			}
		}
		
		ColumnLayout
		{
			spacing: 15
			GroupBox
			{
				title: qsTr("Additional Statistics")
				CheckBox {  text: qsTr("Location parameter")                        ; name: "meanDifference"; id: locationParameter }
				Row
				{
					Layout.leftMargin: Theme.indentationLength
					enabled : locationParameter.checked
					CheckBox {  text: qsTr("Confidence interval")                   ; name: "meanDiffConfidenceIntervalCheckbox"; id: locParConfidenceInterval }
					PercentField { enabled: locParConfidenceInterval.checked        ; name: "descriptivesMeanDiffConfidenceIntervalPercent"  ; defaultValue: 95 }
				}
				CheckBox {  text: qsTr("Effect Size")                               ; name: "effectSize"; id: effectSize }
				Row
				{
					Layout.leftMargin: Theme.indentationLength
					enabled : effectSize.checked
					CheckBox {  text: qsTr("Confidence interval")                   ; name: "effSizeConfidenceIntervalCheckbox"; id: effectSizeConfidenceInterval }
					PercentField { enabled: effectSizeConfidenceInterval.checked    ; name: "descriptivesEffectSizeConfidenceIntervalPercent" ; defaultValue: 95 }
				}
				CheckBox {  text: qsTr("Descriptives")                              ; name: "descriptives"                        }
				CheckBox {  text: qsTr("Descriptives plots")                        ; name: "descriptivesPlots"; id: descriptivePlots  }
				PercentField { text: qsTr("Confidence interval")                    ; name: "descriptivesPlotsConfidenceInterval"; defaultValue: 95; indent: true; enabled: descriptivePlots.checked}
				CheckBox {  text: qsTr("Vovk-Sellke mazimum p-ratio")               ; name: "VovkSellkeMPR"                        }
			}
			
			RadioButtonGroup
			{
				name: "missingValues"
				title: qsTr("Missing Values")
				RadioButton {   text: qsTr("Exclude cases analysis by analysis")    ; name: "excludeAnalysisByAnalysis" ; checked: true }
				RadioButton {   text: qsTr("Exclude cases listwise")                ; name: "excludeListwise"    }
			}
		}
	}
	
}
