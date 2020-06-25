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

Form {
	plotHeight: 340
	plotWidth:  420
	
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		AvailableVariablesList { name: "allVariablesList" }		
		AssignedVariablesList { name: "variables"; title: qsTr("Dependent Variables"); suggestedColumns: ["scale"] }
		AssignedVariablesList { name: "groupingVariable"; title: qsTr("Grouping Variable"); suggestedColumns: ["ordinal", "nominal"]; singleVariable: true }
	}
	
	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "groupsNotEqual";	label: qsTr("Group 1 ≠ Group 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	label: qsTr("Group 1 > Group 2")					}
		RadioButton { value: "groupTwoGreater";	label: qsTr("Group 1 < Group 2")					}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2

		CheckBox
		{
			name: "plotPriorAndPosterior";		label: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";		label: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "plotBayesFactorRobustness";	label: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			enabled: student.checked && priors.defaultPriorsChecked
			name: "plotSequentialAnalysis";		label: qsTr("Sequential analysis")
			CheckBox { name: "plotSequentialAnalysisRobustness";		label: qsTr("Robustness check") }
		}

		CheckBox
		{
			name: "descriptivesPlots";			label: qsTr("Descriptives")
			CIField { name: "descriptivesPlotsCredibleInterval";	label: qsTr("Credible interval") }
		}
	}

	BayesFactorType { }

	RadioButtonGroup
	{
		name: "testStatistic"
		id: testStatistic
		title: qsTr("Tests")
		RadioButton
		{
			id: student
			value: "Student";	label: qsTr("Student"); checked: true }
		RadioButton
		{
			value: "Wilcoxon";	label: qsTr("Mann-Whitney"); id: testWilcoxon
			IntegerField { name: "wilcoxonSamplesNumber"; label: qsTr("No. samples"); defaultValue: 1000; min: 100; max: 10000; fieldWidth: 60 }
		}
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	label: qsTr("Exclude cases per dependent variable"); checked: true }
		RadioButton { value: "excludeListwise";				label: qsTr("Exclude cases listwise")							}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptives"; label: qsTr("Descriptives") }
	}

	SetSeed{enabled: testWilcoxon.checked}

	SubjectivePriors { id: priors; informedPriorsEnabled: student.checked }
}
