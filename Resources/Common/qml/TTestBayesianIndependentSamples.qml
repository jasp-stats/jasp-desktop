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
	usesJaspResults: false
	plotHeight: 340
	plotWidth:  420
	
	VariablesForm
	{
		height: 200
		AssignedVariablesList { name: "variables"; title: qsTr("Dependent Variables"); allowedColumns: ["scale"] }
		AssignedVariablesList { name: "groupingVariable"; title: qsTr("Grouping Variable"); allowedColumns: ["ordinal", "nominal"]; singleItem: true }
	}
	
	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Hypothesis")
		RadioButton { value: "groupsNotEqual";	text: qsTr("Group 1 ≠ Group 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	text: qsTr("Group 1 > Group 2")					}
		RadioButton { value: "groupTwoGreater";	text: qsTr("Group 1 < Group 2")					}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2

		CheckBox
		{
			name: "plotPriorAndPosterior";		text: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";		text: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			name: "plotBayesFactorRobustness";	text: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo";	text: qsTr("Additional info"); checked: true }
		}

		CheckBox
		{
			name: "plotSequentialAnalysis";		text: qsTr("Sequential analysis")
			CheckBox { name: "plotSequentialAnalysisRobustness";		text: qsTr("Robustness check") }
		}

		CheckBox
		{
			name: "descriptivesPlots";			text: qsTr("Descriptives plots")
			PercentField { name: "descriptivesPlotsCredibleInterval";	text: qsTr("Credible interval"); defaultValue: 95 }
		}
	}

	BayesFactorType { }

	RadioButtonGroup
	{
		name: "testStatistic"
		title: qsTr("Tests")
		RadioButton
		{
			value: "Student";	text: qsTr("Student"); checked: true }
		RadioButton
		{
			value: "Wilcoxon";	text: qsTr("Mann-Whitney");
			IntegerField { name: "wilcoxonSamplesNumber"; text: qsTr("No. samples"); defaultValue: 1000; min: 100; max: 10000; fieldWidth: 60 }
		}
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	text: qsTr("Exclude cases analysis by analysis"); checked: true }
		RadioButton { value: "excludeListwise";				text: qsTr("Exclude cases listwise")							}
	}

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptives"; text: qsTr("Descriptives") }
	}

	SubjectivePriors { }
}
