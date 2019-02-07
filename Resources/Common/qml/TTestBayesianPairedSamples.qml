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
		AssignedVariablesList { name: "pairs"; title: qsTr("Variables"); allowedColumns: ["scale"]; listViewType: "AssignedPairs" }
	}
	
	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Hypothesis")
		RadioButton { value: "groupsNotEqual";	text: qsTr("Measure 1 ≠ Measure 2"); checked: true	}
		RadioButton { value: "groupOneGreater";	text: qsTr("Measure 1 > Measure 2");				}
		RadioButton { value: "groupTwoGreater";	text: qsTr("Measure 1 < Measure 2");				}
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

	Group
	{
		title: qsTr("Additional Statistics")
		CheckBox { name: "descriptives"; text: qsTr("Descriptives") }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	text: qsTr("Exclude cases analysis by analysis"); checked: true	}
		RadioButton { value: "excludeListwise";				text: qsTr("Exclude cases listwise")							}
	}

	SubjectivePriors { }
	
}
