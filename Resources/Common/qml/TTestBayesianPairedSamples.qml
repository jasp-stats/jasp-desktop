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

Form {
	usesJaspResults: true
	plotHeight: 340
	plotWidth:  420
	
	VariablesForm
	{
		height: 200
		AssignedVariablesList { name: "pairs"; title: qsTr("Variables"); allowedColumns: ["scale"]; listViewType: "AssignedPairs" }
	}
	
	GridLayout
	{
		ColumnLayout
		{
			spacing: 15
			
			RadioButtonGroup
			{
				name: "hypothesis"
				title: qsTr("Hypothesis")
				RadioButton { value: "groupsNotEqual";	text: qsTr("Measure 1 ≠ Measure 2"); checked: true	}
				RadioButton { value: "groupOneGreater";	text: qsTr("Measure 1 > Measure 2");				}
				RadioButton { value: "groupTwoGreater";	text: qsTr("Measure 1 < Measure 2");				}
			}
			
			BayesFactorType { }
			
			RadioButtonGroup
			{
				name: "testStatistic"
				title: qsTr("Tests")
				RadioButton { value: "Student";		text: qsTr("Student");		checked: true	}
				RadioButton { value: "Wilcoxon";	text: qsTr("Mann-Whitney");	id: wilcoxon	}
				
				Row
				{
					spacing: 5
					enabled: wilcoxon.checked
					Layout.leftMargin: Theme.indentationLength
					Label     { text: qsTr("No. samples") }
					IntegerField { name: "wilcoxonSamplesNumber"; defaultValue: 1000; intValidator { bottom: 100; top: 10000 } }
				}
			}
			
			GroupBox
			{
				title: qsTr("Assumption checks")
				CheckBox { name: "descriptives"; text: qsTr("Descriptives") }
			}
		}
		
		ColumnLayout
		{
			spacing: 15
			
			GroupBox
			{
				title: qsTr("Plots")
				
				CheckBox     { name: "plotPriorAndPosterior";					text: qsTr("Prior and posterior"); id: plotPriorAndPosterior										}
				CheckBox     { name: "plotPriorAndPosteriorAdditionalInfo";		text: qsTr("Additional info"); indent: true; checked: true; enabled: plotPriorAndPosterior.checked	}
				
				CheckBox     { name: "plotBayesFactorRobustness";				text: qsTr("Bayes factor robustness check"); id: plotBayesFactorRobustness							}
				CheckBox     { name: "plotBayesFactorRobustnessAdditionalInfo";	text: qsTr("Additional info"); indent: true; checked: true; enabled: plotBayesFactorRobustness.checked }
				
				CheckBox     { name: "plotSequentialAnalysis";					text: qsTr("Sequential analysis"); id: plotSequentialAnalysis										}
				CheckBox     { name: "plotSequentialAnalysisRobustness";		text: qsTr("Robustness check"); indent: true; enabled: plotSequentialAnalysis.checked				}
				
				CheckBox     { name: "descriptivesPlots";						text: qsTr("Descriptives plots"); id: descriptivesPlots												}
				PercentField { name: "descriptivesPlotsCredibleInterval";		text: qsTr("Credible interval"); defaultValue: 95; indent: true; enabled: descriptivesPlots.checked	}
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
	
	SubjectivePriors { }
	
}
