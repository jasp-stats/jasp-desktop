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


Form
{
	usesJaspResults: true
	plotHeight: 240
	plotWidth:  320
	
	CheckBox { name: "standardizedEffectSize"; checked: true; visible: false }
	
	VariablesForm
	{
		height: 200
		AssignedVariablesList { name: "variables"; title: qsTr("Variables"); allowedColumns: ["scale"] }
	}
	
	DoubleField { name: "testValue"; text: qsTr("Test value:"); defaultValue: 0; validation: false }
	
	GridLayout
	{
		ColumnLayout
		{
			spacing: 15
			
			RadioButtonGroup
			{
				name: "hypothesis"
				title: qsTr("Hypothesis")
				RadioButton { value: "notEqualToTestValue";		text: qsTr("≠ Test value"); checked: true	}
				RadioButton { value: "greaterThanTestValue";	text: qsTr("> Test value");					}
				RadioButton { value: "lessThanTestValue";		text: qsTr("< Test value");					}
			}
			
			BayesFactorType { }
			
			GroupBox
			{
				title: qsTr("Additional Statistics")
				CheckBox { name: "descriptives";	text: qsTr("Descriptives") }
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
