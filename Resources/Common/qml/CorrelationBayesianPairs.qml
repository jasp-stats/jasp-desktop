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
	usesJaspResults: false
	
	VariablesForm
	{
		AssignedVariablesList { name: "pairs"; allowedColumns: ["scale"]; listViewType: "AssignedPairs" }
	}
	
	GridLayout
	{
		ColumnLayout
		{
			RadioButtonGroup
			{
				name: "corcoefficient"
				title: qsTr("Correlation Coefficients")
				RadioButton { value: "Pearson";	text: qsTr("Pearson's rho"); checked: true	}
				RadioButton { value: "Kendall";	text: qsTr("Kendall's tau-b")				}
			}
			
			RadioButtonGroup
			{
				name: "hypothesis"
				title: qsTr("Hypothesis")
				RadioButton { value: "correlated";				text: qsTr("Correlated"); checked: true	}
				RadioButton { value: "correlatedPositively";	text: qsTr("Correlated positively")		}
				RadioButton { value: "correlatedNegatively";	text: qsTr("Correlated negatively")		}
			}
			
			BayesFactorType {}
			
			RadioButtonGroup
			{
				name: "missingValues"
				title: qsTr("Missing Values")
				RadioButton { value: "excludeAnalysisByAnalysis";	text: qsTr("Exclude cases analysis by analysis"); checked: true	}
				RadioButton { value: "excludeListwise";				text: qsTr("Exclude cases listwise")							}
			}
			
		}
		
		ColumnLayout
		{
			GroupBox
			{
				CheckBox { name: "credibleInterval";	text: qsTr("Credible intervals") ;  id: credibleInterval }
				PercentField { name: "ciValue";			text: qsTr("Interval"); defaultValue: 95; enabled: credibleInterval.checked; indent: true; debug: true }
			}
			
			GroupBox
			{
				title: qsTr("Plots")
				CheckBox { name: "plotScatter";							text: qsTr("Scatterplot")										}
				CheckBox { name: "plotPriorAndPosterior";				text: qsTr("Prior and posterior"); id: plotPriorAndPosterior	}
				CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";	text: qsTr("Additional info"); enabled: plotPriorAndPosterior.checked; indent: true }
				CheckBox { name: "plotBayesFactorRobustness";			text: qsTr("Bayes factor robustness check"); id: plotBayesFactorRobustness }
				CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo"; text: qsTr("Additional info"); enabled: plotBayesFactorRobustness.checked; indent: true }
				CheckBox { name: "plotSequentialAnalysis";				text: qsTr("Sequential analysis"); id: plotSequentialAnalysis }
				CheckBox { name: "plotSequentialAnalysisRobustness";	text: qsTr("Robustness check"); enabled: plotSequentialAnalysis.checked; indent: true; debug: true }
			}
			
			GroupBox
			{
				title: qsTr("Prior")
				DoubleField { name: "priorWidth"; text: qsTr("Stretched beta prior width"); defaultValue: 1.0; doubleValidator { top: 2; decimals: 1 } }
			}
		}
		
	}
	
}
