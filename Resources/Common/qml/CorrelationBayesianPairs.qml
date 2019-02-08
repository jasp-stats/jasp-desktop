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
	
	RadioButtonGroup
	{
		name: "corcoefficient"
		title: qsTr("Correlation Coefficients")
		RadioButton { value: "Pearson";	text: qsTr("Pearson's rho"); checked: true	}
		RadioButton { value: "Kendall";	text: qsTr("Kendall's tau-b")				}
	}

	CheckBox
	{
		name: "credibleInterval"; text: qsTr("Credible intervals")
		PercentField { name: "ciValue";	text: qsTr("Interval"); defaultValue: 95; debug: true }
	}

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Hypothesis")
		RadioButton { value: "correlated";				text: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	text: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	text: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2
		CheckBox { name: "plotScatter";				text: qsTr("Scatterplot") }
		CheckBox
		{
			name: "plotPriorAndPosterior";			text: qsTr("Prior and posterior")
			CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";	text: qsTr("Additional info") }
		}
		CheckBox
		{
			name: "plotBayesFactorRobustness";		text: qsTr("Bayes factor robustness check")
			CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo"; text: qsTr("Additional info") }
		}
		CheckBox
		{
			name: "plotSequentialAnalysis";			text: qsTr("Sequential analysis")
			CheckBox { name: "plotSequentialAnalysisRobustness"; text: qsTr("Robustness check"); debug: true }
		}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Prior")
		DoubleField { name: "priorWidth"; text: qsTr("Stretched beta prior width"); defaultValue: 1.0; max: 2; decimals: 1 }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	text: qsTr("Exclude cases analysis by analysis"); checked: true	}
		RadioButton { value: "excludeListwise";				text: qsTr("Exclude cases listwise")							}
	}
}
