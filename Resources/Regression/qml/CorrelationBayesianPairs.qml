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
		AvailableVariablesList { name: "allVariablesList" }		
        AssignedVariablesList { name: "pairs"; suggestedColumns: ["ordinal", "scale"]; listViewType: "Pairs" }
	}
	
	RadioButtonGroup
	{
		name: "corcoefficient"
		title: qsTr("Correlation Coefficients")
		RadioButton { value: "Pearson";	label: qsTr("Pearson's rho"); checked: true	}
		RadioButton { value: "Kendall";	label: qsTr("Kendall's tau-b")				}
	}


	Group
	{
		title: qsTr("Additional Options")
		CheckBox
		{
			name: "credibleInterval"; label: qsTr("Credible intervals")
			CIField { name: "ciValue";	label: qsTr("Interval"); debug: true }
		}
	}

	RadioButtonGroup
	{
		name: "hypothesis"
		title: qsTr("Alt. Hypothesis")
		RadioButton { value: "correlated";				label: qsTr("Correlated"); checked: true	}
		RadioButton { value: "correlatedPositively";	label: qsTr("Correlated positively")		}
		RadioButton { value: "correlatedNegatively";	label: qsTr("Correlated negatively")		}
	}

	Group
	{
		title: qsTr("Plots")
		Layout.rowSpan: 2
		CheckBox { name: "plotScatter";				label: qsTr("Scatterplot") }
		CheckBox
		{
			name: "plotPriorAndPosterior";			label: qsTr("Prior and posterior")
            CheckBox { name: "plotPriorAndPosteriorAdditionalInfo";	label: qsTr("Additional info"); checked: true }
		}
		CheckBox
		{
			name: "plotBayesFactorRobustness";		label: qsTr("Bayes factor robustness check")
            CheckBox { name: "plotBayesFactorRobustnessAdditionalInfo"; label: qsTr("Additional info"); checked: true }
		}
		CheckBox
		{
			name: "plotSequentialAnalysis";			label: qsTr("Sequential analysis")
			CheckBox { name: "plotSequentialAnalysisRobustness"; label: qsTr("Robustness check"); debug: true }
		}
	}

	BayesFactorType {}

	Group
	{
		title: qsTr("Prior")
        DoubleField { name: "priorWidth"; label: qsTr("Stretched beta prior width"); defaultValue: 1.0; min: 0.003; max: 2; decimals: 1 }
	}

	RadioButtonGroup
	{
		name: "missingValues"
		title: qsTr("Missing Values")
		RadioButton { value: "excludeAnalysisByAnalysis";	label: qsTr("Exclude cases analysis by analysis"); checked: true	}
		RadioButton { value: "excludeListwise";				label: qsTr("Exclude cases listwise")							}
	}
}
