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
		height						: 260 * preferencesModel.uiScale
		marginBetweenVariablesLists	: 15

		AvailableVariablesList	{ name: "allVariablesList" }
        AssignedVariablesList	{ name: "y1";	title: qsTr("Successes Group 1");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
        AssignedVariablesList	{ name: "n1";	title: qsTr("Sample Size Group 1");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
        AssignedVariablesList	{ name: "y2";	title: qsTr("Successes Group 2");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
        AssignedVariablesList	{ name: "n2";	title: qsTr("Sample Size Group 2");	singleVariable: true;	suggestedColumns: ["scale", "ordinal"] }
	}

	ColumnLayout
	{
		BayesFactorType { }

		Group
		{
			title	: qsTr("Normal Prior on Log Odds Ratio")

			DoubleField { label: qsTr("μ:"); name: "normal_mu";		defaultValue: 0 }
			DoubleField { label: qsTr("σ:"); name: "normal_sigma";	defaultValue: 1 }
		}

		CheckBox
		{
			name	: "descriptives";
			label	: qsTr("Descriptives")
		}
	}

	ColumnLayout
	{
		Group
		{
			title	: qsTr("Plots")
			CheckBox
			{
				name	: "plotPriorAndPosterior"
				label	: qsTr("Prior and posterior")
				childrenOnSameRow: true

				DropDown
				{
					id: plotPosteriorType
					name: "plotPosteriorType"
					values: [ "LogOddsRatio", "OddsRatio", "RelativeRisk", "AbsoluteRisk", "p1&p2" ]
				}

			}
			CheckBox
			{
				name	: "plotSequentialAnalysis"
				label	: qsTr("Sequential analysis")
			}
			CheckBox
			{
				name	: "plotPriorOnly"
				label	: qsTr("Prior")
				childrenOnSameRow: true

				DropDown
				{
					id: plotPriorType
					name: "plotPriorType"
					values: [ "LogOddsRatio", "OddsRatio", "RelativeRisk", "AbsoluteRisk", "p1&p2", "p1", "p2" ]
				}
			}
		}

		RadioButtonGroup
		{
			name	: "bayesFactorOrder"
			title	: qsTr("Order")
			RadioButton { value: "bestModelTop";	label: qsTr("Compare to best model");	checked: true	}
			RadioButton { value: "nullModelTop";	label: qsTr("Compare to null model")	}
		}
	}


	Section
	{
		title	: qsTr("Advanced Options")

		ColumnLayout
		{
			Group
			{
				title: qsTr("Prior Model Probability")
				DoubleField { name: "orEqualTo1Prob";		label: qsTr("Log odds ratio = 0"); defaultValue: 0.5;  max: 1; min: 0; decimals: 3 }
				DoubleField { name: "orGreaterThan1Prob";	label: qsTr("Log odds ratio > 0"); defaultValue: 0.25; max: 1; min: 0; decimals: 3 }
				DoubleField { name: "orLessThan1Prob";		label: qsTr("Log odds ratio < 0"); defaultValue: 0.25; max: 1; min: 0; decimals: 3 }
				DoubleField { name: "orNotEqualTo1Prob";	label: qsTr("Log odds ratio ≠ 0"); defaultValue: 0;    max: 1; min: 0; decimals: 3 }
			}
		}

		ColumnLayout
		{
			Group
			{
				title: qsTr("Sampling")
				IntegerField { name: "numSamples"; label: qsTr("No. samples"); defaultValue: 10000; min: 100; fieldWidth: 50; }
			}
		}
	}
}
