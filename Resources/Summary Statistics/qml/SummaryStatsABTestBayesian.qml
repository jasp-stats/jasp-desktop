//
// Copyright (C) 2013-2020 University of Amsterdam
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
import JASP 1.0


Form
{
	usesJaspResults: true

	Group
	{
		title: qsTr("Group 1")
		IntegerField { name: "y1";	label: qsTr("Successes")	}
		IntegerField { name: "n1";	label: qsTr("Sample Size")	}
	}

	Group
	{
		title: qsTr("Group 2")
		IntegerField { name: "y2";	label: qsTr("Successes")	}
		IntegerField { name: "n2";	label: qsTr("Sample Size")	}
	}

	Divider { }

	ColumnLayout
	{
		BayesFactorType { id: bayesFactorType }

		Group
		{
			title	: qsTr("Normal Prior on Log Odds Ratio")

			DoubleField { label: qsTr("\u03bc:"); name: "normal_mu";		defaultValue: 0;	negativeValues: true}
			DoubleField { label: qsTr("\u03c3:"); name: "normal_sigma";	defaultValue: 1 }
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

			CheckBox
			{
				name				: "plotRobustness"
				label				: qsTr("Bayes factor robustness check")
				childrenOnSameRow	: true

				DropDown
				{
					id		: plotRobustnessBFType
					name	: "plotRobustnessBFType"
					values	: bayesFactorType.value == "BF01" ? ['BF01', 'BF0+', 'BF0-'] : ['BF10', 'BF+0', 'BF-0']
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
				DoubleField { name: "orNotEqualTo1Prob";	label: qsTr("Log odds ratio \u2260 0"); defaultValue: 0;    max: 1; min: 0; decimals: 3 }
			}

			Group
			{
				title: qsTr("Sampling")
				IntegerField { name: "numSamples"; label: qsTr("No. samples"); defaultValue: 10000; min: 100; fieldWidth: 50; }
			}

			SetSeed {}
		}

		ColumnLayout
		{
			Group
			{
				title	: qsTr("Robustness Plot")

				Group
				{
					title	: qsTr("No. Steps")
					IntegerField { label: qsTr("\u03bc:"); name: "mu_stepsize";	defaultValue: 5; min: 3 }
					IntegerField { label: qsTr("\u03c3:"); name: "sigma_stepsize";	defaultValue: 5; min: 3 }
				}

				Group
				{
					title	: qsTr("Step Range")
					columns : 3

					Label { text: "\u03bc:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter }
					DoubleField
					{
						id				: muLower
						label			: qsTr("lower:")
						name			: "mu_stepsize_lower"
						defaultValue	: plotRobustnessBFType.currentText == "BF+0" ? 0 : -0.5
						max				: muUpper.value
						negativeValues	: true
						inclusive		: JASP.None

					}
					DoubleField
					{
						id				: muUpper
						label			: qsTr("upper:")
						name			: "mu_stepsize_upper"
						defaultValue	: plotRobustnessBFType.currentText == "BF-0" ? 0 : 0.5
						min				: muLower.value
						negativeValues	: true
						inclusive		: JASP.None
					}

					Label { text: "\u03c3:"; Layout.fillHeight: true; verticalAlignment: Text.AlignVCenter }
					DoubleField
					{
						id				: sigmaLower
						label			: qsTr("lower:")
						name			: "sigma_stepsize_lower"
						defaultValue	: 0.1
						max				: sigmaUpper.value
						negativeValues	: false
						inclusive		: JASP.None
					}
					DoubleField
					{
						id				: sigmaUpper
						label			: qsTr("upper:")
						name			: "sigma_stepsize_upper"
						defaultValue	: 1.0
						min				: sigmaLower.value
						negativeValues	: false
						inclusive		: JASP.None
					}
				}
			}
		}
	}
}
