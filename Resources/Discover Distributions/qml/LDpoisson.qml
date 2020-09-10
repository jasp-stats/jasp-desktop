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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

import "./common" as LD

Form
{
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		Group
		{
			title: qsTr("Parameter")
			Layout.columnSpan: 2
			columns: 2
			Text { text: qsTr("Rate:") }
			DoubleField{ name: "lambda"; label: qsTr("λ"); id: lambda; min: 0; defaultValue: 1 }
		}
		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments" }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox{ label: qsTr("Probability mass function"); id: plotPMF; name: "plotPMF"; checked: true }
			CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCMF; name: "plotCMF"; checked: false }
		}

		Group
		{
			title: qsTr("Options")
			enabled: plotPMF.checked || plotCMF.checked
			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				DoubleField
				{
					name: "min_x"; label: qsTr("Range of x from"); id: min_x;
					defaultValue: 0; min: 0; max: parseFloat(max_x.value)
				}
				DoubleField
				{
					name: "max_x"; label: qsTr("to"); id: max_x;
					defaultValue: 5; min: parseFloat(min_x.value);
				}
			}

			Group
			{
				title: qsTr("Highlight")
				Group
				{
					columns: 2
					CheckBox{ name: "highlightDensity"; label: qsTr("Mass"); id: highlightDensity }
					CheckBox{ name: "highlightProbability"; label: qsTr("Cumulative Probability"); id: highlightProbability }
				}
				Row
				{
					spacing: jaspTheme.columnGroupSpacing
					IntegerField
					{
						name: "min"; label: qsTr("Interval"); afterLabel: qsTr("≤ X ≤"); id: min;
						negativeValues: false; defaultValue: 0; max: parseInt(max.value)
					}
					IntegerField
					{
						name: "max"; label: ""; id: max;
						min: parseInt(min.value); defaultValue: 5
					}
				}
			}

		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Poisson"
		formula					: "λ = " + lambda.value
		histogramIsBarPlot		: true
		allowOnlyScaleColumns	: false
		suggestScaleColumns		: true
		enabled					: mainWindow.dataAvailable
	}

	Section
	{
		title: qsTr("Estimate Parameters")
		enabled: mainWindow.dataAvailable

		Group
		{
			CheckBox{ name: "methodMLE";      label: qsTr("Maximum likelihood"); visible: true  }
			CheckBox{ name: "methodMoments";  label: qsTr("Method of moments");  visible: false }
			CheckBox{ name: "methodUnbiased"; label: qsTr("Unbiased estimator"); visible: false }
		}

		Group
		{
			title: qsTr("Output")
			CheckBox
			{
				name: "outputEstimates"; label: qsTr("Estimates"); checked: true
				CheckBox{ name: "outputSE"; label: qsTr("Std. error"); checked: false }
				CheckBox
				{
					name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true
					PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95 }
				}
			}

			CheckBox{ name: "outputVarCov"; label: qsTr("Variance-covariance"); checked: false; visible: false }
			CheckBox{ name: "outputCor";    label: qsTr("Correlation"); checked: false; visible: false }
		}
	}
	Section
	{
		title: qsTr("Assess Fit")
		enabled: mainWindow.dataAvailable

		Group
		{
			title: qsTr("Plots")
			columns: 2
			CheckBox{ name: "estPMF"; label: qsTr("Histogram vs. theoretical pmf") }
			CheckBox{ name: "qqplot"; label: qsTr("Q-Q plot")                      }
			CheckBox{ name: "estCDF"; label: qsTr("Empirical vs. theoretical cdf") }
			CheckBox{ name: "ppplot"; label: qsTr("P-P plot")                      }
		}

		Group
		{
			title: qsTr("Statistics")
			//CheckBox{ name: "kolmogorovSmirnov";  label: qsTr("Kolmogorov-Smirnov")}
			//CheckBox{ name: "cramerVonMisses";    label: qsTr("Cramér–von Mises")  }
			//CheckBox{ name: "andersonDarling";    label: qsTr("Anderson-Darling")  }
			//CheckBox{ name: "shapiroWilk";        label: qsTr("Shapiro-Wilk")      }
			CheckBox{ name: "chiSquare"; label: qsTr("Chi-square")}
		}

	}
}
