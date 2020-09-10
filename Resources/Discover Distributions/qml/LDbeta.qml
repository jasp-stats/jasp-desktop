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
			Layout.columnSpan: 2
			title: "Parameters"
			Group
			{
				columns: 2
				Text { text: qsTr("Shape:") }
				DoubleField{ name: "alpha"; label: qsTr("α"); id: alpha; negativeValues: false; defaultValue: 1 }
				Text { text: qsTr("Shape:") }
				DoubleField{ name: "beta";  label: qsTr("β"); id: beta; negativeValues: false; defaultValue: 1 }
			}

		}

		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments" }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox{ label: qsTr("Probability density function"); id: plotPDF; name: "plotPDF"; checked: true }
			CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCDF; name: "plotCDF"; checked: false }
			CheckBox{ label: qsTr("Quantile function"); id: plotQF; name: "plotQF"; checked: false }
		}

		LD.LDOptions
		{
			enabled				: plotPDF.checked || plotCDF.checked
			min					: 0
			max					: 1
			intervalLowerMax	: .5
			intervalUpperMin	: .5
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName	: "Beta"
		formula				: "α = " + alpha.value + ", β = " + beta.value
		enabled				: mainWindow.dataAvailable
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
			CheckBox{ name: "estPDF"; label: qsTr("Histogram vs. theoretical pdf") }
			CheckBox{ name: "qqplot"; label: qsTr("Q-Q plot")                      }
			CheckBox{ name: "estCDF"; label: qsTr("Empirical vs. theoretical cdf") }
			CheckBox{ name: "ppplot"; label: qsTr("P-P plot")                      }
		}

		Group
		{
			title: qsTr("Statistics")
			CheckBox{ name: "kolmogorovSmirnov";  label: qsTr("Kolmogorov-Smirnov")}
			CheckBox{ name: "cramerVonMisses";    label: qsTr("Cramér–von Mises")  }
			CheckBox{ name: "andersonDarling";    label: qsTr("Anderson-Darling")  }
			//CheckBox{ name: "shapiroWilk";        label: qsTr("Shapiro-Wilk")      }
		}

	}
}
