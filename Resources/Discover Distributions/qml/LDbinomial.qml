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
			title: qsTr("Free parameter")
			columns: 2
			Text { text: qsTr("Probability of success:") }
			DoubleField{ name: "prob"; label: qsTr("p"); id: prob; min: 0; max: 1; defaultValue: 0.5 }
		}

		Group
		{
			title: qsTr("Fixed parameter")
			columns: 2
			Text { text: qsTr("Number of trials:") }
			IntegerField{ name: "size"; label: qsTr("n"); id: size; defaultValue: 10 }
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
					defaultValue: parseFloat(size.value); min: parseFloat(min_x.value); max: parseFloat(size.value)
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
						min: parseInt(min.value); defaultValue: parseInt(size.value); max: parseInt(size.value)
					}
				}
			}
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Binomial"
		formula					: "p = " + prob.value + ", n = " + size.value
		histogramIsBarPlot		: true
		allowOnlyScaleColumns	: false
		suggestScaleColumns		: true
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable; distributionType: "counts" }
}
