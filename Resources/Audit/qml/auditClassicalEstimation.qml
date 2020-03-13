
// Copyright (C) 2013-2018 University of Amsterdam
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

// When making changes to this file always mention @koenderks as a
// reviewer in the Pull Request

import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form {

	usesJaspResults: true

	GridLayout
	{
		columns: 2

		GroupBox
		{
			title: qsTr("Population")

			IntegerField
			{
				id: 			populationSize
				name: 			"populationSize"
				text: 			qsTr("Size")
				fieldWidth: 	100
				defaultValue: 	0
				min: 			0
			}

			DoubleField
			{
				id: 			populationValue
				name: 			"populationValue"
				text: 			qsTr("Value")
				defaultValue: 	0
				fieldWidth: 	100
				min: 			0
				decimals: 		2
				enabled:		!mpu.checked
			}
		}

		GroupBox
		{
			id: 		auditRisk
			title: 		qsTr("Audit Risk")

			PercentField
			{
				name: 			"confidence"
				label: 			qsTr("Confidence")
				decimals: 		2
				defaultValue: 	95
			}
		}
	}

	Divider { }

	VariablesForm
	{
		id: 			variablesFormEstimation
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

		AvailableVariablesList	{ name: "variablesFormEstimation" }

		AssignedVariablesList
		{
			id: 			bookValues
			name: 			"bookValues"
			title: 			qsTr("Book values")
			singleVariable:	true
			allowedColumns:	["scale"]
			enabled: 		!mpu.checked
		}

		AssignedVariablesList
		{
			id: 			auditValues
			name: 			"auditValues"
			title: 			qsTr("Audit values")
			singleVariable: true
			allowedColumns: ["scale"]
		}
	}

	Section
	{
		text: qsTr("Advanced Options")

		GridLayout
		{
			columns: 3

			RadioButtonGroup
			{
				id: 		ir
				title: 		qsTr("Estimator")
				name: 		"estimator"

				RadioButton
				{
					id:			mpu
					text: 		qsTr("Direct")
					name: 		"mpu"
				}

				RadioButton
				{
					text: 		qsTr("Difference")
					name: 		"difference"
				}

				RadioButton
				{
					text:	 	qsTr("Ratio")
					name: 		"ratio"
				}

				RadioButton
				{
					text:	 	qsTr("Regression")
					name: 		"regression"
					checked:	true
				}
			}

			GroupBox
			{
				title: qsTr("Explanatory Text")

				RowLayout
				{
					CheckBox
					{
						id: 		explanatoryText
						text:	 	qsTr("Enable")
						name: 		"explanatoryText"
						checked: 	true
					}

					HelpButton
					{
						helpPage:			"Audit/explanatoryText"
						toolTip: 			qsTr("Show explanatory text and formulas")
					}
				}

				CheckBox
				{
					text:	 		qsTr("Report badges")
					name: 		"reportBadges"
					checked: 	false
					visible: 	false
				}
			}
		}
	}

	Section {
		title: qsTr("Tables and Plots")

		GroupBox
		{
			title: qsTr("Tables")

			CheckBox
			{
				id: requiredSampleSize
				text: qsTr("Required sample size")
				name: "requiredSampleSizeTable"

				DoubleField
				{
					name: 			"requiredUncertainty"
					visible: 		requiredSampleSize.checked
					text: 			qsTr("for an uncertainty of:")
					defaultValue: 	100000
					fieldWidth: 	100
					min: 			0
					decimals: 		2
				}
			}
		}

		GroupBox
		{
			title: qsTr("Plots")

			CheckBox
			{
				text: qsTr("Correlation plot")
				name: "correlationPlot"
				enabled: !mpu.checked
			}
		}
	}

	Item
	{
		Layout.preferredHeight: downloadReportEstimation.height
		Layout.fillWidth: 			true

		Button
		{
			id: 									downloadReportEstimation
			enabled: 							populationSize.value != 0 && populationValue.value != 0 && auditValues.count > 0 && bookValues.count > 0
			anchors.right: 				parent.right
			anchors.bottom: 			parent.bottom
			text: 								qsTr("<b>Download Report</b>")
			onClicked:
			{
				form.exportResults()
			}
		}
	}
}
