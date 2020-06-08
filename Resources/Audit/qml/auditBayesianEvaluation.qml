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

// When making changes to this file always mention @koenderks as a
// reviewer in the Pull Request

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

Form
{
	usesJaspResults: 	true
	columns: 			1

	// Extra options
	CheckBox {name: "priorAndPosteriorPlotExpectedPosterior"; checked: false; visible: false}

	GridLayout
	{
		columns: 3

		RadioButtonGroup
		{
			id: 		materiality
			name: 	"materiality"
			title: 	qsTr("Population Materiality")

			RowLayout
			{
				RadioButton
				{
					id: 								materialityAbsolute
					name: 							"materialityAbsolute"
					text: 							qsTr("Absolute")
					checked: 						mainWindow.dataAvailable
					enabled:						mainWindow.dataAvailable
					childrenOnSameRow: 	true
					onCheckedChanged: 
					{
						if (!variableTypeAuditValues.checked)
							variableTypeAuditValues.checked = true
					}

					DoubleField
					{
						id: 							materialityValue
						visible: 					materialityAbsolute.checked
						name: 						"materialityValue"
						defaultValue: 		0
						min: 							0
						fieldWidth: 			90
						decimals: 				2
						label: 						"$"
					}
				}
			}

			RowLayout
			{
				RadioButton
				{
					id: 								materialityRelative
					name: 							"materialityRelative"
					text: 							qsTr("Relative")
					childrenOnSameRow: 	true
					checked: 						!mainWindow.dataAvailable

					PercentField
					{
						id: 							materialityPercentage
						visible: 					materialityRelative.checked
						decimals: 				2
						defaultValue: 		0
						name: 						"materialityPercentage"
						fieldWidth: 			40
					}
				}
			}
		}

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
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		enabled:					!useSummaryStatistics.checked

		AvailableVariablesList
		{
			id: 		evaluationVariables
			name: 	"evaluationVariables"
		}

		AssignedVariablesList
		{
			id: 				recordNumberVariable
			name: 				"recordNumberVariable"
			title: 				qsTr("Record ID's")
			singleVariable: 	true
			allowedColumns: 	["ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id: 			auditResult
			name: 			"auditResult"
			title: 			qsTr("Audit Result")
			singleVariable: true
			allowedColumns: ["nominal" ,"scale"]
		}

		AssignedVariablesList
		{
			id: 				monetaryVariable
			name: 				"monetaryVariable"
			title: 				variableTypeAuditValues.checked ? qsTr("Book Values <i>(required)</i>") : qsTr("Book Values <i>(optional)</i>")
			singleVariable: 	true
			allowedColumns: 	["scale"]
		}

		AssignedVariablesList
		{
			id: 				sampleCounter
			name: 				"sampleCounter"
			title: 				qsTr("Selection Counter <i>(optional)</i>")
			singleVariable: 	true
			allowedColumns: 	["nominal"]
		}
	}

	RadioButtonGroup {
		id:						variableType 
		title: 				qsTr("Annotation Method")
		name:					"variableType"

		RadioButton {
			id: 				variableTypeAuditValues
			name:				"variableTypeAuditValues"
			label: 			qsTr("Audit values")
			checked:		mainWindow.dataAvailable
			enabled:		mainWindow.dataAvailable
		}

		RadioButton {
			id: 				variableTypeCorrect
			name:				"variableTypeCorrect"
			label: 			qsTr("Correct / Incorrect")	
			enabled:		materialityRelative.checked
			checked: 		!mainWindow.dataAvailable
			onCheckedChanged: 
			{
				if (useSummaryStatistics.checked)
					useSummaryStatistics.checked = false
			}

			CheckBox {
				id: 			useSummaryStatistics
				name: 		"useSumStats"
				label:		qsTr("Use summary statistics")
				checked: 	!mainWindow.dataAvailable

				IntegerField
				{
					id: 						nSumStats
					name: 					"nSumStats"
					text: 					qsTr("Sample size")
					defaultValue: 	0
					min: 						0
					visible:				useSummaryStatistics.checked
				}

				IntegerField
				{
					id: 						kSumStats
					name: 					"kSumStats"
					text: 					qsTr("Found errors")
					defaultValue: 	0
					min: 						0
					visible:				useSummaryStatistics.checked
					max:						nSumStats.value
				}
			}
		}
	}

	Section {
		title: 		qsTr("Prior Information")
		columns: 3

		RadioButtonGroup
		{
			id: 		ir
			title: 		qsTr("Inherent Risk")
			name: 		"IR"

			RadioButton { text: qsTr("High"); 		name: "High"; checked: true	}
			RadioButton { text: qsTr("Medium");		name: "Medium"}
			RadioButton { text: qsTr("Low"); 			name: "Low"}
			RadioButton
			{
				id: 								irCustom
				text:	 							qsTr("Custom")
				name: 							"Custom"
				childrenOnSameRow: 	true

				PercentField
				{
					name: 						"irCustom"
					visible: 					irCustom.checked
					decimals: 				2
					defaultValue: 		100
					min: 							25
				}
			}
		}

		RadioButtonGroup
		{
			id: 		cr
			title: 		qsTr("Control Risk")
			name: 		"CR"

			RadioButton { text: qsTr("High"); 		name: "High"; 	checked: true	}
			RadioButton { text: qsTr("Medium"); 	name: "Medium" 					}
			RadioButton { text: qsTr("Low"); 		name: "Low" 					}
			RadioButton
			{
				id: 							crCustom
				text:	 						qsTr("Custom")
				name: 						"Custom"
				childrenOnSameRow: true

				PercentField
				{
					name: 					"crCustom"
					visible: 				crCustom.checked
					decimals: 			2
					defaultValue: 	100
					min:						25
				}
			}
		}

		RadioButtonGroup
		{
			id: 		expectedErrors
			name: 		"expectedErrors"
			title: 		qsTr("Expected Errors")

			RowLayout
			{
				enabled: mainWindow.dataAvailable

				RadioButton { text: qsTr("Absolute"); name: "expectedAbsolute"; id: expectedAbsolute}

				DoubleField
				{
					name: 			"expectedNumber"
					enabled: 		expectedAbsolute.checked
					defaultValue: 	0
					min: 			0
					max: 			1e10
					decimals: 		2
					visible: 		expectedAbsolute.checked
					fieldWidth: 	60
					label: 			"$"
				}
			}

			RowLayout
			{
				RadioButton { text: qsTr("Relative") ; name: "expectedRelative"; id: expectedRelative; checked: true}

				PercentField
				{
					name: 			"expectedPercentage"
					enabled: 		expectedRelative.checked
					decimals: 		2
					defaultValue: 	0
					visible: 		expectedRelative.checked
					fieldWidth: 	40
				}
			}
		}

	}

	Section
	{
		title: 		qsTr("Advanced Options");
		columns: 	1

		GridLayout
		{
			columns: 3

			RadioButtonGroup
			{
				title: 		qsTr("Estimation Method")
				name: 		"estimator"

				RadioButton
				{
					id: 			betaBound
					name: 		"betaBound"
					text: 		qsTr("Beta")
				}

				RadioButton
				{
					id: 			gammaBound
					name: 		"gammaBound"
					text: 		qsTr("Gamma")
				}

				RadioButton
				{
					id: 			betabinomialBound
					name: 		"betabinomialBound"
					text: 		qsTr("Beta-binomial")
					enabled: 	variableTypeCorrect.checked
				}
			}

			RadioButtonGroup
			{
				title: 		qsTr("Area Under Posterior")
				name: 		"areaUnderPosterior"

				RadioButton
				{
					text: 	qsTr("Credible bound")
					name: 	"displayCredibleBound"
				}

				RadioButton
				{
					text: 	qsTr("Credible interval")
					name: 	"displayCredibleInterval"
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
			}
		}
	}

	Section
	{
		title: qsTr("Tables and Plots")

		GridLayout
		{
			columns: 2

			ColumnLayout
			{

				GroupBox
				{
					title: qsTr("Statistics")

					CheckBox
					{
						text: 	qsTr("Most likely error (MLE)")
						name: 	"mostLikelyError"
					}

					CheckBox
					{
						text: 		qsTr("Evidence ratio")
						name: 		"evidenceRatio"
					}

					CheckBox
					{
						text: qsTr("Bayes factor (BF\u208B\u208A)")
						name: "bayesFactor"
					}
				}

				GroupBox
				{
					title: qsTr("Tables")

					CheckBox
					{
						text: 	qsTr("Prior and posterior descriptives")
						name: 	"priorAndPosteriorStatistics"
					}
				}
			}

			GroupBox
			{
				title: qsTr("Plots")

				CheckBox
				{
					text: 	qsTr("Evaluation information")
					name: 	"evaluationInformation"
				}

				CheckBox
				{
					text: 		qsTr("Correlation plot")
					name: 		"correlationPlot"
					enabled: 	variableTypeAuditValues.checked
				}

				CheckBox
				{
					id: 							priorAndPosteriorPlot
					text: 						qsTr("Prior and posterior")
					name: 						"priorAndPosteriorPlot"

					PercentField
					{
						id: 						priorAndPosteriorPlotLimit
						text: 					qsTr("x-axis limit")
						defaultValue: 	20
						name: 					"priorAndPosteriorPlotLimit"
					}

					CheckBox
					{
						id: 						priorAndPosteriorPlotAdditionalInfo
						text: 					qsTr("Additional info")
						name: 					"priorAndPosteriorPlotAdditionalInfo"
						checked: 				true

						RadioButtonGroup 
						{
							title: 				qsTr("Shade")
							name: 				"shadePosterior"

							RadioButton
							{
								text: 			qsTr("Credible region")
								name: 			"shadePosteriorCredibleRegion"
								checked: 		true
							}

							RadioButton
							{
								text: 			qsTr("Support regions")
								name: 			"shadePosteriorHypotheses"
							}

							RadioButton
							{
								text: 			qsTr("None")
								name: 			"shadePosteriorNone"
							}
						}
					}
				}
			}
		}
	}

	Item
	{
		Layout.preferredHeight: toInterpretation.height
		Layout.fillWidth: 			true

		Button
		{
			id: 			toInterpretation
			anchors.right:	parent.right
			text:			qsTr("<b>Download Report</b>")
			enabled: 		auditResult.count > 0
			onClicked:
			{
				evaluationPhase.expanded = false
				form.exportResults()
			}
		}
	}
}
