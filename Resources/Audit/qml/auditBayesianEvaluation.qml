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

import QtQuick									2.8
import QtQuick.Layouts							1.3
import JASP.Controls							1.0
import JASP.Widgets								1.0

Form
{
	
	columns: 			1

	// Extra options
		CheckBox 
	{ 
		name: 									"workflow"
		checked: 								false
		visible: 								false 
	}

	CheckBox 
	{ 
		name: 									"bayesianAnalysis"
		checked: 								true
		visible: 								false 
	}

	CheckBox 
	{
		name: 									"priorAndPosteriorPlotExpectedPosterior"
		checked: 								false
		visible: 								false
	}

	IntegerField 
	{
		name: 									"sampleSizeIncrease"
		defaultValue: 							1
		visible:								false
	}

	// Start analysis
	GridLayout
	{
		columns: 								3

		GroupBox 
		{
			title: 								qsTr("Sampling Objectives")
			columns:							2

			CheckBox
			{
				id: 							performanceMateriality
				text: 							qsTr("Test against a performance materiality")
				name: 							"performanceMateriality"

				RadioButtonGroup
				{
					id: 						materiality
					name: 						"materiality"

					RowLayout
					{
						visible: 				performanceMateriality.checked
						
						RadioButton
						{
							id: 				materialityRelative
							name: 				"materialityRelative"
							text: 				qsTr("Relative")
							checked:			true
							childrenOnSameRow: 	true

							PercentField
							{
								id: 			materialityPercentage
								visible: 		materialityRelative.checked
								decimals: 		2
								defaultValue: 	0
								name: 			"materialityPercentage"
								fieldWidth: 	40
							}
						}
					}

					RowLayout
					{
						visible: 				performanceMateriality.checked
						
						RadioButton
						{
							id: 				materialityAbsolute
							name: 				"materialityAbsolute"
							text: 				qsTr("Absolute")
							childrenOnSameRow: 	true
							onCheckedChanged:	if(checked & mainWindow.dataAvailable) variableTypeAuditValues.click()

							DoubleField
							{
								id: 			materialityValue
								visible: 		materialityAbsolute.checked
								name: 			"materialityValue"
								defaultValue: 	0
								min: 			0
								fieldWidth: 	90
								decimals: 		2
								label: 			"€"
							}
						}
					}
				}
			}

			HelpButton
			{
				toolTip: 						qsTr("Click to learn more about the performance materiality.")
				helpPage:						"Audit/performanceMateriality"
			}

			CheckBox
			{
				id: 							minimumPrecision
				text: 							qsTr("Obtain a required minimum precision")
				name: 							"minimumPrecision"
			
				PercentField
				{
					id: 						minimumPrecisionPercentage
					name: 						"minimumPrecisionPercentage"
					decimals: 					2
					defaultValue: 				2
					min:						0.5
					max:						99.9
					label: 						qsTr("Relative")
					visible: 					minimumPrecision.checked
				}
			}

			HelpButton
			{
				toolTip: 						qsTr("Click to learn more about the precision.")
				helpPage:						"Audit/minimumPrecision"
			}
		}

		GroupBox
		{
			title: 								qsTr("Population")

			IntegerField
			{
				id: 							populationSize
				name: 							"populationSize"
				text: 							qsTr("Size")
				fieldWidth: 					80
				defaultValue: 					0
				min: 							0
			}

			DoubleField
			{
				id: 							populationValue
				name: 							"populationValue"
				text: 							qsTr("Value")
				defaultValue: 					0
				fieldWidth: 					80
				min: 							0
				decimals: 						2
				onValueChanged:					
				{
												if(populationValue.value == 0) displayPercentages.click()
												if(populationValue.value == 0) separateKnownAndUnknownMisstatement.checked = false
				}
			}
		}

		GroupBox
		{
			id: 								auditRisk
			title: 								qsTr("Audit Risk")

			PercentField
			{
				name: 							"confidence"
				label: 							qsTr("Confidence")
				decimals: 						2
				defaultValue: 					95
			}
		}
	}

	Divider 
	{ 
		width: 									parent.width 
	}

	VariablesForm
	{
		preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight
		enabled:								!useSummaryStatistics.checked & ((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		AvailableVariablesList
		{
			id: 								evaluationVariables
			name: 								"evaluationVariables"
		}

		AssignedVariablesList
		{
			id: 								recordNumberVariable
			name: 								"recordNumberVariable"
			title: 								qsTr("Transaction ID's")
			singleVariable: 					true
			allowedColumns: 					["nominal", "nominalText", "ordinal", "scale"]
		}

		AssignedVariablesList
		{
			id: 								auditResult
			name: 								"auditResult"
			title: 								variableTypeAuditValues.checked ? qsTr("Soll Position") : qsTr("Audit Result")
			singleVariable: 					true
			allowedColumns: 					["nominal", "scale"]
		}

		AssignedVariablesList
		{
			id: 								monetaryVariable
			name: 								"monetaryVariable"
			title: 								variableTypeAuditValues.checked ? qsTr("Ist Position <i>(required)</i>") : qsTr("Ist Position")
			enabled:							variableTypeAuditValues.checked
			singleVariable: 					true
			allowedColumns: 					["scale"]
		}

		AssignedVariablesList
		{
			id: 								sampleCounter
			name: 								"sampleCounter"
			title: 								qsTr("Selection Counter <i>(optional)</i>")
			singleVariable: 					true
			allowedColumns: 					["nominal", "scale"]
		}
	}

	GridLayout
	{
		columns:								2

		RadioButtonGroup 
		{
			id:									variableType 
			title: 								qsTr("Annotation Method")
			name:								"variableType"

			RadioButton 
			{
				id: 							variableTypeAuditValues
				name:							"variableTypeAuditValues"
				label: 							qsTr("Soll values")
				checked:						mainWindow.dataAvailable
				enabled:						mainWindow.dataAvailable
				onCheckedChanged: 				if(checked) useSummaryStatistics.checked = false
			}

			RadioButton 
			{
				id: 							variableTypeCorrect
				name:							"variableTypeCorrect"
				label: 							qsTr("Correct / Incorrect")	
				checked: 						!mainWindow.dataAvailable

				CheckBox 
				{
					id: 						useSummaryStatistics
					name: 						"useSumStats"
					label:						qsTr("Use summary statistics")
					checked: 					!mainWindow.dataAvailable

					IntegerField
					{
						id: 					nSumStats
						name: 					"nSumStats"
						text: 					qsTr("Number of seen transactions")
						defaultValue: 			0
						min: 					0
						visible:				useSummaryStatistics.checked
					}

					IntegerField
					{
						id: 					kSumStats
						name: 					"kSumStats"
						text: 					qsTr("Number of found errors")
						defaultValue: 			0
						min: 					0
						visible:				useSummaryStatistics.checked
						max:					nSumStats.value
					}
				}
			}
		}

		GroupBox
		{
			title: 								qsTr("Explanatory Text")

			RowLayout
			{

				CheckBox
				{
					id: 						explanatoryText
					text:	 					qsTr("Enable")
					name: 						"explanatoryText"
					checked: 					true
				}

				HelpButton
				{
					helpPage:					"Audit/explanatoryText"
					toolTip: 					qsTr("Click to learn more about the explanatory text.")
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("A.     Critical Transactions")
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0
		columns: 								1

		GridLayout
		{
			columns: 							2

			CheckBox
			{
				id: 							flagCriticalTransactions
				name:							"flagCriticalTransactions"
				text:							qsTr("Select critical transactions")
				enabled:						monetaryVariable.count > 0
				checked:						monetaryVariable.count > 0

				CheckBox
				{
					id: 						flagNegativeValues
					name:						"flagNegativeValues"
					text:						qsTr("Negative Ist values")
					enabled:					monetaryVariable.count > 0
					checked:					true
				}
			}

			RadioButtonGroup
			{
				title: 							qsTr("How to handle critical transactions")
				name: 							"handleCriticalTransactions"
				enabled:						flagCriticalTransactions.checked

				RadioButton 
				{ 
					text: 						qsTr("Keep")
					name: 						"inspect"
					checked: 					true	
				}

				RadioButton 
				{ 
					text: 						qsTr("Remove")
					name: 						"remove"
				}
			}
		}
	}

	Section 
	{
		title: 									qsTr("B.     Efficiency Techniques")
		columns: 								1
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		GroupBox
		{
			title: 								qsTr("Efficiency Techniques")
			enabled:							recordNumberVariable.count > 0 & monetaryVariable.count > 0

			RowLayout
			{
				CheckBox
				{
					id: 						separateKnownAndUnknownMisstatement
					text: 						qsTr("Separate known and unknown misstatement")
					name: 						"separateKnownAndUnknownMisstatement"
					onCheckedChanged: 			if(checked) beta.click()
					enabled:					populationSize.value > 0 & populationValue.value > 0 & recordNumberVariable.count > 0 & monetaryVariable.count > 0 & auditResult.count > 0
				}

				HelpButton
				{
					toolTip: 					qsTr("Click to learn more about this efficiency technique.")
					helpPage:					"Audit/separateKnownAndUnknownMisstatement"
				}
			}

			Label
			{
				Layout.leftMargin: 				30 * preferencesModel.uiScale
				text: 							qsTr("<i>Requires additional assumption: The sample taints are homogeneous</i>")
				enabled:						populationSize.value > 0 & populationValue.value > 0 & recordNumberVariable.count > 0 & monetaryVariable.count > 0 & auditResult.count > 0
			}
		}
	}

	Section 
	{
		title: 									qsTr("C.     Prior Information")
		columns: 								3
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RowLayout
		{
			DropDown 
			{
				id: 							priorConstructionMethod
				name: 							"priorConstructionMethod"
				indexDefaultValue: 				0
				label: 							qsTr("Prior information:")
				Layout.columnSpan: 				2
				enabled:						!pasteVariables.checked
				values: 
				[
					{ label: qsTr("None"), 							value: "none"},
					{ label: qsTr("Audit Risk Model"), 				value: "arm"},
					{ label: qsTr("Equal prior probabilities"), 	value: "median"},
					{ label: qsTr("Custom prior probabilities"), 	value: "hypotheses"},
					{ label: qsTr("Earlier sample"), 				value: "sample"},
					{ label: qsTr("Weighted earlier sample"), 		value: "factor"}
				]
			}

			HelpButton
			{
				toolTip: 						qsTr("Click to learn more about the prior information.")
				helpPage:						"Audit/priorInformation"
			}
		}

		RadioButtonGroup
		{
			id: 								expectedErrors
			name: 								"expectedErrors"
			title: 								qsTr("Expected Errors in Sample")

			RowLayout
			{
				RadioButton 
				{ 
					id: 						expectedRelative
					name: 						"expectedRelative"
					text: 						qsTr("Relative")
					checked: 					true
				}

				PercentField
				{
					name: 						"expectedPercentage"
					enabled: 					expectedRelative.checked
					decimals: 					2
					defaultValue: 				0
					visible: 					expectedRelative.checked
					fieldWidth: 				40
				}
			}

			RowLayout
			{
				RadioButton 
				{ 
					id: 						expectedAbsolute
					name: 						"expectedAbsolute"
					text: 						qsTr("Absolute")
					enabled: 					mainWindow.dataAvailable
				}

				DoubleField
				{
					name: 						"expectedNumber"
					enabled: 					expectedAbsolute.checked
					defaultValue: 				0
					min: 						0
					decimals: 					3
					visible: 					expectedAbsolute.checked
					fieldWidth: 				60
					label: 						performanceMateriality.checked & materialityAbsolute.checked ? "€" : ""
				}
			}
		}

		GroupBox
		{
			visible: 							[1].includes(priorConstructionMethod.currentIndex)
			Layout.columnSpan: 					3
			columns: 							3

			RadioButtonGroup
			{
				id: 							ir
				title: 							qsTr("Inherent Risk")
				name: 							"IR"

				RadioButton 
				{ 
					text: 						qsTr("High")
					name: 						"High"
					checked: 					true	
				}

				RadioButton 
				{ 
					text: 						qsTr("Medium")
					name: 						"Medium"
				}

				RadioButton 
				{ 
					text: 						qsTr("Low")
					name: 						"Low"
				}
				
				RadioButton
				{
					id: 						irCustom
					text:	 					qsTr("Custom")
					name: 						"Custom"
					childrenOnSameRow: 			true

					PercentField
					{
						name: 					"irCustom"
						visible: 				irCustom.checked
						decimals: 				2
						defaultValue: 			100
						min: 					25
					}
				}
			}

			RadioButtonGroup
			{
				id: 							cr
				title: 							qsTr("Control Risk")
				name: 							"CR"

				RadioButton 
				{ 
					text: 						qsTr("High")
					name: 						"High"
					checked: 					true	
				}

				RadioButton 
				{ 
					text: 						qsTr("Medium")
					name: 						"Medium"
				}

				RadioButton 
				{ 
					text: 						qsTr("Low")
					name: 						"Low"
				}

				RadioButton
				{
					id: 						crCustom
					text:	 					qsTr("Custom")
					name: 						"Custom"
					childrenOnSameRow: 			true

					PercentField
					{
						name: 					"crCustom"
						visible: 				crCustom.checked
						decimals: 				2
						defaultValue: 			100
						min:					25
					}
				}
			}
		}

		GroupBox
		{
			visible: 							[2].includes(priorConstructionMethod.currentIndex)
			Layout.columnSpan: 					3
			columns: 							1
			enabled:							false

			DoubleField
			{
				name: 							"pHminfixed"
				text: 							qsTr("Prior probability of tolerable misstatement: p(H\u208B) = ")
				defaultValue: 					0.5
			}

			DoubleField
			{
				name: 							"pHplusfixed"
				text: 							qsTr("Prior probability of intolerable misstatement: p(H\u208A) = ")
				defaultValue: 					0.5
			}
		}

		GroupBox
		{
			visible: 							[3].includes(priorConstructionMethod.currentIndex)
			Layout.columnSpan: 					3
			columns: 							1

			DoubleField
			{
				id: 							pHmin
				name: 							"pHmin"
				text: 							qsTr("Prior probability of tolerable misstatement: p(H\u208B) = ")
				defaultValue: 					0.5
				decimals: 						2
				min: 							0
				max: 							1
			}

			DoubleField
			{
				id:								pHplus
				name: 							"pHplus"
				text: 							qsTr("Prior probability of intolerable misstatement: p(H\u208A) = ")
				enabled:						false
				defaultValue: 					0.5
				value:							1 - pHmin.value
				decimals: 						2
			}
		}

		GroupBox
		{
			visible: 							[4, 5].includes(priorConstructionMethod.currentIndex)
			Layout.columnSpan: 					3
			columns: 							1

			IntegerField
			{
				id:								sampleN
				name: 							"sampleN"
				text: 							qsTr("Number of earlier seen transactions")
				defaultValue: 					0
				min: 							0
			}

			DoubleField
			{
				name: 							"sampleK"
				text: 							qsTr("Number of earlier seen errors")
				defaultValue: 					0
				decimals: 						2
				min: 							0
				max:							sampleN.value											
			}

			DoubleField
			{
				name: 							"factor"
				text: 							qsTr("Weighting factor")
				defaultValue: 					0
				decimals: 						2
				min: 							0
				max: 							1
				visible: 						[5].includes(priorConstructionMethod.currentIndex)
			}
		}	
	}

	Section
	{
		title: 									qsTr("D.     Advanced Options")
		columns: 								3
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RadioButtonGroup
		{
			title: 								qsTr("Probability Distribution")
			name: 								"estimator"

			RadioButton
			{
				id: 							betaBound
				name: 							"betaBound"
				text: 							qsTr("Beta")
			}

			RadioButton
			{
				id: 							gammaBound
				name: 							"gammaBound"
				text: 							qsTr("Gamma")
				enabled:						!separateKnownAndUnknownMisstatement.checked
			}

			RadioButton
			{
				id: 							betabinomialBound
				name: 							"betabinomialBound"
				text: 							qsTr("Beta-binomial")
				enabled: 						variableTypeCorrect.checked & !separateKnownAndUnknownMisstatement.checked
			}
		}

		RadioButtonGroup
		{
			title: 								qsTr("Area Under Posterior")
			name: 								"areaUnderPosterior"

			RadioButton
			{
				text: 							qsTr("One-sided upper bound")
				name: 							"displayCredibleBound"
			}

			RadioButton
			{
				text: 							qsTr("Two-sided interval")
				name: 							"displayCredibleInterval"
			}	
		}

		RadioButtonGroup
		{
			title: 								qsTr("Show Results As")
			name: 								"display"

			RadioButton
			{
				text: 							qsTr("Numbers")
				name: 							"displayNumbers"
			}

			RadioButton
			{
				id:								displayPercentages
				text: 							qsTr("Percentages")
				name: 							"displayPercentages"
				checked: 						true
			}

			RadioButton
			{
				text: 							qsTr("Extrapolated amounts")
				name: 							"displayValues"
				enabled:						populationValue.value != 0
			}	
		}
	}

	Section
	{
		title: 									qsTr("E.     Tables and Plots")
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0
		columns:								2

		ColumnLayout
		{

			GroupBox
			{
				title: 							qsTr("Statistics")

				CheckBox
				{
					text: 						qsTr("Most likely error (MLE)")
					name: 						"mostLikelyError"
					checked:					true
				}

				CheckBox
				{
					text: 						qsTr("Obtained precision")
					name: 						"obtainedPrecision"
					checked: 					minimumPrecision.checked
				}

				CheckBox
				{
					text: 						qsTr("Posterior odds")
					name: 						"evidenceRatio"
					enabled:					performanceMateriality.checked
				}

				CheckBox
				{
					text: 						qsTr("Bayes factor (BF\u208B\u208A)")
					name: 						"bayesFactor"
					enabled:					performanceMateriality.checked
				}
			}

			GroupBox
			{
				title: 							qsTr("Tables")

				CheckBox
				{
					text: 						qsTr("Description of prior and posterior distribution")
					name: 						"priorAndPosteriorStatistics"
				}

				CheckBox
				{
					text: 						qsTr("Evaluate post-hoc assumptions")
					name: 						"evaluationAssumptionChecks"
					checked: 					separateKnownAndUnknownMisstatement.checked
					visible: 					separateKnownAndUnknownMisstatement.checked

					CIField 
					{ 
						name: 					"evaluationAssumptionChecksConfidence"
						label: 					qsTr("Confidence interval") 
					}
				}
			}
		}

		GroupBox
		{
			title: 								qsTr("Plots")

			CheckBox
			{
				text: 							qsTr("Evaluate sampling objectives")
				name: 							"evaluationInformation"
			}

			CheckBox
			{
				text: 							qsTr("Scatter plot of Ist and Soll values")
				name: 							"correlationPlot"
				enabled: 						variableTypeAuditValues.checked

				CheckBox
				{
					text: 						qsTr("Display correlation")
					name:						"correlationPlotShowCorrelation"
				}

				CheckBox
				{
					text: 						qsTr("Display transaction ID's")
					name:						"correlationPlotShowIds"
				}
			}

			CheckBox
			{
				id: 							priorAndPosteriorPlot
				text: 							qsTr("Prior and posterior distribution")
				name: 							"priorAndPosteriorPlot"

				PercentField
				{
					id: 						priorAndPosteriorPlotLimit
					text: 						qsTr("x-axis limit")
					defaultValue: 				50
					name: 						"priorAndPosteriorPlotLimit"
				}

				CheckBox
				{
					id: 						priorAndPosteriorPlotIndicateStatistics
					text: 						qsTr("Display inferential statistics")
					name: 						"priorAndPosteriorPlotIndicateStatistics"
				}

				CheckBox
				{
					id: 						priorAndPosteriorPlotAdditionalInfo
					text: 						qsTr("Display additional information")
					name: 						"priorAndPosteriorPlotAdditionalInfo"
					checked: 					true

					RadioButtonGroup 
					{
						title: 					qsTr("Shade")
						name: 					"shadePosterior"

						RadioButton
						{
							text: 				qsTr("Credible region")
							name: 				"shadePosteriorCredibleRegion"
							checked: 			true
						}

						RadioButton
						{
							text: 				qsTr("Support regions")
							name: 				"shadePosteriorHypotheses"
							enabled:			performanceMateriality.checked
						}

						RadioButton
						{
							text: 				qsTr("None")
							name: 				"shadePosteriorNone"
						}
					}
				}
			}
		}
	}

	Item
	{
		Layout.preferredHeight: 				toInterpretation.height
		Layout.fillWidth: 						true

		Button
		{
			id: 								toInterpretation
			anchors.right:						parent.right
			enabled:							((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0
			text:								qsTr("<b>Download Report</b>")
			onClicked:							form.exportResults()
		}
	}
}
