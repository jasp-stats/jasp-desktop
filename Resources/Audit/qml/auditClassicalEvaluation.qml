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
		checked: 								false
		visible: 								false 
	}

	CheckBox 
	{ 
		name: 									"separateKnownAndUnknownMisstatement"
		checked: 								false
		visible: 								false
	}

	RadioButtonGroup
	{
		name: 									"priorConstructionMethod"
		visible: 								false

		RadioButton
		{
			name: 								"arm"
			checked: 							true
		}
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
												if(populationValue.value == 0) stringerBound.click()
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
		enabled:								!useSummaryStatistics.checked & ((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0))

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
		text: 									qsTr("B.     Risk Assessments")
		columns:								3
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RadioButtonGroup
		{
			id: 								ir
			title: 								qsTr("Inherent Risk")
			name: 								"IR"
			enabled:							performanceMateriality.checked

			RadioButton 
			{ 
				text: 							qsTr("High")
				name: 							"High"
				checked: 						true	
			}

			RadioButton 
			{ 
				text: 							qsTr("Medium")
				name: 							"Medium"
			}

			RadioButton 
			{ 
				text: 							qsTr("Low")
				name: 							"Low"
			}

			RadioButton
			{
				id: 							irCustom
				text:	 						qsTr("Custom")
				name: 							"Custom"
				childrenOnSameRow: 				true

				PercentField
				{
					name: 						"irCustom"
					visible: 					irCustom.checked
					decimals: 					2
					defaultValue: 				100
					min: 						25
				}
			}
		}

		RadioButtonGroup
		{
			id: 								cr
			title: 								qsTr("Control Risk")
			name: 								"CR"
			enabled:							performanceMateriality.checked

			RadioButton 
			{ 
				text: 							qsTr("High")
				name: 							"High"
				checked: 						true	
			}

			RadioButton 
			{ 
				text: 							qsTr("Medium")
				name: 							"Medium"
			}

			RadioButton 
			{ 
				text: 							qsTr("Low")
				name: 							"Low"
			}

			RadioButton
			{
				id: 							crCustom
				text:	 						qsTr("Custom")
				name: 							"Custom"
				childrenOnSameRow: 				true

				PercentField
				{
					name: 						"crCustom"
					visible: 					crCustom.checked
					decimals: 					2
					defaultValue: 				100
					min:						25
				}
			}
		}
	}

	Section
	{
		title: 									qsTr("C.     Advanced Options");
		columns: 								2
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RadioButtonGroup
		{
			title: 								qsTr("Evaluation Method")
			name: 								"estimator"
			visible: 							variableTypeAuditValues.checked

			RadioButton
			{
				id: 							stringerBound
				name: 							"stringerBound"
				text: 							qsTr("Stringer bound")
				enabled: 						variableTypeAuditValues.checked
				checked: 						true

				CheckBox
				{
					id: 						stringerBoundLtaAdjustment
					name: 						"stringerBoundLtaAdjustment"
					text: 						qsTr("LTA adjustment")
					enabled:					variableTypeAuditValues.checked
					checked: 					true
				}
			}

			RadioButton 
			{ 
				name: 							"directBound"		
				text: 							qsTr("Direct estimator") 			
				id: 							directBound		
				enabled: 						!variableTypeCorrect.checked && populationValue.value != 0
			}
			
			RadioButton 
			{ 
				name: 							"differenceBound"
				text: 							qsTr("Difference estimator")
				id: 							differenceBound
				enabled: 						!variableTypeCorrect.checked && populationValue.value != 0
			}

			RadioButton 
			{ 
				name: 							"ratioBound"
				text: 							qsTr("Ratio estimator")
				id: 							ratioBound
				enabled: 						!variableTypeCorrect.checked  && populationValue.value != 0
			}

			RadioButton 
			{ 
				name: 							"regressionBound"
				text: 							qsTr("Regression estimator")
				id: 							regressionBound
				enabled: 						!variableTypeCorrect.checked && populationValue.value != 0
			}
		}

		RadioButtonGroup
		{
			title: 								qsTr("Probability Distribution")
			name: 								"estimator2"
			visible: 							variableTypeCorrect.checked
			
			RadioButton 
			{ 
				name: 							"binomialBound"
				text: 							qsTr("Binomial")
				id: 							binomialBound
				enabled: 						variableTypeCorrect.checked
			}
			
			RadioButton 
			{ 
				name: 							"poissonBound"
				text: 							qsTr("Poisson")
				id: 							poissonBound
				enabled: 						variableTypeCorrect.checked
			}
			
			RadioButton 
			{ 
				name: 							"hyperBound"
				text: 							qsTr("Hypergeometric")
				id: 							hyperBound
				enabled: 						variableTypeCorrect.checked 
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
				id: 							displayPercentages
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
		title: 									qsTr("D.     Tables and Plots")
		columns:								2
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		ColumnLayout {

			GroupBox
			{
				title: 							qsTr("Statistics")

				CheckBox
				{
					text: 						qsTr("Most likely error (MLE)")
					name: 						"mostLikelyError"
					checked: 					true
				}

				CheckBox
				{
					text: 						qsTr("Obtained precision")
					name: 						"obtainedPrecision"
					checked: 					minimumPrecision.checked
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
