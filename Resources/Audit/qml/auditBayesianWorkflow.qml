
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

import QtQuick										2.8
import QtQuick.Layouts								1.3
import JASP.Controls								1.0
import JASP.Widgets									1.0

// --------------------------------------------------------------------------------------------------------------------------------------------
// -------------------------------------------------  BEGIN WORKFLOW  -------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------------------------------------

Form
{

	columns: 										1

	// Extra options to be included without being visible
	CheckBox 
	{ 
		name: 										"workflow"
		checked: 									true
		visible: 									false 
	}

	CheckBox 
	{ 
		name: 										"bayesianAnalysis"
		checked: 									true
		visible: 									false 
	}

	CheckBox 
	{ 
		name: 										"useSumStats"
		checked: 									false
		visible: 									false 
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  PLANNING  -----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 										planningPhase
		text: 										planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning")
		expanded:									!samplingChecked.checked
		columns: 									1

		GridLayout
		{
			columns: 								3

			GroupBox 
			{
				title: 								qsTr("Sampling Objectives")
				enabled: 							!pasteVariables.checked
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

								DoubleField
								{
									id: 			materialityValue
									visible: 		materialityAbsolute.checked
									name: 			"materialityValue"
									defaultValue: 	0
									min: 			0
									fieldWidth: 	90
									decimals: 		2
									label: 			euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)
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
						max:						100
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
				id: 								auditRisk
				title: 								qsTr("Audit Risk")
				enabled: 							!pasteVariables.checked

				PercentField
				{
					name: 							"confidence"
					label: 							qsTr("Confidence")
					decimals: 						2
					defaultValue: 					95
					min: 							1
					max: 							99
				}
			}

			GroupBox
			{
				title: 								qsTr("Explanatory Text")
				columns: 							2

				CheckBox
				{
					id: 							explanatoryText
					text: 							qsTr("Enable")
					name: 							"explanatoryText"
					checked: 						true
				}

				HelpButton
				{
					helpPage:						"Audit/explanatoryText"
					toolTip: 						qsTr("Show explanatory text at each step of the analysis")
				}
			}			
		}

		Divider 
		{ 
			width: 									parent.width 
		}

		Item
		{
			Layout.preferredHeight: 				variableSelectionTitle.height
			Layout.fillWidth: 						true

			Label
			{
				id: 								variableSelectionTitle
				anchors.horizontalCenter: 			parent.horizontalCenter
				text: 								qsTr("<b>Variable Definitions</b>")
				font:								jaspTheme.fontLabel
				enabled:							!pasteVariables.checked & ((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0))
			}
		}

		VariablesForm
		{
			id: 			 						variablesFormPlanning
			preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight
			enabled:		 						!pasteVariables.checked & ((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0))

			AvailableVariablesList 
			{ 
				name: 								"variablesFormPlanning" 
			}

			AssignedVariablesList
			{
				id: 								recordNumberVariable
				name: 								"recordNumberVariable"
				title: 								qsTr("Transaction ID's")
				singleVariable: 					true
				allowedColumns: 					["nominal", "nominalText", "ordinal", "scale"]
				allowAnalysisOwnComputedColumns: 	false
			}

			AssignedVariablesList
			{
				id: 								monetaryVariable
				name: 								"monetaryVariable"
				title: 								performanceMateriality.checked & materialityAbsolute.checked ? qsTr("Ist Position <i>(required)</i>") : qsTr("Ist Position <i>(optional)</i>")
				singleVariable: 					true
				allowedColumns: 					["scale"]
				allowAnalysisOwnComputedColumns: 	false
				onCountChanged:								
				{
													monetaryVariable.count > 0 ? gamma.click() : beta.click()
													monetaryVariable.count > 0 ? musSampling.click() : recordSampling.click() 
													monetaryVariable.count > 0 ? variableTypeAuditValues.click() : variableTypeCorrect.click()
				}
			}
		}

		Section
		{
			title: 									qsTr("A.     Critical Transactions")
			enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & monetaryVariable.count > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & recordNumberVariable.count > 0
			columns: 								1

			GridLayout
			{
				columns: 							2
				enabled: 							!pasteVariables.checked

				CheckBox
				{
					id: 							flagCriticalTransactions
					name:							"flagCriticalTransactions"
					text:							qsTr("Select critical transactions")
					enabled:						monetaryVariable.count > 0
					checked:						monetaryVariable.count > 0

					ComputedColumnField
					{
						id: 						criticalTransactions
						name: 						"criticalTransactions"
						text: 						qsTr("Column name critical transactions: ")
						fieldWidth: 				120
						value: 						"Critical"
					}

					CheckBox
					{
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
						id:							inspectCriticalTransactions
						text: 						qsTr("Inspect")
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

			Divider 
			{
				width: 								parent.width
				visible: 							flagCriticalTransactions.checked						
			}

			Label
			{
				anchors.horizontalCenter: 			parent.horizontalCenter
				text: 								qsTr("<b>Critical Transaction List</b>")
				visible:							flagCriticalTransactions.checked
			}

			TableView
			{
				id:									criticalTransactionTable
				name:								"criticalTransactionTable"
				Layout.fillWidth: 					true
				modelType:							"FilteredDataEntryModel"
				source:     						["recordNumberVariable", "monetaryVariable"]
				itemType:							"integer"
				colName: 							""
				visible:							flagCriticalTransactions.checked	
				extraCol: 							criticalTransactions.value
				filter:								criticalTransactions.value + " > 0"		
				implicitHeight: 					200	
			}
		}

		Section 
		{
			title: 									qsTr("B.     Efficiency Techniques")
			columns: 								1
			enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & monetaryVariable.count > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & recordNumberVariable.count > 0

			GroupBox
			{
				title: 								qsTr("Efficiency Techniques")
				enabled:							!pasteVariables.checked & recordNumberVariable.count > 0 & monetaryVariable.count > 0

				RowLayout
				{
					CheckBox
					{
						id: 						separateKnownAndUnknownMisstatement
						text: 						qsTr("Separate known and unknown misstatement")
						name: 						"separateKnownAndUnknownMisstatement"
						onCheckedChanged: 			if(checked) beta.click()
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
				}
			}
		}

		Section 
		{
			title: 									qsTr("C.     Prior Information")
			columns: 								3
			enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & monetaryVariable.count > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & recordNumberVariable.count > 0

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
				enabled:							!pasteVariables.checked

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
						label: 						performanceMateriality.checked & materialityAbsolute.checked ? (euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)) : ""
					}
				}

				RadioButton 
				{
					id: 							expectedAllPossible
					name: 							"expectedAllPossible"
					text: 							qsTr("All possible")
					enabled:						separateKnownAndUnknownMisstatement.checked & !performanceMateriality.checked
				}
			}

			GroupBox
			{
				visible: 							[1].includes(priorConstructionMethod.currentIndex)
				Layout.columnSpan: 					3
				columns: 							3
				enabled:							!pasteVariables.checked

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
				enabled:							!pasteVariables.checked

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
				enabled:							!pasteVariables.checked

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
			text: 									qsTr("D.     Advanced Options")
			columns: 								3
			enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & monetaryVariable.count > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & recordNumberVariable.count > 0

			RadioButtonGroup
			{
				id: 								planningModel
				title: 								qsTr("Probability Distribution")
				name: 								"planningModel"
				enabled:							!pasteVariables.checked

				RadioButton
				{
					id: 							beta
					text: 							qsTr("Beta")
					name: 							"binomial"
					checked: 						true
				}

				RadioButton
				{
					id: 							gamma
					text: 							qsTr("Gamma")
					name: 							"Poisson"
					enabled:						!separateKnownAndUnknownMisstatement.checked
				}

				RadioButton
				{
					id: 							betaBinomial
					text: 							qsTr("Beta-binomial")
					name: 							"hypergeometric"
					enabled:						!separateKnownAndUnknownMisstatement.checked && performanceMateriality.checked
				}
			}

			GroupBox
			{
				title: 								qsTr("Calculation Settings")
				enabled:							!pasteVariables.checked

				IntegerField 
				{
					name: 							"sampleSizeIncrease"
					text: 							qsTr("Step size")
					min: 							1
					max:							20
					defaultValue: 					1
				}
			}

			RadioButtonGroup
			{
				id: 								valuta
				title: 								qsTr("Currency")
				name: 								"valuta"
				enabled:							monetaryVariable.count > 0 | materialityAbsolute.checked

				RadioButton 	
				{ 
					id: 							euroValuta
					text: 							qsTr("Euro (€)")
					name: 							"euroValuta"
					checked: 						true
				}

				RadioButton 	
				{ 
					id: 							dollarValuta	
					text: 							qsTr("Dollar ($)")
					name: 							"dollarValuta"
				}

				RowLayout
				{
					RadioButton	
					{ 
						id: 						otherValuta
						text:						qsTr("Other")
						name: 						"otherValuta"	
					}

					TextField
					{
						id: 						otherValutaName
						name: 						"otherValutaName"
						fieldWidth: 				100
						enabled: 					otherValuta.checked
						visible: 					otherValuta.checked
					}
				}
			}
		}

		Section
		{
			title: 									qsTr("E.     Tables and Plots")
			columns: 								2
			enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & monetaryVariable.count > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & recordNumberVariable.count > 0

			ColumnLayout
			{
				GroupBox
				{
					title: 							qsTr("Statistics")

					CheckBox
					{
						text: 						qsTr("Expected posterior odds")
						name: 						"expectedEvidenceRatio"
						enabled:					performanceMateriality.checked
					}

					CheckBox
					{
						text: 						qsTr("Expected Bayes factor (BF\u208B\u208A)")
						name: 						"expectedBayesFactor"
						enabled:					performanceMateriality.checked
					}
				}

				GroupBox
				{
					title: 							qsTr("Tables")

					CheckBox
					{
						text: 						qsTr("Descriptive statistics for Ist values")
						name: 						"bookValueDescriptives"
						enabled: 					monetaryVariable.count > 0
					}

					CheckBox
					{
						text: 						qsTr("Implicit sample induced by prior distribution")
						name: 						"implicitSampleTable"
					}

					CheckBox
					{
						text: 						qsTr("Description of prior and expected posterior distribution")
						name: 						"priorStatistics"
					}
				}
			}

			GroupBox
			{
				title: 								qsTr("Plots")

				CheckBox
				{
					name: 							"bookValueDistribution"
					text: 							qsTr("Distribution of Ist values")
					enabled: 						monetaryVariable.count > 0
				}

				CheckBox
				{
					text: 							qsTr("Compare required sample sizes")
					name: 							"decisionPlot"
					enabled:						!separateKnownAndUnknownMisstatement.checked & ![2, 3].includes(priorConstructionMethod.currentIndex) & performanceMateriality.checked
				}

				CheckBox
				{
					text: 							qsTr("Implied prior distribution")
					name: 							"priorPlot"
					childrenOnSameRow: 				false

					PercentField
					{
						text: 						qsTr("x-axis limit")
						name: 						"priorPlotLimit"
						defaultValue: 				50
					}

					CheckBox
					{
						text: 						qsTr("Expected posterior distribution")
						name: 						"priorPlotExpectedPosterior"
						enabled:					!expectedAllPossible.checked
					}

					CheckBox
					{
						text: 						qsTr("Display additional information")
						name: 						"priorPlotAdditionalInfo"
						checked: 					true

						RadioButtonGroup
						{
							title: 					qsTr("Shade area")
							name: 					"shadePrior"

							RadioButton
							{
								text: 				qsTr("Credible region")
								name: 				"shadePriorCredibleRegion"
								checked: 			true
							}

							RadioButton
							{
								text: 				qsTr("Hypothesis regions")
								name: 				"shadePriorHypotheses"
								enabled:			performanceMateriality.checked
							}

							RadioButton
							{
								text: 				qsTr("None")
								name: 				"shadePriorNone"
							}
						}
					}
				}
			}
		}

		Item
		{
			Layout.preferredHeight: 				toSampling.height
			Layout.fillWidth: 						true
			enabled:								!pasteVariables.checked

			Button
			{
				id:									downloadReportPlanning
				anchors.right:	 					toSampling.left
				anchors.rightMargin:				jaspTheme.generalAnchorMargin
				text:								qsTr("<b>Download Report</b>")
				enabled:							((materialityRelative.checked ?
													materialityPercentage.value != "0" & recordNumberVariable.count > 0 :
													materialityValue.value 		!= "0" & recordNumberVariable.count > 0 & monetaryVariable.count > 0) |
													(minimumPrecision.checked & minimumPrecisionPercentage.value != "0" & recordNumberVariable.count > 0))
				onClicked: 							form.exportResults()
			}

			CheckBox
			{
				id: 								samplingChecked
				name: 								"samplingChecked"
				anchors.right:						toSampling.left
				width:								0
				visible: 							false
				checked: 							false
			}

			Button
			{
				id: 								toSampling
				anchors.right: 						parent.right
				text: 								qsTr("<b>To Selection</b>")
				enabled: 							!samplingChecked.checked & ((materialityRelative.checked ?
													materialityPercentage.value > 0 & recordNumberVariable.count > 0 :
													materialityValue.value > 0 & recordNumberVariable.count > 0 & monetaryVariable.count > 0) |
													(minimumPrecision.checked & minimumPrecisionPercentage.value > 0 & recordNumberVariable.count > 0))
				onClicked:							samplingChecked.checked	= true
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  SELECTION  ----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 										samplingPhase
		text: 										samplingPhase.expanded ? qsTr("<b>2. Selection</b>") : qsTr("2. Selection")
		enabled: 									samplingChecked.checked
		expanded: 									samplingChecked.checked & !executionChecked.checked
		columns: 									1

		CheckBox 
		{
			id: 									customSampleConstruction
			name: 									"customSampleConstruction"
			text:									qsTr("Add custom variables to sample")
			enabled:								!pasteVariables.checked
		}

		VariablesForm
		{
			id: 									variablesFormSampling
			preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight
			enabled: 								!pasteVariables.checked & customSampleConstruction.checked

			AvailableVariablesList
			{
				name: 								"variablesFormSampling"
			}

			AssignedVariablesList
			{
				name: 								"rankingVariable"
				title: 								qsTr("Ranking Variable <i>(optional)</i>")
				singleVariable: 					true
				allowedColumns: 					["scale"]
				allowAnalysisOwnComputedColumns:	false
			}

			AssignedVariablesList
			{
				name: 								"additionalVariables"
				title: 								qsTr("Additional Variables <i>(optional)</i>")
				Layout.preferredHeight: 			140 * preferencesModel.uiScale
				allowedColumns: 					["scale", "ordinal", "nominal"]
				allowAnalysisOwnComputedColumns:	false
			}
		}

		Section
		{
			title: 									qsTr("A.     Selection Methodology")
			columns: 								3
			expanded: 								samplingChecked.checked & !executionChecked.checked

			CheckBox
			{
				name:								"shufflePopulationBeforeSampling"
				text:								qsTr("Randomly organize transactions before selection")
				checked:							false
				Layout.columnSpan:					3
				enabled:							!pasteVariables.checked & !separateKnownAndUnknownMisstatement.checked
			}

			Divider 
			{ 
				width: 								parent.width 
			} 

			RadioButtonGroup
			{
				id: 								selectionType
				title: 								qsTr("Sampling Units")
				name: 								"selectionType"
				columns:							2
				enabled:							!pasteVariables.checked

				RadioButton
				{
					id: 							musSampling
					text: 							qsTr("Monetary unit sampling")
					name: 							"musSampling"
					enabled: 						monetaryVariable.count > 0
				}

				HelpButton
				{
					helpPage:						"Audit/monetaryUnitSampling"
					toolTip: 						qsTr("Click to learn more about monetary unit sampling.")
				}

				RadioButton
				{
					id: 							recordSampling
					text: 							qsTr("Record sampling")
					name: 							"recordSampling"
					enabled: 						!separateKnownAndUnknownMisstatement.checked
					checked: 						true
				}

				HelpButton
				{
					toolTip: 						qsTr("Click to learn more about record sampling.")
					helpPage:						"Audit/recordSampling"
				}
			}

			RadioButtonGroup
			{
				id: 								selectionMethod
				title: 								qsTr("Selection Method")
				name: 								"selectionMethod"
				columns:							2
				enabled:							!pasteVariables.checked

				RadioButton
				{
					id: 							randomSampling
					text: 							qsTr("Random sampling")
					name: 							"randomSampling"
					enabled: 						!separateKnownAndUnknownMisstatement.checked
				}

				HelpButton
				{
					toolTip: 						qsTr("Click to learn more about random sampling.")
					helpPage:						"Audit/randomSampling"
				}

				RadioButton
				{
					id: 							cellSampling
					text: 							qsTr("Cell sampling")
					name: 							"cellSampling"
					enabled: 						!separateKnownAndUnknownMisstatement.checked
				}

				HelpButton
				{
					toolTip: 						qsTr("Click to learn more about cell sampling.")
					helpPage:						"Audit/cellSampling"
				}

				RadioButton
				{
					id: 							systematicSampling
					text: 							qsTr("Fixed interval sampling")
					name: 							"systematicSampling"
					checked: 						true
				}

				HelpButton
				{
					toolTip: 						qsTr("Click to learn more about fixed interval sampling.")
					helpPage:						"Audit/fixedIntervalSampling"
				}
			}

			IntegerField
			{
				id: 								seed
				text: 								systematicSampling.checked ? qsTr("Starting point") : qsTr("Seed")
				name: 								"seed"
				defaultValue: 						1
				min: 								1
				max: 								99999
				fieldWidth: 						60
				enabled: 							!separateKnownAndUnknownMisstatement.checked & !pasteVariables.checked
				visible:							!separateKnownAndUnknownMisstatement.checked
			}
		}

		Section
		{
			title: 									qsTr("B.     Tables")
			columns: 								1

			GroupBox
			{
				id: 								samplingTables
				title: 								qsTr("Tables")

				CheckBox 
				{ 
					text: 							qsTr("Display selected transactions")
					name: 							"displaySample"									
				}

				CheckBox 
				{ 
					id: 							sampleDescriptives
					text: 							qsTr("Descriptive statistics of selected transactions")
					name: 							"sampleDescriptives"	
				}

				GridLayout
				{
					Layout.leftMargin: 				20 * preferencesModel.uiScale

					ColumnLayout
					{
						spacing: 					5 * preferencesModel.uiScale

						CheckBox 
						{ 
							text: 					qsTr("Mean")
							name: 					"mean"
							enabled: 				sampleDescriptives.checked
							checked: 				true	
						}
						
						CheckBox 
						{ 
							text: 					qsTr("Median")			
							name: 					"median"
							enabled: 				sampleDescriptives.checked
							checked: 				true	
						}
						
						CheckBox 
						{ 
							text: 					qsTr("Std. deviation")	
							name: 					"sd"
							enabled: 				sampleDescriptives.checked
							checked: 				true	
						}
						
						CheckBox 
						{ 
							text: 					qsTr("Variance") 			
							name: 					"var"
							enabled: 				sampleDescriptives.checked					
						}
					}

					ColumnLayout
					{
						spacing: 					5 * preferencesModel.uiScale

						CheckBox 
						{ 
							text: 					qsTr("Minimum")	
							name: 					"min"	
							enabled: 				sampleDescriptives.checked	
						}
						
						CheckBox 
						{ 
							text: 					qsTr("Maximum")
							name: 					"max"	
							enabled: 				sampleDescriptives.checked	
						}
						
						CheckBox 
						{ 
							text: 					qsTr("Range")
							name: 					"range"
							enabled: 				sampleDescriptives.checked	
						}
					}
				}
			}
		}

		Item
		{
			Layout.preferredHeight: 				toExecution.height
			Layout.fillWidth: 						true
			enabled:								!pasteVariables.checked

			Button
			{
				anchors.left: 						parent.left
				text: 								qsTr("<b>Reset Workflow</b>")
				onClicked: 							form.reset()
			}

			Button
			{
				id:									downloadReportSelection
				enabled:							((materialityRelative.checked ?
													materialityPercentage.value != "0" & recordNumberVariable.count > 0 :
													materialityValue.value 		!= "0" & recordNumberVariable.count > 0 & monetaryVariable.count > 0) |
													(minimumPrecision.checked & minimumPrecisionPercentage.value != "0" & recordNumberVariable.count > 0))
				anchors.right:						toExecution.left
				anchors.rightMargin:				jaspTheme.generalAnchorMargin
				text:								qsTr("<b>Download Report</b>")
				onClicked:							form.exportResults()
			}

			CheckBox
			{
				id: 								executionChecked
				anchors.right: 						toExecution.left
				width: 								height
				visible: 							false
				name: 								"executionChecked"
				checked: 							false
			}

			Button
			{
				id: 								toExecution
				anchors.right: 						parent.right
				text: 								qsTr("<b>To Execution</b>")
				enabled:							!executionChecked.checked
				onClicked:							executionChecked.checked = true
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  EXECUTION  ----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 										executionPhase
		text: 										executionPhase.expanded ? qsTr("<b>3. Execution</b>") : qsTr("3. Execution")
		expanded: 									executionChecked.checked & !evaluationChecked.checked
		enabled: 									executionChecked.checked
		columns: 									1	

		Item
		{
			Layout.preferredHeight: 				selectHowToAnalyseObservations.height
			Layout.fillWidth: 						true

			Label
			{
				id: 								selectHowToAnalyseObservations
				anchors.horizontalCenter: 			parent.horizontalCenter
				text: 								qsTr("<b>How would you like to evaluate your transactions?</b>")
			}
		}

		Item
		{
			Layout.preferredHeight: 				variableType.height
			Layout.fillWidth: 						true

			RadioButtonGroup
			{
				id: 								variableType
				name: 								"variableType"
				title: 								qsTr("")
				anchors.horizontalCenter: 			parent.horizontalCenter
				enabled:							!pasteVariables.checked

				RowLayout
				{
					spacing: 						200 * preferencesModel.uiScale

					RowLayout
					{
						RadioButton
						{
							id: 					variableTypeAuditValues
							text: 					qsTr("Soll values")
							name: 					"variableTypeAuditValues"
							checked: 				true
							enabled: 				monetaryVariable.count > 0 & !betaBinomial.checked
						}

						HelpButton 
						{ 
							toolTip: 				qsTr("Adds a column to specify the Soll values of the transactions.")
							helpPage: 				"?" 
						}
					}

					RowLayout
					{
						RadioButton
						{
							id: 					variableTypeCorrect
							text: 					qsTr("Correct / Incorrect")
							name: 					"variableTypeCorrect"
							checked: 				false
							enabled: 				!separateKnownAndUnknownMisstatement.checked
						}

						HelpButton 
						{ 
							toolTip:				qsTr("Adds a column to specify the transactions as correct (0) or misstated (1)")
							helpPage: 				"?" 
						}
					}
				}
			}
		}

		Divider 
		{ 
			width: 									parent.width 
		}

		RowLayout
		{
			GroupBox
			{
				id: 								groupBoxVariableNames
				enabled:							!pasteVariables.checked

				ComputedColumnField
				{
					id: 							sampleFilter
					name: 							"sampleFilter"
					text: 							qsTr("Column name selection result: ")
					fieldWidth: 					120
					value: 							"SelectionResult"
				}

				ComputedColumnField
				{
					id: 							variableName
					name: 							"variableName"
					text: 							variableTypeAuditValues.checked ? qsTr("Column name Soll values: ") : qsTr("Column name audit result: ")
					fieldWidth: 					120
					value: 							"AuditResult"
				}
			}

			Item
			{
				Layout.preferredHeight: 			groupBoxVariableNames.height
				Layout.fillWidth: 					true

				CheckBox
				{
					id: 							pasteVariables
					anchors.right: 					pasteButton.left
					width: 							height
					visible: 						false
					name: 							"pasteVariables"
					checked: 						false
				}

				Button
				{
					id: 							pasteButton
					text: 							qsTr("<b>Fill Variables</b>")
					enabled: 						sampleFilter.value != "" & variableName.value != "" & !pasteVariables.checked
					onClicked:
					{
													pasteVariables.checked 		= true
													performAuditTable.colName   = variableName.value
													performAuditTable.extraCol	= sampleFilter.value
													flagCriticalTransactions.checked & inspectCriticalTransactions.checked ? performAuditTable.filter 	= sampleFilter.value + " > 0" + " | " + criticalTransactions.value + " > 0" : performAuditTable.filter 	= sampleFilter.value + " > 0"
					}
				}
			}
		}

		Item 
		{
			Layout.preferredHeight: 				performAuditText.height
			Layout.fillWidth: 						true

			Label
			{
				id: 								performAuditText
				anchors.horizontalCenter: 			parent.horizontalCenter
				text: 								variableTypeAuditValues.checked ? qsTr("<b>Annotate your selected transactions with their Soll values.</b>") : qsTr("<b>Annotate your selected transactions with a 0 (correct) or a 1 (misstated).</b>")
				visible: 							pasteVariables.checked
			}
		}

		Section
		{
			id: 									executeAuditSection
			title:									qsTr("A.     Data Entry")
			expanded:								pasteVariables.checked
			enabled:								pasteVariables.checked

			TableView
			{
				id:									performAuditTable
				name:								"performAudit"
				Layout.fillWidth: 					true
				modelType:							"FilteredDataEntryModel"
				source:     						["recordNumberVariable", "monetaryVariable", "additionalVariables"]
				colName:    						"Filter"
				itemType:							"double"
				decimals:							3
			}
		}

		Item
		{
			Layout.preferredHeight: 				toEvaluation.height
			Layout.fillWidth: 						true
			enabled:								!evaluationChecked.checked

			Button
			{
				anchors.left: 						parent.left
				text: 								qsTr("<b>Reset Workflow</b>");
				onClicked: 							form.reset()
			}

			CheckBox
			{
				id: 								evaluationChecked
				anchors.right: 						toEvaluation.left
				width: 								height
				visible: 							false
				name: 								"evaluationChecked"
				checked: 							false
			}

			Button
			{
				id: 								toEvaluation
				enabled: 							pasteVariables.checked
				anchors.right: 						parent.right
				text: 								qsTr("<b>To Evaluation</b>")
				onClicked:
				{
													executionPhase.expanded = false
													executeAuditSection.expanded = false
													evaluationChecked.checked = true
													if (beta.checked) 						betaBound.click()
													if (betaBinomial.checked) 		betabinomialBound.click()
													if (gamma.checked) 						gammaBound.click()
													if (recordSampling.checked & variableTypeAuditValues.checked)	regressionBound.click()
				}
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  EVALUATION  ---------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 										evaluationPhase
		text: 										evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation")
		expanded: 									evaluationChecked.checked
		enabled: 									evaluationChecked.checked
		columns: 									1

		VariablesForm
		{
			preferredHeight: 						jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name: 								"evaluationVariables"
				source: 							"variablesFormPlanning"
			}

			AssignedVariablesList
			{
				id: 								auditResult
				name: 								"auditResult"
				title: 								variableTypeAuditValues.checked ? qsTr("Soll Values") : qsTr("Audit Result")
				singleVariable: 					true
				allowedColumns: 					["nominal", "scale"]
			}
		}

		Section
		{
			title: 									qsTr("A.     Advanced Options")
			columns: 								3

			RadioButtonGroup
			{
				title: 								qsTr("Estimation Method")
				name: 								"estimator"
				visible:							false

				RadioButton
				{
					id: 							betaBound
					name: 							"betaBound"
					text: 							qsTr("Beta")
					visible: 						!regressionBound.visible & beta.checked
				}

				RadioButton
				{
					id: 							gammaBound
					name: 							"gammaBound"
					text: 							qsTr("Gamma")
					visible: 						!regressionBound.visible & gamma.checked
				}

				RadioButton
				{
					id: 							betabinomialBound
					name: 							"betabinomialBound"
					text: 							qsTr("Beta-binomial")
					visible: 						!regressionBound.visible & betaBinomial.checked
				}

				RadioButton
				{
					id: 							regressionBound
					name: 							"regressionBound"
					text: 							qsTr("Regression")
					visible: 						recordSampling.checked & variableTypeAuditValues.checked & evaluationChecked.checked
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
					text: 							qsTr("Percentages")
					name: 							"displayPercentages"
					checked: 						true
				}

				RadioButton
				{
					text: 							qsTr("Extrapolated amounts")
					name: 							"displayValues"
					enabled:						monetaryVariable.count > 0
				}	
			}
		}

		Section
		{
			title: 									qsTr("B.     Tables and Plots")
			columns: 								2

			ColumnLayout
			{
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

					CheckBox
					{
						text: 						qsTr("Posterior odds")
						name: 						"evidenceRatio"
						visible: 					!regressionBound.visible & performanceMateriality.checked
					}

					CheckBox
					{
						text: 						qsTr("Bayes factor (BF\u208B\u208A)")
						name: 						"bayesFactor"
						visible: 					!regressionBound.visible & performanceMateriality.checked
					}
				}

				GroupBox
				{
					title: 							qsTr("Tables")
					visible: 						!regressionBound.visible

					CheckBox
					{
						text: 						qsTr("Additional samples")
						name: 						"additionalSamples"
					}

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
					visible: 						!regressionBound.visible

					PercentField
					{
						id: 						priorAndPosteriorPlotLimit
						text: 						qsTr("x-axis limit")
						defaultValue: 				50
						name: 						"priorAndPosteriorPlotLimit"
						visible:					!regressionBound.visible
					}

					CheckBox
					{
						id: 						priorAndPosteriorPlotIndicateStatistics
						text: 						qsTr("Display inferential statistics")
						name: 						"priorAndPosteriorPlotIndicateStatistics"
					}

					CheckBox
					{
						id: 						priorAndPosteriorPlotExpectedPosterior
						text: 						qsTr("Expected posterior distribution")
						name: 						"priorAndPosteriorPlotExpectedPosterior"
						enabled:					!expectedAllPossible.checked
						visible:					!regressionBound.visible
					}

					CheckBox
					{
						id: 						priorAndPosteriorPlotAdditionalInfo
						text: 						qsTr("Display additional information")
						name: 						"priorAndPosteriorPlotAdditionalInfo"
						checked: 					true
						visible:					!regressionBound.visible

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
								text: 				qsTr("Hypothesis regions")
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
				anchors.right: 						parent.right
				text: 								qsTr("<b>Download Report</b>")
				enabled: 							auditResult.count > 0
				onClicked:
				{
													evaluationPhase.expanded = false
													form.exportResults()
				}
			}
		}
	}
}
// --------------------------------------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------  END WORKFLOW  -------------------------------------------------------------------------
// --------------------------------------------------------------------------------------------------------------------------------------------