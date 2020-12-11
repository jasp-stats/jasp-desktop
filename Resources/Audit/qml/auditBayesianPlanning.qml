
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
// <http://www.gnu.org/licenses/>.\
//

// When making changes to this file always mention @koenderks as a
// reviewer in the Pull Request

import QtQuick 									2.8
import QtQuick.Layouts 							1.3
import JASP.Controls 							1.0
import JASP.Widgets 							1.0

Form 
{
	
	columns:									1

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
		name: 									"separateKnownAndUnknownMisstatement"
		checked: 								false
		visible: 								false
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
							onCheckedChanged:	if (checked & populationValue.value == 0) beta.click()

							PercentField
							{
								id: 			materialityPercentage
								visible: 		materialityRelative.checked
								decimals: 		2
								defaultValue: 	0
								name: 			"materialityPercentage"
								fieldWidth: 	50 * preferencesModel.uiScale
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
							onCheckedChanged:	if (checked) gamma.click()

							DoubleField
							{
								id: 			materialityValue
								visible: 		materialityAbsolute.checked
								name: 			"materialityValue"
								defaultValue: 	0
								min: 			0
								fieldWidth: 	90 * preferencesModel.uiScale
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
			title: 								qsTr("Population")

			IntegerField
			{
				id: 							populationSize
				name: 							"populationSize"
				text: 							qsTr("Size")
				fieldWidth: 					80 * preferencesModel.uiScale
				defaultValue: 					0
				min: 							0
			}

			DoubleField
			{
				id: 							populationValue
				name: 							"populationValue"
				text: 							qsTr("Value")
				defaultValue: 					0
				fieldWidth: 					80 * preferencesModel.uiScale
				min: 							0
				decimals: 						2
				onValueChanged:					
				{
												if(populationValue.value > 0) gamma.click()
												if(populationValue.value == 0) beta.click()
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

	Section 
	{
		title: 									qsTr("Prior Information")
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
					fieldWidth: 				50 * preferencesModel.uiScale
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
					fieldWidth: 				80 * preferencesModel.uiScale
					label: 						performanceMateriality.checked & materialityAbsolute.checked ? (euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)) : ""
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
				id: 							sampleN
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
		text: 									qsTr("Advanced Options")
		columns:								4
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RadioButtonGroup
		{
			id: 								planningModel
			title: 								qsTr("Probability Distribution")
			name: 								"planningModel"

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
			}

			RadioButton
			{
				id: 							betaBinomial
				text: 							qsTr("Beta-binomial")
				name: 							"hypergeometric"
				enabled:						performanceMateriality.checked
			}
		}

		GroupBox
		{
			title: 								qsTr("Calculation Settings")

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
			enabled:							materialityAbsolute.checked | populationValue.value > 0

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
					fieldWidth: 				100 * preferencesModel.uiScale
					enabled: 					otherValuta.checked
					visible: 					otherValuta.checked
				}
			}
		}

		GroupBox
		{
			title: 								qsTr("Explanatory Text")
			columns: 2

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

	Section
	{
		title: 									qsTr("Tables and Plots")
		columns:								2
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

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
				text: 							qsTr("Compare required sample sizes")
				name: 							"decisionPlot"
				enabled:						![2, 3].includes(priorConstructionMethod.currentIndex) & performanceMateriality.checked
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
				}

				CheckBox
				{
					text: 						qsTr("Display additional information")
					name: 						"priorPlotAdditionalInfo"
					checked: 					true

					RadioButtonGroup 
					{
						title: 					qsTr("Shade")
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
		Layout.preferredHeight: 				downloadReportPlanning.height
		Layout.fillWidth: 						true

		Button
		{
			id: 								downloadReportPlanning
			enabled: 							((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0
			anchors.right: 						parent.right
			anchors.bottom: 					parent.bottom
			text: 								qsTr("<b>Download Report</b>")
			onClicked: 							form.exportResults()
		}
	}
}
