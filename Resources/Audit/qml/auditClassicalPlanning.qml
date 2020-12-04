
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
							onCheckedChanged:	if (checked & populationValue.value == 0) binomial.click()

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
							onCheckedChanged:	if (checked) poisson.click()

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
												if(populationValue.value > 0) poisson.click()
												if(populationValue.value == 0) binomial.click()
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
		text: 									qsTr("A.     Risk Assessments")
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
				id:								irHigh
				text: 							qsTr("High")
				name: 							"High"
				checked: 						true	
			}

			RadioButton 
			{ 
				text: 							qsTr("Medium")
				name: 							"Medium"
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)
			}

			RadioButton 
			{ 
				text: 							qsTr("Low")
				name: 							"Low"
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)
			}

			RadioButton
			{
				id: 							irCustom
				text:	 						qsTr("Custom")
				name: 							"Custom"
				childrenOnSameRow: 				true
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)

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
				id:								crHigh
				text: 							qsTr("High")
				name: 							"High"
				checked: 						true	
			}

			RadioButton 
			{ 
				text: 							qsTr("Medium")
				name: 							"Medium"
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)
			}

			RadioButton 
			{ 
				text: 							qsTr("Low")
				name: 							"Low"
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)
			}

			RadioButton
			{
				id: 							crCustom
				text:	 						qsTr("Custom")
				name: 							"Custom"
				childrenOnSameRow: 				true
				enabled:						(expectedRelative.checked & expectedPercentage.value == 0) | (expectedAbsolute.checked & expectedNumber.value == 0)

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
					id:							expectedPercentage
					name: 						"expectedPercentage"
					enabled: 					expectedRelative.checked
					decimals: 					2
					defaultValue: 				0
					visible: 					expectedRelative.checked
					fieldWidth: 				40
					onValueChanged:				if(expectedRelative.checked & expectedPercentage.value > 0) 
					{
												irHigh.click()
												crHigh.click()
					}
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
					id:							expectedNumber
					name: 						"expectedNumber"
					enabled: 					expectedAbsolute.checked
					defaultValue: 				0
					min: 						0
					decimals: 					3
					visible: 					expectedAbsolute.checked
					fieldWidth: 				60
					label: 						performanceMateriality.checked & materialityAbsolute.checked ? (euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)) : ""
					onValueChanged:				if(expectedAbsolute.checked & expectedNumber.value > 0) 
					{
												irHigh.click()
												crHigh.click()
					}
				}
			}
		}
	}

	Section
	{
		text:									qsTr("B.     Advanced Options")
		columns:								4
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		RadioButtonGroup
		{
			id: 								planningModel
			title: 								qsTr("Probability Distribution")
			name: 								"planningModel"

			RadioButton
			{
				id: 							binomial									
				text: 							qsTr("Binomial")
				name: 							"binomial"
				checked: 						true
			}

			RadioButton 
			{
				id: 							poisson
				text: 							qsTr("Poisson")
				name: 							"Poisson"
			}

			RadioButton 
			{
				id: 							hypergeometric
				text: 							qsTr("Hypergeometric")
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
					fieldWidth: 				100
					enabled: 					otherValuta.checked
					visible: 					otherValuta.checked
				}
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

	Section
	{
		title: 									qsTr("C.     Plots")
		columns:								2
		enabled:								((performanceMateriality.checked & ((materialityRelative.checked & materialityPercentage.value > 0) | (materialityAbsolute.checked & materialityValue.value > 0 & populationValue.value > 0))) | (minimumPrecision.checked & minimumPrecisionPercentage.value > 0)) & populationSize.value > 0

		GroupBox
		{
			title: 								qsTr("Plots")

			CheckBox
			{
				text: 							qsTr("Compare required sample sizes")
				name: 							"decisionPlot"
				enabled:						performanceMateriality.checked
			}

			CheckBox
			{
				text: 							qsTr("Implied distribution of errors")
				name: 							"samplingDistribution"
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
			enabled: 							materialityRelative.checked ? (populationSize.value != 0 & materialityPercentage.value != 0) : (populationSize.value != 0 & materialityValue.value != 0 & populationValue.value != 0)
			anchors.right: 						parent.right
			anchors.bottom: 					parent.bottom
			text: 								qsTr("<b>Download Report</b>")
			onClicked: 							form.exportResults()
		}
	}
}
