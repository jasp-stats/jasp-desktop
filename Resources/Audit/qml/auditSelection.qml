
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

	columns:									2

	// Extra options
	CheckBox 
	{ 
		name:									"workflow"
		checked:								false
		visible:								false
	}

	VariablesForm
	{
		id:										variablesFormSampling

		AvailableVariablesList
		{
			name:								"variablesFormSampling"
		}

		AssignedVariablesList
		{
			id:									recordNumberVariable
			name:								"recordNumberVariable"
			title:								qsTr("Record ID's")
			singleVariable:						true
			allowedColumns:						["nominal", "nominalText", "ordinal", "scale"]
			allowAnalysisOwnComputedColumns:	false
		}

		AssignedVariablesList
		{
			id:									monetaryVariable
			name:								"monetaryVariable"
			title:								musSampling.checked ? qsTr("Ist Position") : qsTr("Ist Position <i>(optional)</i>")
			singleVariable:						true
			allowedColumns:						["scale"]
			allowAnalysisOwnComputedColumns:	false
			onCountChanged:						monetaryVariable.count > 0 ? musSampling.click() : recordSampling.click() 
		}

		AssignedVariablesList
		{
			name:								"rankingVariable"
			title: 								qsTr("Ranking Variable <i>(optional)</i>")
			singleVariable:						true
			allowedColumns:						["scale"]
			allowAnalysisOwnComputedColumns: 	false
		}

		AssignedVariablesList
		{
			name:								"additionalVariables"
			title: 								qsTr("Additional Variables <i>(optional)</i>")
			Layout.preferredHeight: 			140 * preferencesModel.uiScale
			allowedColumns: 					["scale", "ordinal", "nominal"]
			allowAnalysisOwnComputedColumns: 	false
		}
	}

	IntegerField
	{
		id:										sampleSize
		text: 									qsTr("Number of sampling units to select")
		name: 									"sampleSize"
		defaultValue: 							0
		min: 									0
		max: 									dataSetModel.rowCount()
		fieldWidth: 							60
	}	

	GroupBox
	{
		title: 									qsTr("Explanatory Text")
		columns: 								2
		visible:								false

		CheckBox
		{
			id: 								explanatoryText
			text: 								qsTr("Enable")
			name: 								"explanatoryText"
			checked: 							true
		}

		HelpButton
		{
			helpPage:							"Audit/explanatoryText"
			toolTip: 							qsTr("Show explanatory text at each step of the analysis")
		}
	}

	Section
	{
		title: 									qsTr("A.    Selection Methodology")
		columns:								3

		CheckBox
		{
			name:								"shufflePopulationBeforeSampling"
			text:								qsTr("Randomly organize transactions before selection")
			checked:							false
			Layout.columnSpan:					3
		}

		Divider 
		{ 
			width: 								parent.width 
		} 

		RadioButtonGroup
		{
			id: 								selectionType
			title:								qsTr("Sampling Units")
			name: 								"selectionType"
			columns:							2

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
			title:								qsTr("Selection Method")
			name: 								"selectionMethod"
			columns:							2

			RadioButton
			{
				id: 							randomSampling
				text: 							qsTr("Random sampling")
				name: 							"randomSampling"
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
		}
	}

	Section
	{
		title:									qsTr("B     Advanced Options")
		columns:								1

		RadioButtonGroup
		{
			id: 								valuta
			title: 								qsTr("Currency")
			name: 								"valuta"
			enabled:							monetaryVariable.count > 0

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
		title: 									qsTr("C.    Tables")
		columns:								1

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

	CheckBox 
	{ 
		id: 									addSampleIndicator  
		name: 									"addSampleIndicator"
		text: 									qsTr("Add selection counter to data")
		enabled: 								recordNumberVariable.count > 0

		ComputedColumnField 
		{ 
				id:								sampleIndicatorColumn
				name: 							"sampleIndicatorColumn"
				text: 							qsTr("Column name: ")
				fieldWidth: 					120
				visible:    					addSampleIndicator.checked
		}
	}

	Item
	{
		Layout.preferredHeight: 				downloadReportSelection.height
		Layout.fillWidth: 						true
		Layout.columnSpan:						2

		Button
		{
			id:									downloadDataSelection
			anchors.right:	 					downloadReportSelection.left
			anchors.rightMargin:				jaspTheme.generalAnchorMargin
			text:								qsTr("<b>Export Data</b>")
			enabled:							sampleSize.value > 0
			onClicked: 							form.exportResults() // This should still be changed to data export
			visible:							false // That is why this is false
		}

		Button
		{
			id: 								downloadReportSelection
			enabled:							sampleSize.value > 0
			anchors.right: 						parent.right
			anchors.bottom: 					parent.bottom
			text: 								qsTr("<b>Download Report</b>")
			onClicked: 							form.exportResults()
		}
	}
}
