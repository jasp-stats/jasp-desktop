
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

import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0


Form
{
	usesJaspResults: 	true
    columns: 			1

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  PLANNING  -----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 		planningPhase
		text: 		planningPhase.expanded ? qsTr("<b>1. Planning</b>") : qsTr("1. Planning")
		expanded:	!samplingChecked.checked
		columns: 	1

		GridLayout
		{
			columns: 2
			enabled: !pasteVariables.checked

			RadioButtonGroup
			{
				id: 	materiality
				name: 	"materiality"
				title: 	qsTr("Population Materiality")

				RowLayout
				{
					RadioButton
					{
						id: 				materialityAbsolute
						name: 				"materialityAbsolute"
						text: 				qsTr("Absolute")
						checked: 			true
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

				RowLayout
				{
					RadioButton
					{
						id: 				materialityRelative
						name: 				"materialityRelative"
						text: 				qsTr("Relative")
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

		Divider { width: parent.width }

		Item
		{
			Layout.preferredHeight: variableSelectionTitle.height
			Layout.fillWidth: 			true

			Label
			{
				id: 						variableSelectionTitle
				anchors.horizontalCenter: 	parent.horizontalCenter
				text: 						qsTr("<b>Variable definition</b>")
				font:						jaspTheme.fontLabel
			}
		}

		VariablesForm
		{
			id: 			 variablesFormPlanning
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			enabled:		 !pasteVariables.checked

			AvailableVariablesList { name: "variablesFormPlanning" }

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
				id: 				monetaryVariable
				name: 				"monetaryVariable"
				title: 				materialityAbsolute.checked ? qsTr("Book Values <i>(required)</i>") : qsTr("Book Values <i>(optional)</i>")
				singleVariable: 	true
				allowedColumns: 	["scale"]
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
					title: 		qsTr("Inherent Risk")
					name: 		"IR"
					enabled:	!pasteVariables.checked

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
					id: 		expectedErrors
					name: 		"expectedErrors"
					title: 		qsTr("Expected Errors")
					enabled:	!pasteVariables.checked

					RowLayout
					{
						enabled: monetaryVariable.count > 0

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
							label: 			euroValuta.checked ? "€" : (dollarValuta.checked ? "$" : otherValutaName.value)
						}
					}

					RowLayout
					{
						RadioButton { text: qsTr("Relative") ; name: "expectedRelative"; id: expectedRelative; checked: true}

						PercentField
						{
							name: 			"expectedPercentage"
							enabled: 		expectedRelative.checked
							decimals: 		3
							defaultValue: 	0
							visible: 		expectedRelative.checked
							fieldWidth: 	40
						}
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
							text: 		qsTr("Enable")
							name: 		"explanatoryText"
							checked: 	true
						}

						HelpButton
						{
							helpPage:			"Audit/explanatoryText"
							toolTip: 			qsTr("Show explanatory text at each step of the analysis")
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

				RadioButtonGroup
				{
					id: 		cr
					title: 		qsTr("Control Risk")
					name: 		"CR"
					enabled:	!pasteVariables.checked

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
					id: 		planningModel
					title: 		qsTr("Planning Distribution")
					name: 		"planningModel"
					enabled:	!pasteVariables.checked

					RadioButton
					{
						id: 				beta
						text: 			qsTr("Beta")
						name: 			"binomial"
						checked: 		true
					}

					RadioButton
					{
						id: 				gamma
						text: 			qsTr("Gamma")
						name: 			"Poisson"
					}

					RadioButton
					{
						id: 				betaBinomial
						text: 			qsTr("Beta-binomial")
						name: 			"hypergeometric"
					}
				}


				RadioButtonGroup
				{
					id: 		valuta
					title: 		qsTr("Currency")
					name: 		"valuta"
					visible:	monetaryVariable.count > 0 || materialityAbsolute.checked

					RadioButton 	{ text: qsTr("Euro (€)"); 	name: "euroValuta"; 	checked: true; 	id: euroValuta 		}
					RadioButton 	{ text: qsTr("Dollar ($)"); name: "dollarValuta"; 	checked: false; id: dollarValuta	}
					RowLayout
					{
						RadioButton	{ text:	qsTr("Other");		name: "otherValuta"; 	checked: false; id: otherValuta		}
						TextField
						{
							id: 		otherValutaName
							name: 		"otherValutaName"
							fieldWidth: 100
							enabled: 	otherValuta.checked
							visible: 	otherValuta.checked
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
							text: qsTr("Expected evidence ratio")
							name: "expectedEvidenceRatio"
						}

						CheckBox
						{
							text: qsTr("Expected Bayes factor (BF\u208B\u208A)")
							name: "expectedBayesFactor"
						}
					}

					GroupBox
					{
						title: 	qsTr("Tables")

						CheckBox
						{
							text: 		qsTr("Book value descriptives")
							name: 		"bookValueDescriptives"
							enabled: 	monetaryVariable.count > 0
						}

						CheckBox
						{
							text: 		qsTr("Implicit sample")
							name: 		"implicitSampleTable"
						}

						CheckBox
						{
							text: qsTr("Prior and expected posterior descriptives")
							name: "priorStatistics"
						}
					}
				}

				GroupBox
				{
					title: qsTr("Plots")

					CheckBox
					{
						id: 		bookValueDistribution
						enabled: 	monetaryVariable.count > 0
						text: 		qsTr("Book value distribution")
						name: 		"bookValueDistribution"
					}

					CheckBox
					{
						text: 		qsTr("Sample size comparison")
						name: 		"decisionPlot"
					}

					CheckBox
					{
						text: 								qsTr("Implied prior from risk assessments")
						name: 								"priorPlot"
						childrenOnSameRow: 		false

						PercentField
						{
							text: 							qsTr("x-axis limit")
							name: 							"priorPlotLimit"
							defaultValue: 			20
						}

						CheckBox
						{
							text: 							qsTr("Expected posterior")
							name: 							"priorPlotExpectedPosterior"
						}

						CheckBox
						{
							text: 							qsTr("Additional info")
							name: 							"priorPlotAdditionalInfo"
							checked: 						true

							RadioButtonGroup 
							{
								title: 				qsTr("Shade")
								name: 				"shadePrior"

								RadioButton
								{
									text: 			qsTr("Credible region")
									name: 			"shadePriorCredibleRegion"
									checked: 		true
								}

								RadioButton
								{
									text: 			qsTr("Support regions")
									name: 			"shadePriorHypotheses"
								}

								RadioButton
								{
									text: 			qsTr("None")
									name: 			"shadePriorNone"
								}
							}
						}
					}
				}
			}
		}

		Item
		{
			Layout.preferredHeight: toSampling.height
			Layout.fillWidth: 			true
			enabled:								!pasteVariables.checked

			Button
			{
				id:						downloadReportPlanning
				anchors.right:	 		toSampling.left
				anchors.rightMargin:	jaspTheme.generalAnchorMargin
				text:					qsTr("<b>Download Report</b>")
				enabled:				materialityRelative.checked ?
										materialityPercentage.value != "0" 	:
										materialityValue.value 		!= "0" && recordNumberVariable.count > 0 && monetaryVariable.count > 0

				onClicked: 	form.exportResults()
			}

			CheckBox
			{
				id: 			samplingChecked
				name: 			"samplingChecked"
				anchors.right:	toSampling.left
				width:			0
				visible: 		false
				checked: 		false
			}

			Button
			{
				id: 			toSampling
				anchors.right: 	parent.right
				text: 			qsTr("<b>To Selection</b>")
				enabled: 		!samplingChecked.checked && (materialityRelative.checked ?
									materialityPercentage.value != "0" && recordNumberVariable.count > 0 :
									materialityValue.value 		!= "0" && recordNumberVariable.count > 0 && monetaryVariable.count > 0)
				onClicked:
				{
					samplingChecked.checked	= true

					if (monetaryVariable.count == 0)  recordSampling.click()
					if (monetaryVariable.count > 0)   musSampling.click()
					if (monetaryVariable.count == 0)  variableTypeCorrect.click()
					if (monetaryVariable.count > 0)   variableTypeAuditValues.click()
					
					if (betaBinomial.checked)					variableTypeCorrect.click()
				}
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  SELECTION  ----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 		samplingPhase
		text: 		samplingPhase.expanded ? qsTr("<b>2. Selection</b>") : qsTr("2. Selection")
		enabled: 	samplingChecked.checked
		expanded: 	samplingChecked.checked && !executionChecked.checked
		columns: 	1

		VariablesForm
		{
			id: 			variablesFormSampling
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			enabled:		!pasteVariables.checked

			AvailableVariablesList
			{
				name: 			"variablesFormSampling"
				source: 		"variablesFormPlanning"
			}

			AssignedVariablesList
			{
				name: 			"rankingVariable"
				title: 			qsTr("Ranking Variable <i>(optional)</i>")
				singleVariable: true
				allowedColumns: ["scale"]
			}

			AssignedVariablesList
			{
				name: 			"additionalVariables"
				title: 			qsTr("Additional Variables <i>(optional)</i>")
				Layout.preferredHeight: 		140 * preferencesModel.uiScale
				allowedColumns: ["scale", "ordinal", "nominal"]
			}
		}

		Section
		{
			title: qsTr("Advanced Options")

			GridLayout
			{
				columns: 	3
				enabled:	!pasteVariables.checked

				RadioButtonGroup
				{
					id: 		selectionType
					title: 		qsTr("Sampling Units")
					name: 		"selectionType"

					RowLayout
					{
						RadioButton
						{
							id: 		musSampling
							text: 		qsTr("Monetary unit sampling")
							name: 		"musSampling"
							enabled: 	monetaryVariable.count > 0
							checked: 	true
						}

						HelpButton
						{
							helpPage:			"Audit/monetaryUnitSampling"
							toolTip: 			qsTr("Select observations with probability proportional to their value")
						}
					}

					RowLayout
					{
						RadioButton
						{
							id: 		recordSampling
							text: 		qsTr("Record sampling")
							name: 		"recordSampling"
						}

						HelpButton
						{
							toolTip: 			qsTr("Select observations with equal probability")
							helpPage:			"Audit/recordSampling"
						}
					}
				}

				RadioButtonGroup
				{
					id: 		selectionMethod
					title: 		qsTr("Selection Method")
					name: 		"selectionMethod"

					RowLayout
					{
						RadioButton
						{
							id: randomSampling
							text: qsTr("Random sampling")
							name: "randomSampling"
						}

						HelpButton
						{
							toolTip: 			qsTr("Select observations by random sampling")
							helpPage:			"Audit/randomSampling"
						}
					}

					RowLayout
					{
						RadioButton
						{
							id: 		cellSampling
							text: 		qsTr("Cell sampling")
							name: 		"cellSampling"
						}

						HelpButton
						{
							toolTip: 	qsTr("Select observations by cell sampling")
							helpPage:	"Audit/cellSampling"
						}
					}

					RowLayout
					{
						RadioButton
						{
							id: 		systematicSampling
							text: 		qsTr("Fixed interval sampling")
							name: 		"systematicSampling"
							checked: 	true
						}

						HelpButton
						{
							toolTip: 	qsTr("Select observations by fixed interval sampling")
							helpPage:	"Audit/fixedIntervalSampling"
						}
					}
				}

				IntegerField
				{
					id: 						seed
					enabled:				!systematicSampling.checked
					text: 					qsTr("Seed")
					name: 					"seed"
					defaultValue: 	1
					min: 						1
					max: 						999
					fieldWidth: 		60
				}
			}
		}

		Section
		{
			title: qsTr("Tables")

			GridLayout
			{
				GroupBox
				{
					id: 	samplingTables
					title: 	qsTr("Tables")

					CheckBox { text: qsTr("Display selected observations"); 	name: "displaySample"									}
					CheckBox { text: qsTr("Selection descriptives"); 			name: "sampleDescriptives"; 	id: sampleDescriptives	}

					GridLayout
					{
						Layout.leftMargin: 20 * preferencesModel.uiScale

						ColumnLayout
						{
							spacing: 		5 * preferencesModel.uiScale

							CheckBox { text: qsTr("Mean"); 				name: "mean"; 	enabled: sampleDescriptives.checked; checked: true	}
							CheckBox { text: qsTr("Median"); 			name: "median"; enabled: sampleDescriptives.checked; checked: true	}
							CheckBox { text: qsTr("Std. deviation"); 	name: "sd"; 	enabled: sampleDescriptives.checked; checked: true	}
							CheckBox { text: qsTr("Variance"); 			name: "var"; 	enabled: sampleDescriptives.checked					}
						}

						ColumnLayout
						{
							spacing: 	5 * preferencesModel.uiScale

							CheckBox { text: qsTr("Minimum"); 	name: "min"; 	enabled: sampleDescriptives.checked	}
							CheckBox { text: qsTr("Maximum"); 	name: "max"; 	enabled: sampleDescriptives.checked	}
							CheckBox { text: qsTr("Range"); 	name: "range"; 	enabled: sampleDescriptives.checked	}
						}
					}
				}
			}
		}

		Item
		{
			Layout.preferredHeight: toExecution.height
			Layout.fillWidth: 			true
			enabled:								!pasteVariables.checked

			Button
			{
				anchors.left: 	parent.left
				text: 			qsTr("<b>Reset Workflow</b>")
				onClicked: 		form.reset()
			}

			Button
			{
				id:						downloadReportSelection
				enabled:				materialityRelative.checked ? (materialityPercentage.value == "0" ? false : true) : (materialityValue.value == "0" ? false : true)
				anchors.right:			toExecution.left
				anchors.rightMargin:	jaspTheme.generalAnchorMargin
				text:					qsTr("<b>Download Report</b>")
				onClicked:				form.exportResults()
			}

			CheckBox
			{
				id: 				executionChecked
				anchors.right: 		toExecution.left
				width: 				height
				visible: 			false
				name: 				"executionChecked"
				checked: 			false
			}

			Button
			{
				id: 				toExecution
				anchors.right: 		parent.right
				text: 				qsTr("<b>To Execution</b>")
				enabled:			!executionChecked.checked
				onClicked:
				{
					executionChecked.checked = true

					if (monetaryVariable.count == 0 || betaBinomial.checked)	variableTypeCorrect.click()
					else																											variableTypeAuditValues.click()
				}
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  EXECUTION  ----------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 		executionPhase
		text: 		executionPhase.expanded ? qsTr("<b>3. Execution</b>") : qsTr("3. Execution")
		expanded: 	executionChecked.checked && !evaluationChecked.checked
		enabled: 	executionChecked.checked
		columns: 	1

		Item
		{
			Layout.preferredHeight: selectHowToAnalyseObservations.height
			Layout.fillWidth: 			true

			Label
			{
				id: 						selectHowToAnalyseObservations
				anchors.horizontalCenter: 	parent.horizontalCenter
				text: 						qsTr("<b>How would you like to evaluate your observations?</b>")
			}
		}

		Item
		{
			Layout.preferredHeight: variableType.height
			Layout.fillWidth: 			true

			RadioButtonGroup
			{
				id: 						variableType
				name: 						"variableType"
				title: 						qsTr("")
				anchors.horizontalCenter: 	parent.horizontalCenter
				enabled:					!pasteVariables.checked

				RowLayout
				{
					spacing: 200 * preferencesModel.uiScale

					RowLayout
					{
						RadioButton
						{
							id: 		variableTypeAuditValues
							text: 		qsTr("Audit values")
							name: 		"variableTypeAuditValues"
							checked: 	true
							enabled: 	monetaryVariable.count > 0 && !betaBinomial.checked
						}

						HelpButton { toolTip: qsTr("Adds a column to specify the audit value of the observations"); helpPage: "?" }
					}

					RowLayout
					{
						RadioButton
						{
							id: 		variableTypeCorrect
							text: 		qsTr("Correct / Incorrect")
							name: 		"variableTypeCorrect"
							checked: 	false
							enabled: 	true
						}

						HelpButton { toolTip:	qsTr("Adds a column to specify the observations as correct (0) or incorrect (1)"); helpPage: "?" }
					}
				}
			}
		}

		Divider { width: parent.width }

		RowLayout
		{
			GroupBox
			{
				id: 		groupBoxVariableNames
				enabled:	!pasteVariables.checked

				ComputedColumnField
				{
					id: 		sampleFilter
					name: 		"sampleFilter"
					text: 		"Column name selection result: "
					fieldWidth: 120
				}

				ComputedColumnField
				{
					id: 		variableName
					name: 		"variableName"
					text: 		"Column name audit result: "
					fieldWidth: 120
				}
			}

			Item
			{
				Layout.preferredHeight: groupBoxVariableNames.height
				Layout.fillWidth: 			true

				CheckBox
				{
					id: 				pasteVariables
					anchors.right: 		pasteButton.left
					width: 				height
					visible: 			false
					name: 				"pasteVariables"
					checked: 			false
				}

				Button
				{
					id: 			pasteButton
					text: 			qsTr("<b>Fill Variables</b>")
					enabled: 		sampleFilter.value != "" && variableName.value != "" && !pasteVariables.checked
					onClicked:
					{
						pasteVariables.checked 		= true
						performAuditTable.colName   = variableName.value
						performAuditTable.filter 	= sampleFilter.value + " > 0"
						performAuditTable.extraCol	= sampleFilter.value
					}

				}
			}
		}

		Item {
			Layout.preferredHeight: performAuditText.height
			Layout.fillWidth: 			true

			Label
			{
				id: 						performAuditText
				anchors.horizontalCenter: 	parent.horizontalCenter
				text: 						variableTypeAuditValues.checked ? qsTr("<b>Annotate your observations with their audit values.</b>") : qsTr("<b>Annotate your observations with 0 (correct) or 1 (incorrect).</b>")
				visible: 					pasteVariables.checked
			}
		}

		Section
		{
			id: 					executeAuditSection
			title:					"Data Entry"
			expanded:				pasteVariables.checked
			enabled:				pasteVariables.checked

			TableView
			{
				id:									performAuditTable
				name:								"performAudit"
				Layout.fillWidth: 	true
				modelType:					"FilteredDataEntryModel"
        source:     				["recordNumberVariable", "monetaryVariable", "additionalVariables"]
        colName:    				"Filter"
				itemType:						"double"
			}
		}

		Item
		{
			Layout.preferredHeight: toEvaluation.height
			Layout.fillWidth: 			true
			enabled:								!evaluationChecked.checked

			Button
			{
				anchors.left: 	parent.left
				text: 			qsTr("<b>Reset Workflow</b>");
				onClicked: 		form.reset()
			}

			CheckBox
			{
				id: 				evaluationChecked
				anchors.right: 		toEvaluation.left
				width: 				height
				visible: 			false
				name: 				"evaluationChecked"
				checked: 			false
			}

			Button
			{
				id: 				toEvaluation
				enabled: 			pasteVariables.checked
				anchors.right: 		parent.right
				text: 				qsTr("<b>To Evaluation</b>")
				onClicked:
				{
					executionPhase.expanded 		= false
					executeAuditSection.expanded	= false
					evaluationChecked.checked 		= true

					if (beta.checked) 						betaBound.click()
					if (betaBinomial.checked) 		betabinomialBound.click()
					if (gamma.checked) 						gammaBound.click()

					if (recordSampling.checked && variableTypeAuditValues.checked)
						regressionBound.click()

				}
			}
		}
	}

	// --------------------------------------------------------------------------------------------------------------------------------------------
	// ---------------------------------------------------  EVALUATION  ---------------------------------------------------------------------------
	// --------------------------------------------------------------------------------------------------------------------------------------------

	Section
	{
		id: 			evaluationPhase
		text: 			evaluationPhase.expanded ? qsTr("<b>4. Evaluation</b>") : qsTr("4. Evaluation")
		expanded: 		evaluationChecked.checked
		enabled: 		evaluationChecked.checked
		columns: 		1

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight

			AvailableVariablesList
			{
				name: 		"evaluationVariables"
				source: 	"variablesFormPlanning"
			}

			AssignedVariablesList
			{
				id: 			auditResult
				name: 			"auditResult"
				title: 			qsTr("Audit Result")
				singleVariable: true
				allowedColumns: ["nominal", "scale"]
			}
		}

		Section
		{
			title: 		qsTr("Advanced Options")
			columns: 	1

			GridLayout
			{
				columns: 2

				RadioButtonGroup
				{
					title: 		qsTr("Estimation Method")
					name: 		"estimator"

					RadioButton
					{
						id: 			betaBound
						name: 		"betaBound"
						text: 		qsTr("Beta")
						visible: 	!regressionBound.visible && beta.checked
					}

					RadioButton
					{
						id: 			gammaBound
						name: 		"gammaBound"
						text: 		qsTr("Gamma")
						visible: 	!regressionBound.visible && gamma.checked
					}

					RadioButton
					{
						id: 			betabinomialBound
						name: 		"betabinomialBound"
						text: 		qsTr("Beta-binomial")
						visible: 	!regressionBound.visible && betaBinomial.checked
					}

					RadioButton
					{
						id: 		coxAndSnellBound
						name: 		"coxAndSnellBound"
						text: 		qsTr("Cox and Snell")
						visible: 	musSampling.checked && variableTypeAuditValues.checked
					}

					RadioButton
					{
						id: 			regressionBound
						name: 		"regressionBound"
						text: 		qsTr("Regression")
						visible: 	recordSampling.checked && variableTypeAuditValues.checked && evaluationChecked.checked
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
							visible: 	!regressionBound.visible
						}

						CheckBox
						{
							text: qsTr("Bayes factor (BF\u208B\u208A)")
							name: "bayesFactor"
							visible: 	!regressionBound.visible
						}
					}

					GroupBox
					{
						title: qsTr("Tables")
						visible: !regressionBound.visible

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
						visible: 					!regressionBound.visible

						PercentField
						{
							id: 						priorAndPosteriorPlotLimit
							text: 					qsTr("x-axis limit")
							defaultValue: 	20
							name: 					"priorAndPosteriorPlotLimit"
							visible:				!regressionBound.visible
						}

						CheckBox
						{
							id: 						priorAndPosteriorPlotExpectedPosterior
							text: 					qsTr("Expected posterior")
							name: 					"priorAndPosteriorPlotExpectedPosterior"
							visible:				!regressionBound.visible
						}

						CheckBox
						{
							id: 						priorAndPosteriorPlotAdditionalInfo
							text: 					qsTr("Additional info")
							name: 					"priorAndPosteriorPlotAdditionalInfo"
							checked: 				true
							visible:				!regressionBound.visible

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
				anchors.right: 	parent.right
				text: 			qsTr("<b>Download Report</b>")
				enabled: 		auditResult.count > 0
				onClicked:
				{
					evaluationPhase.expanded = false
					form.exportResults()
				}
			}
		}
	}
}
