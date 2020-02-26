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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
 	id: form

//// Variable inputs ////
	VariablesForm
	{
		AvailableVariablesList {name: "variablesList"}

		AssignedVariablesList
		{
			name: 			"effectSize"
			title: 			qsTr("Effect Size")
			singleVariable: true
			allowedColumns: ["scale"]
		}

	AssignedVariablesList
		{
			id: 			standardError
			enabled: 		confidenceInterval.count < 1 // Only if no confidence interval input
			name: 			"standardError"
			title: 			qsTr("Effect Size Standard Error")
			singleVariable: true
			allowedColumns: ["scale"]
		}

	AssignedPairsVariablesList
		{
			id: 			confidenceInterval
			enabled: 		standardError.count == 0 // Only if no standard error input (only one of the two is necessary)
			name: 			"confidenceInterval"
			title: 			qsTr("95% CI Lower and Upper Bound")
			singleVariable: true
			allowedColumns: ["scale"]
		}

	AssignedVariablesList
		{
			name: 			"studyLabels"
			title: 			qsTr("Study Labels")
			singleVariable:	true
			allowedColumns: ["nominalText"]
		}
	}

	GridLayout
	{
		columns: 2

//// Analysis choices ////
		RadioButtonGroup
		{
			name: 	"modelSpecification"
			title: 	qsTr("Model")

			RadioButton
			{
				id: 				checkFE
				value: 				"FE"
				label: 				qsTr("Fixed effects")
				onCheckedChanged:	{
					if (checked)
					{
						priorModelProbabilityGroup.resetHypotheses()
						forestObserved.click()
					}
				}
			}

			RadioButton
			{
				id: 				checkRE
				value: 				"RE"
				label: 				qsTr("Random effects")
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()
			}

			RadioButton
			{
				id: 				checkBMA
				value: 				"BMA"
				label: 				qsTr("Model averaging")
				checked: 			true
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()
			}

			RadioButton
			{
				id: 				checkCRE
				value: 				"CRE"
				label: 				qsTr("Constrained random effects")
				onCheckedChanged:	if(checked) priorModelProbabilityGroup.resetHypotheses()

				// Constrain effect sizes to be all positive or all negative
				RadioButtonGroup
				{
					name: "direction"
					columns: 2

					RadioButton
					{
						id: 		checkPos
						value: 		"allPos"
						label: 		qsTr("All positive")
						checked:	true
					}

					RadioButton
					{
						id: 	checkNeg
						value: 	"allNeg"
						label: 	qsTr("All negative")
					}
				}
			}
		}

//// Tables ////
		Group
		{
		    title: qsTr("Table")

			CheckBox
			{
				name: 	"postTable";
				label: 	qsTr("Model probabilities")
			}

			CheckBox
			{
				name: 	"esTable";
				label: 	qsTr("Effect sizes per study")
			}
		}

	//// BF ////
		BayesFactorType { }


	//// Priors ////
		Section
		{
			title: 		qsTr("Prior")
			columns:	1

			Group
			{
				columns: 2

			RadioButtonGroup
			{
				title: 	qsTr("Effect size")
				name: 	"priorES"

				RadioButton
				{
					id: 				cauchyInformative
					label: 				qsTr("Cauchy")
					name: 				"cauchy"
					checked: 			true
					childrenOnSameRow: 	true

					DoubleField
					{
						label: 			qsTr("location:")
						name: 			"informativeCauchyLocation"
						visible: 		cauchyInformative.checked
						defaultValue: 	0
						negativeValues: true
					}

					DoubleField
					{
						label: 			qsTr("scale:");
						name: 			"informativeCauchyScale"
						visible: 		cauchyInformative.checked
						defaultValue: 	0.707
						fieldWidth: 	50
					}


				}

				RadioButton
				{
					id: 				normalInformative
					label: 				qsTr("Normal")
					name: 				"normal"
					childrenOnSameRow:	true

					DoubleField
					{
						label: 			qsTr("mean:")
						name: 			"informativeNormalMean"
						visible: 		normalInformative.checked
						defaultValue: 	0
						negativeValues: true
					}

					DoubleField
					{
						label: 			qsTr("std:")
						name: 			"informativeNormalStd"
						visible: 		normalInformative.checked
						defaultValue: 	0.707
						fieldWidth: 	50
					}


				}

				RadioButton
				{
					id: 				tInformative
					label: 				qsTr("t")
					name: 				"t"
					childrenOnSameRow: 	true

					DoubleField
					{
						label: 			qsTr("location:")
						name: 			"informativeTLocation"
						visible: 		tInformative.checked
						defaultValue: 	0
						negativeValues: true
					}

					DoubleField
					{
						label: 			qsTr("scale:")
						name: 			"informativeTScale"
						visible: 		tInformative.checked
						defaultValue: 	0.707
						fieldWidth: 	50
					}

					IntegerField
					{
						label: 			qsTr("df:");
						name: 			"informativeTDf";
						visible: 		tInformative.checked;
						min:			1
						defaultValue: 	1
					}


				}
			}
			Group
			{
				title: qsTr("Truncation")
				CheckBox
				{
					name: 				"checkLowerPrior"
					childrenOnSameRow: 	true
					checked: 			checkCRE.checked && checkPos.checked

					DoubleField
					{
						id: 			lowerTT
						name: 			"lowerTrunc"
						label: 			qsTr("Lower bound:")
						fieldWidth: 	50
						negativeValues: !checkCRE.checked && checkPos.checked
						defaultValue: 	0
						max: 			checkCRE.checked && checkNeg.checked ? 0 : Infinity
					}
				}

				CheckBox
				{
					name: 				"checkUpperPrior"
					childrenOnSameRow: 	true
					checked: 			checkCRE.checked && checkNeg.checked

					DoubleField
					{
						id: upperTT
						name: 			"upperTrunc"
						label: 			qsTr("Upper bound:")
						fieldWidth: 	50
						negativeValues: !checkCRE.checked && checkPos.checked
						defaultValue: 	0
						max: 			checkCRE.checked && checkNeg.checked ? 0 : Infinity
					}
				}
			}

		}

			RadioButtonGroup
			{
				enabled:	checkRE.checked || checkCRE.checked || checkBMA.checked
				title: 		qsTr("Heterogeneity (Between study SD)")
				name: 		"priorSE"

				RadioButton
				{
					id: 				igInformative
					name: 				"inverseGamma"
					label: 				qsTr("Inverse gamma")
					childrenOnSameRow: 	true
					checked: 			true

					DoubleField
					{
						label: 			qsTr("shape:")
						name: 			"inverseGammaShape"
						visible: 		igInformative.checked
						defaultValue: 	1
						fieldWidth: 	50
					}

					DoubleField
					{
						label: 			qsTr("scale:")
						name: 			"inverseGammaScale"
						visible: 		igInformative.checked
						defaultValue:	0.15
						fieldWidth: 	50
					}
				}

				RadioButton
				{
					id: 				halfTInformative
					name: 				"halfT"
					label: 				qsTr("Half t")
					childrenOnSameRow: 	true

					DoubleField
					{
						label: 			qsTr("scale:")
						name: 			"informativehalfTScale"
						visible: 		halfTInformative.checked
						defaultValue: 	0.707
						fieldWidth: 	50
					}

					IntegerField
					{
						label: 			qsTr("df:")
						name: 			"informativehalfTDf"
						visible: 		halfTInformative.checked
						min:			1
						defaultValue: 	1
					}
				}
			}

			CheckBox
			{
				name: 	"plotPrior"
				label: 	qsTr("Plot prior(s)")
			}
		}

	//// Plots section ////
		Section
	    {
			columns: 	1
			title: 		qsTr("Plots")

			Group
			{
				columns: 2

				CheckBox
				{
					id: 		checkForest
					name: 		"checkForestPlot"
					label: 		qsTr("Forest plot")
					checked: 	true

					RadioButtonGroup
					{
						name: "forestPlot"

						RadioButton
						{
							id:		forestObserved
							name: 		"plotForestObserved"
							label: 		qsTr("Observed")
							checked: 	true
						}

						RadioButton
						{
							enabled: 	!checkFE.checked
							name: 		"plotForestEstimated"
							label: 		qsTr("Estimated")
						}

						RadioButton
						{
							enabled: 	!checkFE.checked
							name: 		"plotForestBoth"
							label: 		qsTr("Both")
						}
					}
				}

				RadioButtonGroup
				{
					name: 		"orderForest"
					title: 		qsTr("Order")
					enabled: 	checkForest.checked

					RadioButton
					{
						name: 	"ascendingForest"
						label: 	qsTr("Ascending")
					}

					RadioButton
					{
						name: 	"descendingForest"
						label: 	qsTr("Descending")
					}

					RadioButton
					{
						name: 	"labelForest"
						label: 	qsTr("Row order")
					}
				}
			}

			CheckBox
			{
				name: 	"plotCumForest"
				label: 	qsTr("Cumulative forest plot")
			}

			CheckBox
			{
				name: 		"plotPosterior"
				label: 		qsTr("Prior and posterior")

				CheckBox
				{
					name: "addInfo"
					label: qsTr("Additional info")
				}

				CheckBox
				{
					name: "addLines"
					enabled: checkBMA.checked || checkCRE.checked
					label: qsTr("Add fixed and random effects posterior")
				}
				CheckBox
				{
					name: 	"shade"
					label: 	qsTr("Shade 95% CI")
				}
			}

			Group
			{
				title: qsTr("Sequential plot")

				CheckBox
				{
					name: 	"plotSequential"
					label: 	qsTr("Bayes factors")
				}

				CheckBox
				{
					name: 	"plotSeqPM"
					label: 	qsTr("Posterior model probabilities")
				}
			}
	    }

	//// Advanced section for prior model probabilities sampling settings ////
	    Section
	    {
			columns: 	2
			title: 		qsTr("Advanced")

	      	Group
	      	{
				id:			priorModelProbabilityGroup
	        	enabled: 	checkFE.checked || checkRE.checked || checkBMA.checked
	        	title: 		qsTr("Prior model probability")

				property double fixedEffectsHypothesisVal:	checkFE.checked ? 0.5 :
																checkRE.checked ? 0 :
																	checkBMA.checked ? 0.25 : 0

				property double randomEffectsHypothesisVal:	checkFE.checked ? 0 :
												              	checkRE.checked ? 0.5 :
												              		checkBMA.checked ? 0.25 : 0

				function resetHypotheses() {
					priorH0FE.value = fixedEffectsHypothesisVal
					priorH1FE.value = fixedEffectsHypothesisVal
					priorH0RE.value = randomEffectsHypothesisVal
					priorH1RE.value = randomEffectsHypothesisVal

					priorH0FE.editingFinished()
					priorH1FE.editingFinished()
					priorH0RE.editingFinished()
					priorH1RE.editingFinished()
				}

		        Group
		        {
		          	enabled: 			checkFE.checked || checkBMA.checked
		          	title: 				qsTr("Fixed effects")

				  	onEnabledChanged: 	if(enabled) priorModelProbabilityGroup.resetHypotheses()

					DoubleField
					{
						id: 			priorH0FE
						name: 			"priorH0FE"
						label: 			"H\u2080"
						defaultValue: 	priorModelProbabilityGroup.fixedEffectsHypothesisVal
					}

					DoubleField
					{
						id: 			priorH1FE
						name: 			"priorH1FE"
						label: 			"H\u2081"
						defaultValue: 	priorModelProbabilityGroup.fixedEffectsHypothesisVal
					}
		        }

		        Group
		        {
		          	title: 				qsTr("Random effects")
		          	enabled: 			checkRE.checked || checkBMA.checked
					onEnabledChanged: 	if(enabled) priorModelProbabilityGroup.resetHypotheses()

		          	DoubleField
		          	{
						id: 			priorH0RE
						name: 			"priorH0RE"
						label: 			"H\u2080"
						defaultValue: 	priorModelProbabilityGroup.randomEffectsHypothesisVal
		          	}

					DoubleField
					{
						id: 			priorH1RE
		            	name: 			"priorH1RE"
		            	label: 			"H\u2081"
						defaultValue: 	priorModelProbabilityGroup.randomEffectsHypothesisVal
		          	}
		        }
			}

			Group
			{
				enabled: !checkCRE.checked

				Group
				{
					title: 		qsTr("Estimation settings (MCMC)")
					columns: 	2

					IntegerField
					{
						label: 			qsTr("iterations:")
						name: 			"iterMCMC"
						defaultValue: 	!checkCRE.checked ? 2000 : 10000
						min:			100
						max: 			1000000
						fieldWidth: 	100
					}

					IntegerField
					{
						label: 			qsTr("chains:")
						name: 			"chainsMCMC"
						defaultValue: 	4
						min:			1
						max: 			10
						fieldWidth: 	50
					}
				}

				Group
				{
					title: qsTr("Bayes factor computation")

					RadioButtonGroup
					{
						name: "BFComputation"

						RadioButton
						{
							name: 		"integration";
							label: 		qsTr("Integration")
							checked: 	true
						}

						RadioButton
						{
							id: 				"bridge"
							name: 				"bridgeSampling"
							label: 				qsTr("Bridge sampling")
							childrenOnSameRow: 	true

							IntegerField
							{
								label:  		qsTr("iterations:")
								name:     		"iterBridge"
								visible:      	bridge.checked
								defaultValue: 	5000
								max:			1000000
								fieldWidth: 	100
							}
						}
					}
				}
			}
		}
 	}
}
