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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0

Section
{
	expanded: false
	title: qsTr("Inference")
	columns: 2

	property alias plotsPredictionsObserved:	plotsPredictionsObserved.label
	property alias plotsPosteriorObserved:		plotsPosteriorObserved.label
	property alias plotsBothSampleProportion:	plotsBothSampleProportion.label
	property alias bfTypevsName: 				bfTypevsName.source

	Group
	{
		Layout.columnSpan:	2
		columns:			2
		title:	qsTr("Bayes Factor")

		RadioButtonGroup
		{
			name:	"bfType"
			RadioButton
			{
				value:	"inclusion"
				label:	qsTr("vs. rest")
				checked: true
			}

			RadioButton
			{
				value:	"best"
				label:	qsTr("vs. best")
			}

			RadioButton
			{
				name:	"vs"
				label:	qsTr("vs.")
				childrenOnSameRow: true

				DropDown
				{
					name:				"bfTypevsName"
					id:					bfTypevsName
					indexDefaultValue:	0
				}
			}
		}

		RadioButtonGroup
		{
			name: "bayesFactorType"

			RadioButton { label: qsTr("BF\u2081\u2080")			; name: "BF10"; checked: true}
			RadioButton { label: qsTr("BF\u2080\u2081")			; name: "BF01"}
			RadioButton { label: qsTr("log(BF\u2081\u2080)")	; name: "LogBF10"}
		}
	}


	CheckBox
	{
		name:		"plotsPrior"
		label:		qsTr("Prior distribution")
		checked:	false

		RadioButtonGroup
		{
			name: "plotsPriorType"

			RadioButton
			{
				value: "conditional"
				label: qsTr("Conditional")
				checked: true

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPriorEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPriorCI"
					label: qsTr("CI")
					id: plotsPriorCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorTypeCI"
						label: ""
						values: ["central", "HPD", "custom"]
						id: plotsPriorTypeCI
					}
				}

				Group
				{
					columns: 2
					CIField
					{
						visible: plotsPriorTypeCI.currentText == "central" |
								 plotsPriorTypeCI.currentText == "HPD"
						enabled: plotsPriorCI.checked
						name: "plotsPriorCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsPriorTypeCI.currentText == "custom"
						enabled: plotsPriorCI.checked
						name: "plotsPriorLower"
						label: qsTr("Lower")
						id: plotsPriorLower
						fieldWidth: 50
						defaultValue: 0.25; min: 0; max: plotsPriorUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPriorTypeCI.currentText == "custom"
						enabled: plotsPriorCI.checked
						name: "plotsPriorUpper"
						label: qsTr("Upper")
						id: plotsPriorUpper
						fieldWidth: 50
						defaultValue: 0.75; min: plotsPriorLower.value; max: 1; inclusive: JASP.MinMax
					}
				}
			}

			RadioButton
			{
				value: "joint"
				label: qsTr("Joint")

				RadioButtonGroup
				{
					name:  "plotsPriorJointType"

					RadioButton
					{
						value: "overlying"
						label: qsTr("Overlying")
						checked: true
					}

					RadioButton
					{
						value: "stacked"
						label: qsTr("Stacked")
					}

				}

			}

			RadioButton
			{
				value: "marginal";
				label: qsTr("Marginal")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPriorMarginalEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorMarginalEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPriorMarginalCI"
					label: qsTr("CI")
					id: plotsPriorMarginalCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorMarginalType"
						label: ""
						values: ["central", "HPD", "custom"]
						id: plotsPriorMarginalType
					}
				}

				Group
				{
					columns: 2
					CIField{
						visible: plotsPriorMarginalType.currentText == "central" |
								 plotsPriorMarginalType.currentText == "HPD"
						enabled: plotsPriorMarginalCI.checked
						name: "plotsPriorMarginalCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsPriorMarginalType.currentText == "custom"
						enabled: plotsPriorMarginalCI.checked
						name: "plotsPriorMarginalLower"
						label: qsTr("Lower")
						id: plotsMarginalPriorLower
						fieldWidth: 50
						defaultValue: 0.25; min: 0; max: plotsPriorMarginalUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPriorMarginalType.currentText == "custom"
						enabled: plotsPriorMarginalCI.checked
						name: "plotsPriorMarginalUpper"
						label: qsTr("Upper")
						id: plotsPriorMarginalUpper
						fieldWidth: 50
						defaultValue: 0.75; min: plotsPriorMarginalLower.value; max: 1; inclusive: JASP.MinMax
					}
				}


			}

		}
	}


	CheckBox
	{
		name:		"plotsPredictions"
		label:		qsTr("Prior predictive distribution")
		checked:	false

		RadioButtonGroup
		{
			name: "plotsPredictionType"

			RadioButton
			{
				value: "conditional"
				label: qsTr("Conditional")
				checked: true

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPredictionEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPredictionEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPredictionCI"
					label: qsTr("CI")
					id: plotsPredictionCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPredictionTypeCI"
						label: ""
						values: ["central", "HPD", "custom"]
						id: plotsPredictionTypeCI
					}
				}

				Group
				{
					columns: 2

					CIField
					{
						visible: plotsPredictionTypeCI.currentText == "central" |
								 plotsPredictionTypeCI.currentText == "HPD"
						enabled: plotsPredictionCI.checked
						name: "plotsPredictionCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					IntegerField
					{
						visible: plotsPredictionTypeCI.currentText == "custom"
						enabled: plotsPredictionCI.checked
						name: "plotsPredictionLower"
						label: qsTr("Lower")
						id: plotsPredictionLower
						fieldWidth: 50
						defaultValue: 0; min: 0; max: plotsPredictionUpper.value; inclusive: JASP.MinMax
					}

					IntegerField
					{
						visible: plotsPredictionTypeCI.currentText == "custom"
						enabled: plotsPredictionCI.checked
						name: "plotsPredictionUpper"
						label: qsTr("Upper")
						id: plotsPredictionUpper
						fieldWidth: 50
						defaultValue: 1
						min: plotsPredictionLower.value; inclusive: JASP.MinMax
					}

				}
			}

			RadioButton
			{
				value: "joint"
				label: qsTr("Joint")

				RadioButtonGroup
				{
					name: "plotsPredictionJointType"

					RadioButton
					{
						value: "overlying"
						label: qsTr("Overlying")
						checked: true
					}

					RadioButton
					{
						value: "stacked"
						label: qsTr("Stacked")
					}

				}

			}

			RadioButton
			{
				value: "marginal";
				label: qsTr("Marginal")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPredictionMarginalEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPredictionMarginalEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPredictionMarginalCI"
					label: qsTr("CI")
					id: plotsPredictionMarginalCI
					childrenOnSameRow: true
				
					DropDown
					{
						name: "plotsPredictionMarginalTypeCI"
						label: ""
						values: ["central", "HPD", "custom"]
						id: plotsPredictionMarginalTypeCI
					}
				}

				Group
				{
					columns: 2
					CIField
					{
						visible: plotsPredictionMarginalTypeCI.currentText == "central" |
								 plotsPredictionMarginalTypeCI.currentText == "HPD"
						enabled: plotsPredictionMarginalCI.checked
						name: "plotsPredictionMarginalCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsPredictionMarginalTypeCI.currentText == "custom"
						enabled: plotsPredictionMarginalCI.checked
						name: "plotsPredictionMarginalLower"
						label: qsTr("Lower")
						id: plotsMarginalPredictionLower
						fieldWidth: 50
						defaultValue: 0; min: 0; max: plotsPredictionMarginalUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPredictionMarginalTypeCI.currentText == "custom"
						enabled: plotsPredictionMarginalCI.checked
						name: "plotsPredictionMarginalUpper"
						label: qsTr("Upper")
						id: plotsPredictionMarginalUpper
						fieldWidth: 50
						defaultValue: 1; min: plotsPredictionMarginalLower.value; inclusive: JASP.MinOnly
					}
				}
			}

		}

		CheckBox
		{
			name:		"plotsPredictionsObserved"
			id:			plotsPredictionsObserved
			label:		qsTr("Observed proportion")
			checked:	false
		}

		CheckBox
		{
			name:	"predictionPlotTable"
			label:	qsTr("Predictions table")
		}
	}


	CheckBox
	{
		Layout.columnSpan: 2
		name:		"plotsPredictiveAccuracy"
		label:		qsTr("Predictive accuracy")
		checked:	false

		RadioButtonGroup
		{
			name: "plotsPredictiveAccuracyType"
			RadioButton { value: "conditional"; label: qsTr("Conditional"); checked: true}
			RadioButton { value: "joint";		label: qsTr("Joint")}
			RadioButton { value: "marginal"; 	label: qsTr("Normalized")}

		}
	}


	CheckBox
	{
		name: "plotsPosterior"; label: qsTr("Posterior distribution"); checked: false;

		RadioButtonGroup
		{
			name: "plotsPosteriorType"
			RadioButton
			{
				checked: true
				value: "conditional"
				label: qsTr("Conditional")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPosteriorEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPosteriorCI"
					label: qsTr("CI")
					id: plotsPosteriorCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorTypeCI"
						label: ""
						values: ["central", "HPD", "custom","support"]
						id: plotsPosteriorTypeCI
					}
				}

				Group
				{
					columns: 2
					CIField
					{
						visible: plotsPosteriorTypeCI.currentText == "central" |
								 plotsPosteriorTypeCI.currentText == "HPD"
						enabled: plotsPosteriorCI.checked
						name: "plotsPosteriorCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsPosteriorTypeCI.currentText == "custom"
						enabled: plotsPosteriorCI.checked
						name: "plotsPosteriorLower"
						label: qsTr("Lower")
						id: plotsPosteriorLower
						fieldWidth: 50
						defaultValue: 0; min: 0; max: plotsPosteriorUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPosteriorTypeCI.currentText == "custom"
						enabled: plotsPosteriorCI.checked
						name: "plotsPosteriorUpper"
						label: qsTr("Upper")
						id: plotsPosteriorUpper
						fieldWidth: 50
						defaultValue: 1; min: plotsPosteriorLower.value; max: 1; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPosteriorTypeCI.currentText == "support"
						enabled: plotsPosteriorCI.checked
						name: "plotsPosteriorBF"
						label: qsTr("BF")
						fieldWidth: 50
						defaultValue: 1; min: 0; inclusive: JASP.None
					}
				}

			}

			RadioButton
			{
				value: "joint"
				label: qsTr("Joint")

				RadioButtonGroup
				{
					name: "plotsPosteriorJointType"

					RadioButton
					{
						value: "overlying"
						label: qsTr("Overlying")
						checked: true
					}

					RadioButton
					{
						value: "stacked"
						label: qsTr("Stacked")
					}

				}

			}

			RadioButton
			{
				value: "marginal";
				label: qsTr("Marginal")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPosteriorMarginalEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorMarginalEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPosteriorMarginalCI"
					label: qsTr("CI")
					id: plotsPosteriorMarginalCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorMarginalType"
						label: ""
						values: ["central", "HPD", "custom","support"]
						id: plotsPosteriorMarginalType
					}
				}

				Group
				{
					columns: 2
					CIField
					{
						visible: plotsPosteriorMarginalType.currentText == "central" |
								 plotsPosteriorMarginalType.currentText == "HPD"
						enabled: plotsPosteriorMarginalCI.checked
						name: "plotsPosteriorMarginalCoverage"
						label: qsTr("Mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsPosteriorMarginalType.currentText == "custom"
						enabled: plotsPosteriorMarginalCI.checked
						name: "plotsPosteriorMarginalLower"
						label: qsTr("Lower")
						id: plotsPosteriorMarginalLower
						fieldWidth: 50
						defaultValue: 0.25; min: 0; max: plotsPosteriorMarginalUpper.value; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPosteriorMarginalType.currentText == "custom"
						enabled: plotsPosteriorMarginalCI.checked
						name: "plotsPosteriorMarginalUpper"
						label: qsTr("Upper")
						id: plotsPosteriorMarginalUpper
						fieldWidth: 50
						defaultValue: 0.75; min: plotsPosteriorMarginalLower.value; max: 1; inclusive: JASP.MinMax
					}

					DoubleField
					{
						visible: plotsPosteriorMarginalType.currentText == "support"
						enabled: plotsPosteriorMarginalCI.checked
						name: "plotsPosteriorMarginalBF"
						label: qsTr("BF")
						fieldWidth: 50
						defaultValue: 1; min: 0; inclusive: JASP.None
					}
				}


			}

		}

		CheckBox
		{
			name:	"plotsPosteriorObserved"
			id:		plotsPosteriorObserved
			label:	qsTr("Observed proportion")
			checked: false
		}
	}


	CheckBox
	{
		name: "plotsBoth"
		label: qsTr("Prior and posterior distribution")
		checked: false

		RadioButtonGroup
		{
			name: "plotsBothType"
			RadioButton
			{
				checked: true
				value: "conditional"
				label: qsTr("Conditional")
			}

			RadioButton
			{
				value: "joint";
				label: qsTr("Joint")
			}

			RadioButton
			{
				value: "marginal";
				label: qsTr("Marginal")
			}

		}

		CheckBox
		{
			name:		"plotsBothSampleProportion"
			id:			plotsBothSampleProportion
			label:		qsTr("Observed proportion")
			checked:	false
		}
	}

}
