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
import JASP.Theme 1.0

Section
{
	expanded: false
	title: qsTr("Posterior prediction")

	property alias predictionPlotProp: predictionPlotProp.label

	Group
	{
		IntegerField
		{
			name: "predictionN"
			label: qsTr("Future observations")
			id: predictionN
			min: 1
			defaultValue: 1
		}

		CheckBox
		{
			name: "predictionTable"
			label: qsTr("Summary")

			DropDown
			{
				label:	qsTr("Point estimate")
				name: "predictionTableEstimate"
				values: ["mean", "median", "mode"]
			}
		}


		Group
		{
			title: qsTr("Plots")

			DropDown
			{
				name: "colorPalettePrediction"
				label: qsTr("Color palette")
				indexDefaultValue: 0
				values:
					[
					{ label: qsTr("Colorblind"),		value: "colorblind"		},
					{ label: qsTr("Colorblind Alt."),	value: "colorblind3"	},
					{ label: qsTr("Viridis"),			value: "viridis"		},
					{ label: qsTr("ggplot2"),			value: "ggplot2"		},
					{ label: qsTr("Gray"),				value: "gray"			}
					]
			}

			CheckBox
			{
				name:		"plotsPredictionsPost"
				label:		qsTr("Posterior predictive distribution")
				checked:	false

				RadioButtonGroup
				{
					name: "plotsPredictionPostType"

					RadioButton
					{
						value: "conditional"
						label: qsTr("Conditional")
						checked: true

						CheckBox
						{
							label:	qsTr("Point estimate")
							name: "plotsPredictionPostEstimate"
							childrenOnSameRow: true

							DropDown
							{
								name: "plotsPredictionPostEstimateType"
								label: ""
								values: ["mean", "median", "mode"]
							}
						}
						
						CheckBox
						{
							name: "plotsPredictionPostCI"
							label: qsTr("CI")
							id: plotsPredictionPostCI
							childrenOnSameRow: true

							DropDown
							{
								name: "plotsPredictionPostTypeCI"
								label: ""
								values: ["central", "HPD", "custom"]
								id: plotsPredictionPostTypeCI
							}
						}

						Group
						{
							columns: 2

							CIField
							{
								visible: plotsPredictionPostTypeCI.currentText == "central" |
										 plotsPredictionPostTypeCI.currentText == "HPD"
								enabled: plotsPredictionPostCI.checked
								name: "plotsPredictionPostCoverage"
								label: qsTr("coverage")
								fieldWidth: 50
								defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
							}

							IntegerField
							{
								visible: plotsPredictionPostTypeCI.currentText == "custom"
								enabled: plotsPredictionPostCI.checked
								name: "plotsPredictionPostLower"
								label: qsTr("lower")
								id: plotsPredictionPostLower
								fieldWidth: 50
								defaultValue: 0; min: 0; max: plotsPredictionPostUpper.value; inclusive: JASP.MinMax
							}

							IntegerField
							{
								visible: plotsPredictionPostTypeCI.currentText == "custom"
								enabled: plotsPredictionPostCI.checked
								name: "plotsPredictionPostUpper"
								label: qsTr("upper")
								id: plotsPredictionPostUpper
								fieldWidth: 50
								defaultValue: 1
								min: plotsPredictionPostLower.value; inclusive: JASP.MinMax
							}

						}
					}

					RadioButton
					{
						value: "joint"
						label: qsTr("Joint")

						RadioButtonGroup
						{
							name: "plotsPredictionPostJointType"

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
							name: "plotsPredictionPostMarginalEstimate"
							childrenOnSameRow: true

							DropDown
							{
								name: "plotsPredictionPostMarginalEstimateType"
								label: ""
								values: ["mean", "median", "mode"]
							}
						}
				

						CheckBox
						{
							name: "plotsPredictionPostMarginalCI"
							label: qsTr("CI")
							id: plotsPredictionPostMarginalCI
							childrenOnSameRow: true

							DropDown
							{
								name: "plotsPredictionPostMarginalTypeCI"
								label: ""
								values: ["central", "HPD", "custom"]
								id: plotsPredictionPostMarginalTypeCI
							}
						}

						Group
						{
							columns: 2
							CIField
							{
								visible: plotsPredictionPostMarginalTypeCI.currentText == "central" |
										 plotsPredictionPostMarginalTypeCI.currentText == "HPD"
								enabled: plotsPredictionPostMarginalCI.checked
								name: "plotsPredictionPostMarginalCoverage"
								label: qsTr("coverage")
								fieldWidth: 50
								defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
							}

							DoubleField
							{
								visible: plotsPredictionPostMarginalTypeCI.currentText == "custom"
								enabled: plotsPredictionPostMarginalCI.checked
								name: "plotsPredictionPostMarginalLower"
								label: qsTr("lower")
								id: plotsMarginalPredictionPostLower
								fieldWidth: 50
								defaultValue: 0; min: 0; max: plotsPredictionPostMarginalUpper.value; inclusive: JASP.MinMax
							}

							DoubleField
							{
								visible: plotsPredictionPostMarginalTypeCI.currentText == "custom"
								enabled: plotsPredictionPostMarginalCI.checked
								name: "plotsPredictionPostMarginalUpper"
								label: qsTr("upper")
								id: plotsPredictionPostMarginalUpper
								fieldWidth: 50
								defaultValue: 1; min: plotsPredictionPostMarginalLower.value; inclusive: JASP.MinOnly
							}
						}
					}

					CheckBox
					{
						name:	"predictionPostPlotProp"
						id:		predictionPlotProp
						label:	qsTr("Show sample proportions")
					}

					CheckBox
					{
						name:	"predictionPostPlotTable"
						label:	qsTr("Predictions table")
					}
				}

			}
		}
	}
}
