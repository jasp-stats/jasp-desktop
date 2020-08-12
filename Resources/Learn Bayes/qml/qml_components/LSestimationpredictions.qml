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
				label: qsTr("Posterior predictive distribution")
				name: "plotsPredictions"

				RadioButtonGroup
				{
					name: "predictionPlotType"
					RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
					RadioButton { value: "stacked"; 	label: qsTr("Stacked")}
					RadioButton
					{
						value: "individual"
						label: qsTr("Individual")

						CheckBox
						{
							name: "plotsPredictionCI"
							label: qsTr("CI")
							id: plotsPredictionCI
							childrenOnSameRow: true

							DropDown
							{
								visible: plotsPredictionCI.checked
								name: "plotsPredictionType"
								label: ""
								values: ["central", "HPD", "custom"]
								id: plotsPredictionType
							}
						}	

						Group
						{
							columns: 2

							CIField{
								visible: plotsPredictionType.currentText == "central" |
											plotsPredictionType.currentText == "HPD"
								enabled: plotsPredictionCI.checked
								name: "plotsPredictionCoverage"
								label: qsTr("probability")
								fieldWidth: 40
								defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
							}

							IntegerField{
								visible: plotsPredictionType.currentText == "custom"
								enabled: plotsPredictionCI.checked
								name: "plotsPredictionLower"
								label: qsTr("lower")
								id: plotsPredictionLower
								fieldWidth: 50
								defaultValue: 0; min: 0; max: plotsPredictionUpper.value; inclusive: JASP.MinMax
							}

							IntegerField{
								visible: plotsPredictionType.currentText == "custom"
								enabled: plotsPredictionCI.checked
								name: "plotsPredictionUpper"
								label: qsTr("upper")
								id: plotsPredictionUpper
								fieldWidth: 50
								defaultValue: 1
								min: plotsPredictionLower.value; max: predictionN.value; inclusive: JASP.MinMax
							}

						}
					}

					CheckBox
					{
						name: "predictionPlotProp"
						id:	predictionPlotProp
						label: qsTr("Show sample proportions")
					}
				}
			}
		}
	}
}
