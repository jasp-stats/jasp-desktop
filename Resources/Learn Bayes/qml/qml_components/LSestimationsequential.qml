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
	title: qsTr("Sequential analysis")


	CheckBox
	{
		name: "plotsIterative"
		label: qsTr("Point estimate")
		checked: false

		RadioButtonGroup
		{
			name: "plotsIterativeType"
			RadioButton
			{
				value: "overlying"
				label: qsTr("All")

			
				RadioButtonGroup
				{
					name: "plotsIterativeCenter"
					RadioButton{value: "mean"; label: qsTr("Mean")}
					RadioButton{value: "median"; label: qsTr("Median")}
				}

				Group
				{

					CheckBox
					{
						name: "plotsIterativeIndividualCI"
						label: qsTr("CI")
						id: plotsIterativeIndividualCI
						childrenOnSameRow: true

						DropDown
						{
							name: "plotsIterativeIndividualType"
							label: ""
							values: ["central", "HPD", "support"]
							id: plotsIterativeIndividualType
						}
					}

					CIField
					{
						visible: plotsIterativeIndividualType.currentText == "central" |
									plotsIterativeIndividualType.currentText == "HPD"
						enabled: plotsIterativeIndividualCI.checked
						name: "plotsIterativeCoverage"
						label: qsTr("probability")
						fieldWidth: 40
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField
					{
						visible: plotsIterativeIndividualType.currentText == "support"
						enabled: plotsIterativeIndividualCI.checked
						name: "plotsIterativeBF"
						label: qsTr("BF")
						fieldWidth: 50
						defaultValue: 1; min: 0; inclusive: JASP.None
					}
				}

			}
			
			RadioButton { value: "stacked"; label: qsTr("Stacked")}

			CheckBox
			{
				name:  "plotsIterativeUpdatingTable"
				label: qsTr("Updating table")
			}
		}

	}

	CheckBox
	{
		name: "plotsIterativeInterval"
		id: plotsIterativeInterval
		label: qsTr("Interval")
		checked: false

		RadioButtonGroup
		{
			name: "plotsIterativeIntervalType"
			id: plotsIterativeIntervalType

			Group
			{
				columns: 2
				DoubleField
				{
					enabled: plotsIterativeInterval.checked
					name: "plotsIterativeIntervalLower"
					label: qsTr("lower")
					id: plotsIterativeIntervalLower
					fieldWidth: 50
					defaultValue: 0.25; min: 0; max: plotsIterativeIntervalUpper.value; inclusive: JASP.MinOnly
				}

				DoubleField
				{
					enabled: plotsIterativeInterval.checked
					name: "plotsIterativeIntervalUpper"
					label: qsTr("upper")
					id: plotsIterativeIntervalUpper
					fieldWidth: 50
					defaultValue: 0.75; min: plotsIterativeIntervalLower.value; max: 1; inclusive: JASP.MaxOnly
				}
			}

			RadioButton	{ value: "overlying"; label: qsTr("All")}
			RadioButton { value: "stacked"; label: qsTr("Stacked")}
			CheckBox	{
				name:  "plotsIterativeIntervalUpdatingTable"
				label: qsTr("Updating table")
			}
		}
	}
	
	CheckBox
	{
		Layout.columnSpan: 2
		name: "doIterative"
		label: qsTr("Posterior updating table")
		checked: false
	}
}
