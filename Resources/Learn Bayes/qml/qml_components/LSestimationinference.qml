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
import JASP 1.0

Section
{
	expanded: false
	title: qsTr("Inference")
	columns: 2

	property alias plotsBothSampleProportion: plotsBothSampleProportion.label

	DropDown
	{
		Layout.columnSpan:	2
		name:		"pointEstimate"
		label:		qsTr("Point estimate")
		values:		["mean", "median", "mode"]
	}


	CheckBox
	{
		name: "plotsPrior"; label: qsTr("Prior distribution"); checked: false	;
		RadioButtonGroup
		{
			name: "plotsPriorType"
			RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
			RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
			RadioButton {
				value: "individual"
				label: qsTr("Individual")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPriorIndividualEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorIndividualEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPriorIndividualCI"
					label: qsTr("CI")
					id: plotsPriorIndividualCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPriorIndividualType"
						label: ""
						values: ["central", "HPD", "custom"]
						id: plotsPriorIndividualType
					}
				}

				Group
				{
					columns: 2
					CIField{
						visible: plotsPriorIndividualType.currentText == "central" |
									plotsPriorIndividualType.currentText == "HPD"
						enabled: plotsPriorIndividualCI.checked
						name: "plotsPriorCoverage"
						label: qsTr("mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField{
						visible: plotsPriorIndividualType.currentText == "custom"
						enabled: plotsPriorIndividualCI.checked
						name: "plotsPriorLower"
						label: qsTr("lower")
						id: plotsPriorLower
						fieldWidth: 50
						defaultValue: 0.25; min: 0; max: plotsPriorUpper.value; inclusive: JASP.MinMax
					}

					DoubleField{
						visible: plotsPriorIndividualType.currentText == "custom"
						enabled: plotsPriorIndividualCI.checked
						name: "plotsPriorUpper"
						label: qsTr("upper")
						id: plotsPriorUpper
						fieldWidth: 50
						defaultValue: 0.75; min: plotsPriorLower.value; max: 1; inclusive: JASP.MinMax
					}
				}
			}
		}
	}

	CheckBox
	{
		name: "plotsPosterior"; label: qsTr("Posterior distribution"); checked: false	;
		RadioButtonGroup
		{
			name: "plotsPosteriorType"
			RadioButton { value: "overlying"; 	label: qsTr("All"); checked: true}
			RadioButton { value: "stacked"; 	label: qsTr("Stacked")	}
			RadioButton {
				value: "individual"
				label: qsTr("Individual")

				CheckBox
				{
					label:	qsTr("Point estimate")
					name: "plotsPosteriorIndividualEstimate"
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorIndividualEstimateType"
						label: ""
						values: ["mean", "median", "mode"]
					}
				}

				CheckBox
				{
					name: "plotsPosteriorIndividualCI"
					label: qsTr("CI")
					id: plotsPosteriorIndividualCI
					childrenOnSameRow: true

					DropDown
					{
						name: "plotsPosteriorIndividualType"
						label: ""
						values: ["central", "HPD", "custom", "support"]
						id: plotsPosteriorIndividualType
					}
				}

				Group
				{
					columns: 2
					CIField{
						visible: plotsPosteriorIndividualType.currentText == "central" |
									plotsPosteriorIndividualType.currentText == "HPD"
						enabled: plotsPosteriorIndividualCI.checked
						name: "plotsPosteriorCoverage"
						label: qsTr("mass")
						fieldWidth: 50
						defaultValue: 95; min: 0; max: 100; inclusive: JASP.MaxOnly
					}

					DoubleField{
						visible: plotsPosteriorIndividualType.currentText == "custom"
						enabled: plotsPosteriorIndividualCI.checked
						name: "plotsPosteriorLower"
						label: qsTr("lower")
						id: plotsPosteriorLower
						fieldWidth: 50
						defaultValue: 0.25; min: 0; max: plotsPosteriorUpper.value; inclusive: JASP.MinMax
					}

					DoubleField{
						visible: plotsPosteriorIndividualType.currentText == "custom"
						enabled: plotsPosteriorIndividualCI.checked
						name: "plotsPosteriorUpper"
						label: qsTr("upper")
						id: plotsPosteriorUpper
						fieldWidth: 50
						defaultValue: 0.75; min: plotsPosteriorLower.value; max: 1; inclusive: JASP.MinMax
					}

					DoubleField{
						visible: plotsPosteriorIndividualType.currentText == "support"
						enabled: plotsPosteriorIndividualCI.checked
						name: "plotsPosteriorBF"
						label: qsTr("BF")
						fieldWidth: 50
						defaultValue: 1; min: 0; inclusive: JASP.None
					}
				}

			}

		}
	}

	CheckBox
	{
		Layout.columnSpan: 2
		name: "plotsBoth"
		label: qsTr("Prior and posterior distribution")
		checked: false

		CheckBox
		{
			name:		"plotsBothSampleProportion"
			label:		qsTr("Observed proportion")
			id:			plotsBothSampleProportion
			checked:	false
		}
	}

}
