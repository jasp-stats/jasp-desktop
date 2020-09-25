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
	expanded:	false
	title:		qsTr("Sequential Analysis")
	enabled:	dataTypeB.checked || dataTypeC.checked

	property alias bfTypevsNameSequential: 	bfTypevsNameSequential.source

	CheckBox
	{
		name:		"plotsIterative"
		label:		qsTr("Test results")
		checked:	false

		RadioButtonGroup
		{
			name: "plotsIterativeType"
			RadioButton { value: "conditional";		label: qsTr("Conditional"); checked: true}
			RadioButton { value: "joint";			label: qsTr("Joint")}
			RadioButton { value: "marginal";		label: qsTr("Normalized")}
			RadioButton
			{
				value:	"BF"
				label:	qsTr("Bayes factor")

				Group
				{
					columns: 2
					RadioButtonGroup
					{
						name:	"bfTypeSequential"
						RadioButton
						{
							value:	"inclusion"
							label:	qsTr("vs. all")
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
								name:				"bfTypevsNameSequential"
								id:					bfTypevsNameSequential
								indexDefaultValue:	0
							}
						}
					}

					RadioButtonGroup
					{
						name: "bayesFactorTypeSequential"

						RadioButton { label: qsTr("BF\u2081\u2080")			; name: "BF10"; checked: true}
						RadioButton { label: qsTr("BF\u2080\u2081")			; name: "BF01"}
						RadioButton { label: qsTr("log(BF\u2081\u2080)")	; name: "LogBF10"}
					}
				}

			}
		}

		CheckBox
		{
			name:	"plotsIterativeUpdatingTable"
			label:	qsTr("Updating table")
		}

	}

}
