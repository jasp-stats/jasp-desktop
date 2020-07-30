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
	title: qsTr("Sequential analysis")
	enabled: dataTypeB.checked || dataTypeC.checked

	CheckBox
	{
		name: "plotsIterative"
		label: qsTr("Test results")
		checked: false

		RadioButtonGroup
		{
			name: "plotsIterativeType"
			RadioButton { value: "conditional";		label: qsTr("Conditional"); checked: true}
			RadioButton { value: "joint";			label: qsTr("Joint")}
			RadioButton { value: "marginal";		label: qsTr("Normalized")}
			RadioButton
			{
				value: "BF"
				label: qsTr("Bayes factor")

				DropDown
					{
					name: "BF_comparison"
					label: qsTr("Against")
					indexDefaultValue: 0
					source: "priors"
				}

				CheckBox
				{
					name: "BF_log"
					label: qsTr("log(BF)")
					checked: false
				}

			}
		}

		CheckBox
		{
			name:  "plotsIterativeUpdatingTable"
			label: qsTr("Updating table")
		}

	}

}
