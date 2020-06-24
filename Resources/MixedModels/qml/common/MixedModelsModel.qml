//
// Copyright (C) 2013-2020 University of Amsterdam
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
import QtQuick			2.12
import JASP.Controls	1.0
import JASP.Widgets		1.0
import JASP				1.0
import QtQuick.Layouts	1.3


Section
{
	title:			qsTr("Model")

	VariablesForm
	{
		preferredHeight:	250

		AvailableVariablesList
		{
			name:	"availableModelComponents"
			title:	qsTr("Model components")
			source:	"fixedVariables"
		}

		AssignedVariablesList
		{
			id:				fixedEffects
			name:			"fixedEffects"
			title:			qsTr("Fixed effects")
			listViewType:	JASP.Interaction
		}
	}

	ComponentsList
	{
		id:					randomEffetcs
		title:				qsTr("Random effects")
		name:				"randomEffects"
		source:				"randomVariables"
		visible:			count > 0

		rowComponent: Group
		{
			RowLayout
			{
				Layout.preferredWidth:	randomEffetcs.width
				Label { text: qsTr("Random slopes by %1").arg(rowValue); Layout.preferredWidth: parent.width / 2 }
				CheckBox { label: qsTr("Correlations"); name: "correlations"; checked: true; Layout.preferredWidth: parent.width / 2 }
			}
			ComponentsList
			{
				name:				"randomComponents"
				source:				"fixedEffects"

				rowComponent: CheckBox { name: "randomSlopes"; label: rowValue; checked: true }
			}
		}
	}

}
