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

import QtQuick			2.0
import JASP.Controls	1.0
import QtQuick.Layouts	1.3 as L
import JASP				1.0

Item
{
	id					: contrastsList
	height				: contrasts.height + (customContrastsView.visible ? customContrastsView.height : 0)
	implicitHeight		: height
	width				: parent.width
	implicitWidth		: width
	L.Layout.columnSpan	: parent.columns

	property alias	source					: contrasts.source
	property string	repeatedMeasureFactors	: "repeatedMeasuresFactors"
	property bool	addCustom				: true

	VariablesList
	{
		id				: contrasts
		title			: qsTr("Factors")
		source			: "fixedFactors"
		name			: "contrasts"
		listViewType	: JASP.AssignedVariables
		height			: 200 * preferencesModel.uiScale
		draggable		: false

		rowComponents:
		[
			Component
			{
				DropDown
				{
					name		: "contrast"
					values		: addCustom
								  ? [
										{ label: qsTr("none"), value: "none" },
										{ label: qsTr("deviation"), value: "deviation" },
										{ label: qsTr("simple"), value: "simple" },
										{ label: qsTr("difference"), value: "difference" },
										{ label: qsTr("Helmert"), value: "Helmert" },
										{ label: qsTr("repeated"), value: "repeated" },
										{ label: qsTr("polynomial"), value: "polynomial" },
										{ label: qsTr("custom"), value: "custom" }
									]
								  :	[
										{ label: qsTr("none"), value: "none" },
										{ label: qsTr("deviation"), value: "deviation" },
										{ label: qsTr("simple"), value: "simple" },
										{ label: qsTr("difference"), value: "difference" },
										{ label: qsTr("Helmert"), value: "Helmert" },
										{ label: qsTr("repeated"), value: "repeated" },
										{ label: qsTr("polynomial"), value: "polynomial" }
									]
				}
			}
		]

	}

	ComponentsList
	{
		id					: customContrastsView
		name				: "customContrasts"
		anchors.top			: contrasts.bottom
		anchors.topMargin	: jaspTheme.rowSpacing
		cellHeight			: 160 * preferencesModel.uiScale
		height				: count * cellHeight + 10
		visible				: count > 0
		source				: [ { name: "contrasts", condition: "contrastValue == 'custom'", conditionVariables: [{ name: "contrastValue", component: "contrast", property: "currentValue"}] }]

		rowComponent: Group
		{
			Text
			{
				height			: 30 * preferencesModel.uiScale
				text			: qsTr("Custom contrast for %1").arg(rowValue)
			}

			CustomContrastsTableView
			{
				id						: tableCustomContrasts
				columnName				: rowValue
				factorsSource			: contrastsList.repeatedMeasureFactors
				name					: "values"
				implicitHeight			: 130 * preferencesModel.uiScale
				implicitWidth			: customContrastsView.cellWidth
				width					: implicitWidth
				height					: implicitHeight
			}
		}
	}
}
