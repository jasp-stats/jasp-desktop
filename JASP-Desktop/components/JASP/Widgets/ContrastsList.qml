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
	implicitHeight		: itemContrastVariables.height + (itemCustomContrasts.visible ? itemCustomContrasts.height : 0)
	height				: implicitHeight
	implicitWidth		: parent.width
	width				: implicitWidth
	L.Layout.columnSpan	: parent.columns

	property alias	source					: itemContrastVariables.source
	property string	repeatedMeasureFactors	: "repeatedMeasuresFactors"
	property bool	addCustom				: true
	property alias	itemContrastVariables	: itemContrastVariables
	property alias	itemCustomContrasts		: itemCustomContrasts
	property alias	variablesHeight			: itemContrastVariables.height
	property alias	customContrastsHeight	: itemCustomContrasts.preferredHeight
	property var	contrastValues			:
		[
			{ label: qsTr("none")		, value: "none"			},
			{ label: qsTr("deviation")	, value: "deviation"	},
			{ label: qsTr("simple")		, value: "simple"		},
			{ label: qsTr("difference")	, value: "difference"	},
			{ label: qsTr("Helmert")	, value: "Helmert"		},
			{ label: qsTr("repeated")	, value: "repeated"		},
			{ label: qsTr("polynomial")	, value: "polynomial"	},
			{ label: qsTr("custom")		, value: "custom"		}
		]

	readonly property var constrastCustomValue: [{ label: qsTr("custom"), value: "custom"}]

	onAddCustomChanged:
	{
		if (addCustom)	contrastValues.push( constrastCustomValue[0] )
		else			contrastValues.pop()
	}

	VariablesList
	{
		id				: itemContrastVariables
		title			: qsTr("Factors")
		source			: "fixedFactors"
		name			: "contrasts"
		listViewType	: JASP.AssignedVariables
		height			: 200 * preferencesModel.uiScale
		draggable		: false

		rowComponent: DropDown
		{
			id			: contrastValuesItem
			name		: "contrast"
			enabled		: !addCustom || !rowValueIsInteraction
			currentIndex: enabled ? 0 : (contrastsList.contrastValues.length - 1)
			values		: contrastsList.contrastValues
		}

	}

	ComponentsList
	{
		id					: itemCustomContrasts
		name				: "customContrasts"
		anchors.top			: itemContrastVariables.bottom
		anchors.topMargin	: jaspTheme.rowSpacing
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
				preferredWidth	: itemCustomContrasts.width
				name			: "values"
				modelType		: "AnovaCustomContrasts"
				itemType		: "double"
				minimum			: -Infinity
				decimals		: 3
				buttonsInRow	: false
				columnName		: rowValue
				factorsSource	: contrastsList.repeatedMeasureFactors
			}
		}
	}
}
