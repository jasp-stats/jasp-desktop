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

import QtQuick
import JASP.Controls
import QtQuick.Layouts as L
import JASP

Item
{
	id					: contrastsList
	implicitHeight		: itemContrastVariables.height + (itemCustomContrasts.visible ? itemCustomContrasts.height : 0)
	implicitWidth		: jaspForm.width

	property int preferredHeight:	implicitHeight
	property int preferredWidth:	jaspForm.width

	L.Layout.columnSpan			: parent.columns
	L.Layout.preferredWidth		: preferredWidth
	L.Layout.preferredHeight	: preferredHeight

	property string	factorsSourceName		: "fixedFactors"
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
		source			: factorsSourceName
		name			: "contrasts"
		listViewType	: JASP.AssignedVariables
		height			: 200 * preferencesModel.uiScale
		draggable		: false

		rowComponent: DropDown
		{
			id			: contrastValuesItem
			name		: "contrast"
			values		: contrastsList.contrastValues
		}
	}

	ComponentsList
	{
		id					: itemCustomContrasts
		name				: "customContrasts"
		anchors.top			: itemContrastVariables.bottom
		anchors.topMargin	: jaspTheme.rowSpacing
		width				: jaspForm.width
		visible				: count > 0
		source				: [ { name: "contrasts", condition: "contrast == 'custom'" }]

		rowComponent: Group
		{
			Text
			{
				height			: 30 * preferencesModel.uiScale
				text			: qsTr("Custom for %1").arg(rowValue)
			}

			CustomContrastsTableView
			{
				preferredWidth	: jaspForm.width - 2 * jaspTheme.contentMargin
				name			: "values"
				itemType		: JASP.Double
				minimum			: -Infinity
				decimals		: 3
				columnName		: rowValue
				factorsSource	: contrastsList.repeatedMeasureFactors
			}
		}
	}
}
