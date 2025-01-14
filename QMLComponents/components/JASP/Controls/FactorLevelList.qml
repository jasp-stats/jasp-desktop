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
import QtQml.Models
import JASP.Controls

FactorLevelListBase
{
	id:							repeatedMeasuresFactorsList
	implicitWidth:				parent.width
	implicitHeight:				jaspTheme.defaultVariablesFormHeight
	background:					rectangle
	factorName:					qsTr("Factor")
	levelName:					qsTr("Level")
	factorPlaceHolder:			qsTr("New Factor")
	levelPlaceHolder:			qsTr("New Level")

	readonly	property string deleteIcon: "cross.png"

	Text
	{
		id:				text
		anchors.top:	parent.top
		anchors.left:	parent.left
		text:			repeatedMeasuresFactorsList.title
		height:			repeatedMeasuresFactorsList.title ? 20 * preferencesModel.uiScale : 0
		font:			jaspTheme.font
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Rectangle
	{
		id:				rectangle
		anchors.top:	text.bottom
		anchors.left:	parent.left
		height:			repeatedMeasuresFactorsList.height - text.height
		width:			parent.width
		color:			debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width:	1
		border.color:	jaspTheme.borderColor

		JASPScrollBar
		{
			id:				scrollBar
			flickable:		listView
			manualAnchor:	true
			vertical:		true
			z:				1337

			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
				margins:	2
			}
		}


		GridView
		{
			id:					listView
			cellHeight:			22 * preferencesModel.uiScale
			cellWidth:			width
			clip:				true
			boundsBehavior:		Flickable.StopAtBounds


			anchors.fill:		parent
			anchors.margins:	4 * preferencesModel.uiScale
			model:				repeatedMeasuresFactorsList.model
			delegate:			itemComponent
		}
	}

	Component
	{
		id: itemComponent

		FocusScope
		{
			id:		itemWrapper
			height: listView.cellHeight
			width:	scrollBar.visible ?  listView.cellWidth - scrollBar.width : listView.cellWidth

			Rectangle
			{
				id:				itemRectangle
				anchors.fill:	parent
				focus:			true
				color:			jaspTheme.controlBackgroundColor

				TextField
				{
					id:								colName
					isBound:						false
					value:							model.virtual ? "" : model.name
					placeholderText:				model.virtual ? model.name : ""
					anchors.centerIn:				parent
					fieldWidth:						parent.width
					fieldHeight:					parent.height
					useExternalBorder:				false
					showBorder:						false
					selectValueOnFocus:				true
					control.horizontalAlignment:	model.type === "level" ? TextInput.AlignLeft : TextInput.AlignHCenter
					onEditingFinished:				itemChanged(index, displayValue)
				}

				Image
				{
					source:					jaspTheme.iconPath + deleteIcon
					anchors.right:			parent.right
					anchors.verticalCenter:	parent.verticalCenter
					visible:				model.deletable
					height:					16 * preferencesModel.uiScale
					width:					16 * preferencesModel.uiScale
					z:						2

					MouseArea
					{
						anchors.fill: parent
						onClicked: itemRemoved(index)
					}
				}
			}
		}
	}
}
