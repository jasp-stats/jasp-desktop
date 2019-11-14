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


import QtQuick 2.11
import JASP.Widgets 1.0

JASPControl
{
	id:						jaspListControl
	controlType:			"VariablesListView"
	background:				listRectangle
	width:					parent.width
	implicitWidth:			width
	height:					jaspTheme.defaultVariablesFormHeight
	implicitHeight:			height
	useControlMouseArea:	false

	property var	model
	property string title
	property alias	label:				jaspListControl.title
	property alias	count:				listGridView.count
	property int	columns:			1
	property var	source
	property alias	syncModels:			jaspListControl.source

	property alias	listGridView:		listGridView
	property alias	listRectangle:		listRectangle
	property alias	scrollBar:			scrollBar
	property alias	listTitle:			listTitle

	property var	itemComponent
	property var	extraControlColumns:	[]
	property var	extraControlComponents:	[]
	property string extraControlOptionName:	""
	property alias	extraControlTitles:	titles.model


	Text
	{
		id:				listTitle
		anchors.top:	parent.top
		anchors.left:	parent.left
		text:			title
		height:			title ? jaspTheme.listTitle : 0
		font:			jaspTheme.font
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled

	}

	Row
	{
		width:				parent.width
		anchors.top:		jaspListControl.top;
		spacing:			5
		layoutDirection:	Qt.RightToLeft
		Repeater
		{
			id: titles;
			Label { text: modelData }
		}
	}

	Rectangle
	{
		id:				listRectangle
		anchors.top:	listTitle.bottom
		anchors.left:	parent.left
		height:			jaspListControl.height - listTitle.height
		width:			parent.width
		color:			debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width:	1
		border.color:	jaspTheme.borderColor


		JASPScrollBar
		{
			id:				scrollBar
			flickable:		listGridView
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
			id:						listGridView
			cellHeight:				20  * preferencesModel.uiScale
			cellWidth:				width / jaspListControl.columns
			clip:					true
			focus:					true
			anchors.fill:			parent
			anchors.margins:		4  * preferencesModel.uiScale
			anchors.rightMargin:	scrollBar.width + anchors.margins
			model:					jaspListControl.model
			delegate:				jaspListControl.itemComponent
			boundsBehavior:			Flickable.StopAtBounds

		}
	}

	Component.onCompleted:
	{
		var length = jaspListControl.resources.length
		for (var i = length - 1; i >= 0; i--)
		{
			var column = jaspListControl.resources[i];
			if (column instanceof ExtraControlColumn)
			{
				if (!column.name)
					form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has no name defined").arg(jaspListControl.name))
				if (column.type)
				{
					var type = column.type
					if (type === "DropDown") type = "ComboBox"
					var component = Qt.createComponent(type + ".qml")
					if (component.status === Component.Error)
						form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has an unknown type: %2").arg(jaspListControl.name).arg(type))
					else
					{
						jaspListControl.extraControlComponents.push(component)
						jaspListControl.extraControlColumns.push(column);
					}
				}
				else
					form.addError(qsTr("An ExtraControlColumn in VariablesList %1 has no type defined").arg(jaspListControl.name))
			}
		}
	}

}
