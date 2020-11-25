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

import QtQuick			2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts	1.11
import JASP.Controls	1.0		as JASPC

Rectangle
{
	id: plotEditingAxisTable

	property	alias	model					: tableView.model

						width					: implicitWidth
						height					: implicitHeight
						implicitWidth			: parent.width
						implicitHeight			: (2 * jaspTheme.itemPadding + jaspTheme.font.pixelSize) * tableView.rows + 2 * jaspTheme.generalAnchorMargin + (1 + tableView.rows) * tableView.rowSpacing // Should be roughly the same as the height of the tableview below

						border.width			: 1
						border.color			: jaspTheme.uiBorder
						color					: jaspTheme.white


	Item
	{
		anchors.fill:		parent
		anchors.margins:	parent.border.width
		clip:				true

		TableView
		{
			id:					tableView
			delegate:			tableDelegateComp
			columnSpacing:		rowSpacing
			rowSpacing:			2 * jaspTheme.uiScale
			flickableDirection:	Flickable.HorizontalFlick
			onVisibleChanged:	if(visible) forceLayout()
			onModelChanged:		forceLayout()

			anchors
			{
				fill:			parent
				margins:		jaspTheme.generalAnchorMargin
			}
		}
	}

	Component
	{
		id: tableDelegateComp

		Rectangle
		{
			height:			implicitHeight
			width:			implicitWidth
			implicitHeight:	showText.implicitHeight + 2 * jaspTheme.itemPadding
			implicitWidth:	showText.implicitWidth  + 2 * jaspTheme.itemPadding

			color:			jaspTheme.controlBackgroundColor
			border.width:	!showText.visible	? 1	: 0
			border.color:	jaspTheme.borderColor
			radius:			jaspTheme.borderRadius

			onWidthChanged:   TableView.view.forceLayout()
			onHeightChanged:  TableView.view.forceLayout()

			Text
			{
				id:						showText
				font:					jaspTheme.font
				text:					display
				anchors.fill:			parent
				anchors.margins:		jaspTheme.itemPadding
				color:					jaspTheme.textEnabled
				horizontalAlignment:	TextInput.AlignHCenter
				verticalAlignment:		TextInput.AlignVCenter
			}

			TextField
			{
				id:						editText
				font:					jaspTheme.font
				visible:				!showText.visible
				anchors.fill:			parent
				onTextEdited:			display = text
				onActiveFocusChanged:	if(!activeFocus) showText.visible = true
				padding:				0
				bottomInset:			jaspTheme.itemPadding
				topInset:				jaspTheme.itemPadding
				rightInset:				jaspTheme.itemPadding
				leftInset:				jaspTheme.itemPadding

				selectByMouse:			true
				selectedTextColor:		jaspTheme.white
				selectionColor:			jaspTheme.itemSelectedColor
				color:					jaspTheme.textEnabled
				horizontalAlignment:	TextInput.AlignHCenter
				verticalAlignment:		TextInput.AlignVCenter

				background: Item{}
			}

			MouseArea
			{
				anchors.fill:	parent
				enabled:		visible
				visible:		showText.visible
				cursorShape:	Qt.IBeamCursor
				onClicked:
				{
					editText.text		= display;
					showText.visible	= false;

					editText.forceActiveFocus()
				}
			}
		}
	}

	MouseArea
	{
		anchors.fill: tableView
		acceptedButtons: Qt.RightButton
		onClicked:
		{
			if (mouse.button === Qt.RightButton)
				contextMenu.popup()
		}
		onPressAndHold:
		{
			if (mouse.source === Qt.MouseEventNotSynthesized)
				contextMenu.popup()
		}

		Menu
		{
			id: contextMenu
			MenuItem { text: qsTr("Insert break left")		}
			MenuItem { text: qsTr("Insert break right")		}
			MenuItem { text: qsTr("Delete break")			}
		}
	}
}
