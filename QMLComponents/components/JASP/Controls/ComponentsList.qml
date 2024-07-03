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
import QtQuick.Controls as QtControls
import JASP
import QtQuick.Layouts

ComponentsListBase
{
	id						: componentsList
	background				: itemRectangle
	implicitWidth 			: itemFlickable.contentWidth + 2 * jaspTheme.contentMargin
	implicitHeight			: (itemTitle.visible ? itemTitle.height : 0) + itemFlickable.contentHeight + 2 * jaspTheme.contentMargin
	shouldStealHover		: false
	innerControl			: itemGrid
	addItemManually			: !source && !rSource
	Layout.columnSpan		: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1
	preferredWidth			: parent.width
	preferredHeight			: implicitHeight

	property alias	label				: componentsList.title
	property alias	columns				: itemGrid.columns
	property alias	rows				: itemGrid.rows
	property alias	itemGrid			: itemGrid
	property alias	itemRectangle		: itemRectangle
	property alias	itemScrollbar		: verticalScrollbar
	property alias	itemTitle			: itemTitle
	property alias	rowSpacing			: itemGrid.rowSpacing
	property alias	columnSpacing		: itemGrid.columnSpacing
	property alias	addIconItem			: addIconItem
	property bool	showAddIcon			: addItemManually
	property string	removeIcon			: "cross.png"
	property string	addIcon				: "round_addition.png"
	property string addTooltip			: qsTr("Add a row")
	property string removeTooltip		: qsTr("Remove a row")
	property bool   addBorder           : true

	function rowAt(rowIndex)
	{
		return itemRepeater.itemAt(rowIndex).rowComponentItem
	}

	Text
	{
		id				: itemTitle
		anchors.top		: parent.top
		anchors.left	: parent.left
		text			: title
		visible			: title !== ""
		height			: visible ? jaspTheme.variablesListTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Rectangle
	{
		id				: itemRectangle
		anchors
		{
			top			: itemTitle.bottom
			left		: parent.left
		}
		height			: parent.height - itemTitle.y - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.analysisBackgroundColor
		border.width	: addBorder ? 1 : 0
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: verticalScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: true
			z				: 2

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
			}
		}

		JASPScrollBar
		{
			id				: horizontalScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: false
			z				: 2

			anchors
			{
				left		: parent.left
				right		: parent.right
				bottom		: parent.bottom
			}
		}

		Flickable
		{
			id						: itemFlickable
			anchors
			{
				fill				: parent
				margins				: jaspTheme.contentMargin
			}
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: itemGrid.width + jaspTheme.scrollbarBoxWidth
			contentHeight			: (addIconItem.visible ? (addIconItem.y + addIconItem.height - 2 * jaspTheme.contentMargin) : itemGrid.y + itemGrid.height) + jaspTheme.scrollbarBoxWidth

			Item
			{
				id				: itemHeaderLabels
				height			: visible ? jaspTheme.variablesListTitle : 0
				width			: parent.width
				visible			: controlNameXOffsetMap.length > 0

				Repeater
				{
					model: controlNameXOffsetMap.length
					Text
					{
						x		: (typeof controlNameXOffsetMap[index] !== "undefined") ? controlNameXOffsetMap[index].x : 0
						text	: (typeof controlNameXOffsetMap[index] !== "undefined") ? controlNameXOffsetMap[index].label : ""
						font	: jaspTheme.font
						color	: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
					}
				}
			}

			Grid
			{
				id					: itemGrid
				anchors
				{
					top				: itemHeaderLabels.bottom
					left			: parent.left
				}

				focus				: true
				columns				: addItemManually ? 2 : 1
				rows				: itemRepeater.count
				rowSpacing			: 1
				columnSpacing		: jaspTheme.contentMargin
				flow				: Grid.TopToBottom
				verticalItemAlignment: Grid.AlignVCenter

				Repeater
				{
					id				: itemRepeater
					model			: componentsList.model
					delegate		: rowComponent
				}
				Repeater
				{
					model			: addItemManually ? componentsList.model : 0
					delegate		: removeIconComponent
				}
			}

			MenuButton
			{
				id					: addIconItem
				height				: visible ? width : 0
				radius				: width
				visible				: showAddIcon && (maximumItems <= 0 || maximumItems > componentsList.count)
				iconSource			: jaspTheme.iconPath + addIcon
				onClicked			: addItem()
				toolTip				: addTooltip
				opacity				: enabled ? 1 : .5
				anchors
				{
					top				: itemGrid.bottom
					left			: parent.left
					leftMargin		: (itemRectangle.width - width) / 2
				}
			}
		}
	}

	Component
	{
		id: rowComponent

		FocusScope
		{
			id		: itemWrapper
			height	: rowComponentItem ? rowComponentItem.height : 0
			width	: rowComponentItem.width

			property var rowComponentItem	: model.rowComponent

			Component.onCompleted:
			{
				if (rowComponentItem)
				{
					rowComponentItem.parent = itemWrapper;
					rowComponentItem.anchors.left = itemWrapper.left
					rowComponentItem.anchors.verticalCenter = itemWrapper.verticalCenter
				}
			}
		}
	}


	Component
	{
		id: removeIconComponent

		FocusScope
		{
			id		: itemWrapperWithDelete
			height	: removeIconID.height
			width	: removeIconID.width

			property var	rowComponentItem	: model.rowComponent
			property bool	isDeletable			: addItemManually && (!model.type || model.type.includes("deletable"))

			Image
			{
				id						: removeIconID
				source					: jaspTheme.iconPath + removeIcon
				visible					: rowComponentItem.enabled && itemWrapperWithDelete.isDeletable && index >= componentsList.minimumItems
				height					: jaspTheme.iconSize
				width					: jaspTheme.iconSize

				QtControls.ToolTip.text			: removeTooltip
				QtControls.ToolTip.timeout		: jaspTheme.toolTipTimeout
				QtControls.ToolTip.delay		: jaspTheme.toolTipDelay
				QtControls.ToolTip.visible		: removeTooltip !== "" && deleteMouseArea.containsMouse

				MouseArea
				{
					id					: deleteMouseArea
					anchors.fill		: parent
					onClicked			: removeItem(index)
					cursorShape			: Qt.PointingHandCursor
				}
			}
		}

	}

}
