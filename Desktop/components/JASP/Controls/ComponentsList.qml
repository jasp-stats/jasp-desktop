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
import QtQuick.Controls 2.4 as QtControls
import JASP				1.0
import JASP.Widgets		1.0
import QtQuick.Layouts	1.3

ComponentsListBase
{
	id						: componentsList
	background				: itemRectangle
	implicitWidth 			: parent.width
	implicitHeight			: itemTitle.height + itemGrid.height + 2 * jaspTheme.contentMargin + (showAddIcon ? addIconItem.height : 0)
	shouldStealHover		: false
	innerControl			: itemGrid

	property string title
	property alias	label				: componentsList.title
	property alias	columns				: itemGrid.columns
	property alias	rows				: itemGrid.rows
	property alias	itemGrid			: itemGrid
	property alias	itemRectangle		: itemRectangle
	property alias	itemScrollbar		: itemScrollbar
	property alias	itemTitle			: itemTitle
	property alias	rowSpacing			: itemGrid.rowSpacing
	property alias	columnSpacing		: itemGrid.columnSpacing

	property bool	addItemManually		: !source && !values
	property bool	showAddIcon			: addItemManually
	property int	minimumItems		: 0
	property int	maximumItems		: -1
	property string newItemName			: "#"
	property string	removeIcon			: "cross.png"
	property string	addIcon				: "duplicate.png"
	property string addTooltip			: qsTr("Add a row")
	property string removeTooltip		: qsTr("Remove a row")
	property var	defaultValues		: []

	signal addItem();
	signal removeItem(int index);
	signal nameChanged(int index, string name)

	Text
	{
		id				: itemTitle
		anchors.top		: parent.top
		anchors.left	: parent.left
		text			: title
		height			: title ? jaspTheme.listTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Rectangle
	{
		id				: itemRectangle
		anchors.top		: itemTitle.bottom
		anchors.left	: parent.left
		height			: componentsList.height - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.analysisBackgroundColor
		border.width	: 1
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: itemScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: true
			z				: 1337

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				margins		: 2
			}
		}

		Flickable
		{
			id						: itemFlickable
			anchors.fill			: parent
			anchors.margins			: jaspTheme.contentMargin
			anchors.rightMargin		: itemScrollbar.width + anchors.margins
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: itemGrid.width
			contentHeight			: itemGrid.height

			Grid
			{
				id						: itemGrid
				width					: itemRectangle.width - 2 * jaspTheme.contentMargin - (itemScrollbar.visible ? itemScrollbar.width + 2 : 0)
				focus					: true
				columns					: 1
				rowSpacing				: 1
				columnSpacing			: 1

				Repeater
				{
					id						: itemRepeater
					model					: componentsList.model
					delegate				: components
				}
			}
		}
	}

	MenuButton
	{
		id					: addIconItem
		width				: height
		radius				: height
		visible				: showAddIcon && (maximumItems <= 0 || maximumItems > componentsList.count)
		iconSource			: jaspTheme.iconPath + addIcon
		onClicked			: addItem()
		toolTip				: addTooltip
		anchors
		{
			bottom			: parent.bottom
			horizontalCenter: parent.horizontalCenter
		}
	}

	Component
	{
		id: components

		FocusScope
		{
			id		: itemWrapper
			height	: rowComponentItem ? rowComponentItem.height : 0
			width	: componentsList.itemGrid.width

			property var	rowComponentItem	: model.rowComponent
			property bool	isDeletable			: addItemManually && (!model.type || model.type.includes("deletable"))

			Component.onCompleted:
			{
				if (rowComponentItem)
				{
					rowComponentItem.parent = itemWrapper;
					rowComponentItem.anchors.left = itemWrapper.left
					rowComponentItem.anchors.verticalCenter = itemWrapper.verticalCenter
				}
			}

			Image
			{
				id						: removeIconID
				source					: jaspTheme.iconPath + removeIcon
				anchors.right			: parent.right
				anchors.verticalCenter	: parent.verticalCenter
				visible					: itemWrapper.isDeletable && componentsList.count > componentsList.minimumItems
				height					: jaspTheme.iconSize
				width					: jaspTheme.iconSize
				z						: 2

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
