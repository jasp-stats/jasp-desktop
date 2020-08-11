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

JASPGridControl
{
	id					: componentsList
	controlType			: JASPControlBase.ComponentsList
	itemComponent		: components
	implicitHeight		: itemTitle.height + itemGrid.height + 2 * jaspTheme.contentMargin + (showAddIcon ? addIconItem.height : 0)


	property bool	addItemManually	: !source && !values
	property bool	showAddIcon		: addItemManually
	property int	minimumItems	: 0
	property int	maximumItems	: -1
	property string newItemName		: "#"
	property string	removeIcon		: "cross.png"
	property string	addIcon			: "duplicate.png"
	property string addTooltip		: qsTr("Add a row")
	property string removeTooltip	: qsTr("Remove a row")
	property var	defaultValues	: []

	signal addItem();
	signal removeItem(int index);
	signal nameChanged(int index, string name)

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
			height	: rowComponentsItem.height
			width	: componentsList.itemGrid.width

			property bool isDeletable: addItemManually && (!model.type || model.type.includes("deletable"))

			RowComponents
			{
				id						: rowComponentsItem
				anchors.verticalCenter	: parent.verticalCenter
				anchors.left			: parent.left
				spacing					: componentsList.columnSpacing
				controls				: model.rowComponents
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
