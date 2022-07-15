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


import QtQuick			2.11
import QtQuick.Controls 2.5 as QtControls
import QtQuick.Layouts	1.3
import JASP				1.0

ComponentsListBase
{
	id						: tabView
	background				: rectangleItem
	implicitWidth 			: parent.width
	implicitHeight			: itemStack.y + itemStack.height
	shouldStealHover		: false
	innerControl			: itemTabBar
	addItemManually			: !source
	minimumItems			: 1
	newItemName				: qsTr("New tab")

	property alias	label				: tabView.title
	property bool	showAddIcon			: addItemManually
	property bool	showRemoveIcon		: addItemManually
	property bool	tabNameEditable		: addItemManually
	property string	removeIcon			: "cross.png"
	property string	addIcon				: "duplicate.png"
	property string addTooltip			: qsTr("Add a tab")
	property string removeTooltip		: qsTr("Remove this tab")
	property alias	newTabName			: tabView.newItemName
	property alias	itemTabBar			: itemTabBar
	property alias	itemTitle			: itemTitle
	property alias  content				: tabView.rowComponent
	property alias	currentIndex		: itemTabBar.currentIndex
	property var	buttonComponent		: defaultButtonButton

	Text
	{
		id				: itemTitle
		anchors.top		: parent.top
		anchors.left	: parent.left
		text			: title
		height			: title ? jaspTheme.variablesListTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Component
	{
		id: defaultButtonButton

		QtControls.TabButton
		{
			id		: tabButton
			width	: Math.min(100, (rectangleItem.width - itemRepeater.count - (showAddIcon ? addIconItem.width : 0)) / itemRepeater.count)
			contentItem: Text
			{
				color				: jaspTheme.black
				text				: model.name
				horizontalAlignment	: Text.AlignLeft
				verticalAlignment	: Text.AlignVCenter
				elide				: Text.ElideRight
				width				: parent.width - (removeIconItem.visible ? (removeIconItem.width + 2 * jaspTheme.labelSpacing) : 0)
			}

			background: Rectangle
			{
				color		: itemTabBar.currentIndex === index ? jaspTheme.analysisBackgroundColor : jaspTheme.grayLighter

				Rectangle
				{
					anchors
					{
						right			: parent.right
						bottom			: parent.bottom
						top				: parent.top
						bottomMargin	: 4 * preferencesModel.uiScale
						topMargin		: 4 * preferencesModel.uiScale
					}

					visible: index == tabView.count - 1 || (itemTabBar.currentIndex != index && itemTabBar.currentIndex != index + 1)

					width	: 1
					color	: jaspTheme.gray
				}
			}

			onDoubleClicked:
			{
				if (tabNameEditable)
				{
					textFieldItem.visible = true
					textFieldItem.forceActiveFocus();
				}
			}

			TextField
			{
				id					: textFieldItem
				z					: 3
				isBound				: false
				visible				: false
				useExternalBorder	: false
				value				: model.name
				fieldWidth			: parent.width
				fieldHeight			: parent.height
				onEditingFinished	: tabView.nameChanged(index, value)

				onActiveFocusChanged: if (!activeFocus) visible = false
			}

			Image
			{
				id						: removeIconItem
				source					: jaspTheme.iconPath + tabView.removeIcon
				anchors.right			: parent.right
				anchors.rightMargin		: 4 * preferencesModel.uiScale
				anchors.verticalCenter	: parent.verticalCenter
				visible					: tabView.showRemoveIcon && tabView.minimumItems < tabView.count
				height					: jaspTheme.iconSize * preferencesModel.uiScale
				width					: jaspTheme.iconSize * preferencesModel.uiScale
				z						: 2

				QtControls.ToolTip.text			: removeTooltip
				QtControls.ToolTip.timeout		: jaspTheme.toolTipTimeout
				QtControls.ToolTip.delay		: jaspTheme.toolTipDelay
				QtControls.ToolTip.visible		: removeTooltip !== "" && deleteMouseArea.containsMouse

				MouseArea
				{
					id				: deleteMouseArea
					anchors.fill	: parent
					onClicked		: tabView.removeItem(index)
				}
			}
		}
	}

	Rectangle
	{
		id				: rectangleItem

		anchors.top		: itemTitle.bottom
		anchors.left	: parent.left
		height			: itemTabBar.height + itemStack.height + 2 * preferencesModel.uiScale
		width			: parent.width

		color			: "transparent"
		radius			: jaspTheme.borderRadius
		border.color	: jaspTheme.borderColor
		border.width	: 1
		z				: 2
	}

	QtControls.TabBar
	{
		id				: itemTabBar
		anchors
		{
			top			: itemTitle.bottom
			left		: parent.left
		}

		background: Rectangle
		{
			color: jaspTheme.grayLighter
		}

		Repeater
		{
			id			: itemRepeater
			model		: tabView.model
			delegate	: tabView.buttonComponent
		}
	}

	MenuButton
	{
		id				: addIconItem
		height			: 28 * preferencesModel.uiScale //jaspTheme.defaultRectangularButtonHeight
		width			: height
		radius			: height
		visible			: tabView.showAddIcon && (tabView.maximumItems <= 0 || tabView.maximumItems > tabView.count)
		iconSource		: jaspTheme.iconPath + tabView.addIcon
		onClicked		: addItem()
		toolTip			: tabView.addTooltip
		anchors
		{
			left			: itemTabBar.right
			verticalCenter	: itemTabBar.verticalCenter
		}
	}

	StackLayout
	{
		id				: itemStack
		anchors
		{
			top			: itemTabBar.bottom
			topMargin	: 2 * preferencesModel.uiScale
			left		: parent.left
			right		: parent.right
		}

		currentIndex		: itemTabBar.currentIndex

		Repeater
		{
			model			: tabView.model
			FocusScope
			{
				id:	tabViewWrapper
				property var rowComponentItem: model.rowComponent

				width	: rowComponentItem ? rowComponentItem.width : 0
				height	: rowComponentItem ? rowComponentItem.height : 0

				Component.onCompleted:
				{
					rowComponentItem.parent = tabViewWrapper
					rowComponentItem.z = 10
					itemStack.height = Qt.binding( function() { return rowComponentItem.height; });
				}
			}
		}
	}
}
