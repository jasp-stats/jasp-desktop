//
// Copyright (C) 2013-2024 University of Amsterdam
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
import QtQuick.Layouts
import JASP

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
	controlType				: JASPControl.TabView
	focusOnTab				: false
	Layout.columnSpan		: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1
	preferredWidth			: parent.width
	preferredHeight			: implicitHeight

	property alias	label				: tabView.title
	property bool	showAddIcon			: addItemManually
	property bool	showRemoveIcon		: addItemManually
	property bool	tabNameEditable		: addItemManually
	property string	removeIcon			: "cross.png"
	property string	addIcon				: "round_addition.png"
	property string addTooltip			: qsTr("Add a tab")
	property string removeTooltip		: qsTr("Remove this tab")
	property alias	newTabName			: tabView.newItemName
	property alias	itemTabBar			: itemTabBar
	property alias	itemTitle			: itemTitle
	property alias  content				: tabView.rowComponent
	property alias	currentIndex		: itemTabBar.currentIndex
	property var	buttonComponent		: defaultButtonButton

	property real	tabBarHeight		: 28 * preferencesModel.uiScale
	property real	tabButtonRadius		: 5 * preferencesModel.uiScale
	property real	tabButtonWidth		: 100 * preferencesModel.uiScale

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
			// In order to make rounded button, the tabbar height is set a bit higher, and the bottom line of the buttons with its rounded side is removed.
			id		: tabButton
			width	: Math.min(100 * jaspTheme.uiScale, (rectangleItem.width - itemRepeater.count - (tabView.showAddIcon ? addIconItem.width : 0)) / itemRepeater.count)
			height	: itemTabBar.height

			contentItem: Item
			{
				anchors.fill			: parent
				anchors.bottomMargin	: tabView.tabButtonRadius
				Text
				{
					anchors.verticalCenter	: parent.verticalCenter
					horizontalAlignment	: Text.AlignHCenter

					leftPadding			: jaspTheme.labelSpacing
					color				: jaspTheme.black
					text				: model.name
					font				: jaspTheme.font
					elide				: Text.ElideRight
					width				: parent.width - jaspTheme.labelSpacing - (removeIconItem.visible ? removeIconItem.width  : 0)
					visible				: !textFieldItem.visible
				}

				Image
				{
					id						: removeIconItem
					source					: jaspTheme.iconPath + tabView.removeIcon
					anchors.right			: parent.right
					anchors.rightMargin		: 4 * preferencesModel.uiScale
					anchors.verticalCenter	: parent.verticalCenter
					visible					: tabView.showRemoveIcon && tabView.minimumItems < tabView.count && !textFieldItem.visible
					height					: jaspTheme.iconSize * preferencesModel.uiScale
					width					: jaspTheme.iconSize * preferencesModel.uiScale

					QtControls.ToolTip.text			: tabView.removeTooltip
					QtControls.ToolTip.timeout		: jaspTheme.toolTipTimeout
					QtControls.ToolTip.delay		: jaspTheme.toolTipDelay
					QtControls.ToolTip.visible		: tabView.removeTooltip !== "" && deleteMouseArea.containsMouse

					MouseArea
					{
						id				: deleteMouseArea
						anchors.fill	: parent
						onClicked		: tabView.removeItem(index)

					}
				}

				TextField
				{
					id					: textFieldItem
					isBound				: false
					visible				: false
					useExternalBorder	: false
					value				: model.name
					fieldWidth			: parent.width
					fieldHeight			: parent.height
					onEditingFinished	: tabView.nameChanged(index, displayValue)

					onActiveFocusChanged: if (!activeFocus) visible = false
				}

			}

			background: Rectangle
			{
				color			: tabButton.checked ? jaspTheme.uiBackground : jaspTheme.grayLighter
				radius			: tabView.tabButtonRadius
				border.width	: 1
				border.color	: checked ? jaspTheme.uiBorder : jaspTheme.borderColor

				Rectangle
				{
					// Remove the bottom line with its rounded border of the button.
					anchors.bottom			: parent.bottom
					anchors.bottomMargin	: -1
					anchors.leftMargin		: 1
					height					: tabView.tabButtonRadius
					width					: parent.width
					color					: jaspTheme.uiBackground
				}

				Rectangle
				{
					// Add a line onder the button when it is not checked
					anchors.bottom			: parent.bottom
					anchors.bottomMargin	: tabView.tabButtonRadius - 1
					anchors.left			: parent.left
					anchors.leftMargin		: -1
					width					: parent.width + 2
					height					: 1
					color					: jaspTheme.uiBorder
					visible					: !checked
				}
			}

			onDoubleClicked:
			{
				if (tabView.tabNameEditable)
				{
					textFieldItem.visible = true
					textFieldItem.forceActiveFocus();
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

		Rectangle
		{
			anchors
			{
				fill		: parent
				topMargin	: tabView.tabBarHeight
			}
			color			: "transparent"
			radius			: jaspTheme.borderRadius
			border.color	: jaspTheme.uiBorder
			border.width	: 1
		}
	}

	QtControls.TabBar
	{
		id				: itemTabBar
		contentHeight	: tabView.tabBarHeight + tabView.tabButtonRadius

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
		onClicked		: tabView.addItem()
		toolTip			: tabView.addTooltip
		anchors
		{
			left			: itemTabBar.right
			bottomMargin	: tabView.tabButtonRadius
		}
	}

	// As the TabBar height is a bit higher than its appearance, it removes a bit of the the border at the left side of the TabView control.
	// Just redraw it.
	Rectangle
	{
		anchors.left			: itemTabBar.left
		anchors.bottom			: itemTabBar.bottom
		anchors.bottomMargin	: -1

		width			: 1
		height			: tabView.tabButtonRadius + 1
		color			: jaspTheme.uiBorder
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
		onCurrentIndexChanged: height = Qt.binding( function() { return rep.itemAt(currentIndex).height; });

		Repeater
		{
			id				: rep
			model			: tabView.model
			FocusScope
			{
				id:	tabViewWrapper
				property var rowComponentItem: model.rowComponent

				width	: rowComponentItem ? rowComponentItem.width : 0
				height	: rowComponentItem ? rowComponentItem.height : 0

				Component.onCompleted: rowComponentItem.parent = tabViewWrapper
			}
		}
	}
}
