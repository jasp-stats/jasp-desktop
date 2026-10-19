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
import JASP.Controls

/*!
    \qmltype TabView
    \inqmlmodule JASP.Controls 1.0
    \brief A tabbed container that manages dynamic panels.

	Displays a tab bar where each tab shows
    its own panel of child controls. Tabs can be added, removed, and
    renamed (double-click). Commonly used when an analysis needs a variable
    number of configuration panels (e.g. one per group).
	It has in fact the same functinality as ComponentsList, buut instead of displaying the components in rows,
	it display them as Tabs.

    \section1 R Binding

    \list
    \li \b{R Type:} list (each tab produces one element in the list)
    \li \b{Default:} [] (one tab created by default)
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b label (string) - Title displayed above the tab bar. Alias: title. Default: "".
	\li \b source (var) - Source control for populating the tabs. Default: undefined. This can be an id or name (or an array of names/ids) of another controls.
	\li \b content (Component) - One QML component (use Row or RowLayout if more items are needed), that will be repeated for each row. In each row, you can use the rowValue, rowLabel, rowType or rowIndex that gives you resp. the value, label, type (if it is a variable) and index linked to each row.
    \li \b showAddIcon (bool) - Show a "+" button to add tabs. Default: true when addItemManually.
    \li \b showRemoveIcon (bool) - Show a "×" icon on each tab. Default: true when addItemManually.
	\li \b addItemManually (bool) - Allow user to add/remove tabs. Default: false when source is set, true otherwise
    \li \b tabNameEditable (bool) - Allow double-click to rename tabs. Default: true when addItemManually.
    \li \b newTabName (string) - Default name for newly added tabs. Default: "New tab".
    \li \b currentIndex (int) - Index of the currently selected tab. Default: 0.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    TabView {
        name: "models"
        title: qsTr("Models")
        newTabName: qsTr("Model 1")
		content:  VariablesList { name: "predictors"; title: qsTr("Predictors") }
    }
    \endqml
*/
ComponentsListBase
{
	id						: tabView
	background				: rectangleItem
	implicitWidth 			: parent ? parent.width : 0
	implicitHeight			: itemStack.y + itemStack.height
	shouldStealHover		: false
	innerControl			: itemTabBar
	addItemManually			: !source
	minimumItems			: 1
	newItemName				: qsTr("New tab")
	controlType				: JASPControl.TabView
	focusOnTab				: false
	Layout.columnSpan		: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1
	preferredWidth			: parent ? parent.width : 0
	preferredHeight			: implicitHeight
	newItemLabel			: newItemValue
	optionKeyLabel			: (values != null && values.length > 0 && values[0].hasOwnProperty("label")) ? "keyLabel" : ""

	property alias	label				: tabView.title
	property alias	newItemName			: tabView.newItemValue		// For backward compatibility
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
	property color	backgroundColor		: jaspTheme.uiBackground
	property color	tabButtonColor		: jaspTheme.grayLighter

	property real	tabBarHeight		: 28 * preferencesModel.uiScale
	property real	tabButtonRadius		: 5 * preferencesModel.uiScale
	property real	tabButtonWidth		: 100 * preferencesModel.uiScale

	function isTabRemovable(index)
	{
		return true
	}
	function isTabEditable(index)
	{
		return isTabRemovable(index)
	}

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
			id				: tabButton
			width			: Math.min(tabButtonWidth, (rectangleItem.width - itemRepeater.count - (tabView.showAddIcon ? addIconItem.width : 0)) / itemRepeater.count)
			height			: itemTabBar.height
			hoverEnabled	: true		// Without this, tabButton.hovered never becomes true and the ToolTip below never shows
			onClicked		: forceActiveFocus()
			onActiveFocusChanged: if (activeFocus) itemTabBar.currentIndex = model.index

			// Down arrow moves keyboard focus from the tab button down into the content of its tab
			// (the matching tabViewWrapper in itemStack). Tab still cycles between the tab buttons.
			Keys.onDownPressed: (event) =>
			{
				var wrapper = rep.itemAt(model.index)
				if (wrapper)
				{
					wrapper.forceActiveFocus()
					event.accepted = true
				}
			}

			property bool isEditable: tabView.tabNameEditable && tabView.isTabEditable(model.index)
			property bool isRemovable: tabView.showRemoveIcon && tabView.minimumItems < tabView.count && !textFieldItem.visible && isTabRemovable(model.index)

			contentItem: Item
			{
				anchors.fill			: parent
				anchors.bottomMargin	: tabView.tabButtonRadius
				Text
				{
					id					: tabButtonLabel
					anchors.verticalCenter	: parent.verticalCenter
					horizontalAlignment	: Text.AlignHCenter

					leftPadding			: jaspTheme.labelSpacing
					color				: jaspTheme.black
					text				: tabButton.activeFocus ? ("<b>" + model.name + "</b>") : model.name
					font				: jaspTheme.font
					elide				: Text.ElideRight
					width				: parent.width - jaspTheme.labelSpacing - (removeIconItem.visible ? removeIconItem.width  : 0)
					visible				: !textFieldItem.visible

					QtControls.ToolTip.visible	: tabButton.hovered && (tabButtonLabel.truncated || tabButton.isEditable)
					QtControls.ToolTip.text		: (tabButtonLabel.truncated ? model.value : "")
												+ (tabButtonLabel.truncated && tabButton.isEditable ? "\n" : "")
												+ (tabButton.isEditable ? qsTr("Double click to edit") : "")


				}

				Image
				{
					id						: removeIconItem
					source					: jaspTheme.iconPath + tabView.removeIcon
					anchors.right			: parent.right
					anchors.rightMargin		: 4 * preferencesModel.uiScale
					anchors.verticalCenter	: parent.verticalCenter
					visible					: tabButton.isRemovable
					height					: jaspTheme.iconSize * preferencesModel.uiScale
					width					: jaspTheme.iconSize * preferencesModel.uiScale

					QtControls.ToolTip.text			: tabView.removeTooltip
					QtControls.ToolTip.visible		: tabView.removeTooltip !== "" && deleteMouseArea.containsMouse
					QtControls.ToolTip.toolTip.background:		Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

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
					value				: model.value
					fieldWidth			: parent.width
					fieldHeight			: parent.height
					onEditingFinished	: tabView.keyValueChanged(index, displayValue)

					onActiveFocusChanged: if (!activeFocus) visible = false
				}

			}

			background: Rectangle
			{
				color			: tabButton.checked ? backgroundColor : tabButtonColor
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
					width					: parent.width + 1
					color					: backgroundColor
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
				if (tabButton.isEditable)
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

		color			: backgroundColor
		radius			: tabView.tabButtonRadius

		Rectangle
		{
			anchors
			{
				fill		: parent
				topMargin	: tabView.tabBarHeight
			}
			color			: "transparent"
			radius			: tabView.tabButtonRadius
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
			color: tabButtonColor
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
			leftMargin	: 1 // Remove border line
			rightMargin	: 1
		}

		currentIndex		: itemTabBar.currentIndex
		onCurrentIndexChanged: height = Qt.binding( function() { return currentIndex >= 0 ? rep.itemAt(currentIndex).height : 0; });

		Repeater
		{
			id				: rep
			model			: tabView.model
			FocusScope
			{
				id:	tabViewWrapper
				property var rowComponentItem: model.rowComponent

				width	: itemStack.width
				height	: rowComponentItem ? rowComponentItem.height : 0

				Component.onCompleted:
				{
					rowComponentItem.parent = tabViewWrapper
					rowComponentItem.width = Qt.binding(function() {return itemStack.width})
				}
			}
		}
	}
}
