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
import QtQuick.Layouts
import JASP.Controls
import JASP

/*!
    \qmltype ComponentsList
    \inqmlmodule JASP.Controls 1.0
    \brief A dynamic list that repeats a user-defined component for each row.

    Displays a scrollable list of repeated QML components defined via a rowComponent.
    Rows can be added and removed by the user (when addItemManually is true) or
    populated from a source control. Each row's controls are bound to separate R list entries.

    \section1 R Binding

    \list
    \li \b{R Type:} list (array of objects, one per row)
    \li \b{Default:} [] (empty array)
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Title displayed above the list. Alias: label. Default: "".
	\li \b source (var) - Source control for populating rows. Default: undefined. This can be an id or name (or an array of names/ids) of another controls.
    \li \b rSource (string) - R source for populating rows. Default: "".
	\li \b rowComponent (Component) - One QML component (use Row or RowLayout if more items are needed), that will be repeated for each row. In each row, you can use the rowValue, rowLabel, rowType or rowIndex that gives you resp. the value, label, type (if it is a variable) and index linked to each row.
	\li \b addItemManually (bool) - Allow user to add/remove rows via buttons. Default: false when source is set, true otherwise
    \li \b minimumItems (int) - Minimum number of rows that must remain. Default: 0.
    \li \b maximumItems (int) - Maximum number of rows allowed (-1 for unlimited). Default: -1.
	\li \b rows (int) - Number of grid rows. Default: equals row count. Read only.
    \li \b rowSpacing (real) - Vertical spacing between rows. Default: 1.
    \li \b showAddIcon (bool) - Show the add-row icon button. Default: equals addItemManually.
    \li \b addIcon (string) - Icon file for the add button. Default: "round_addition.png".
    \li \b removeIcon (string) - Icon file for the remove button. Default: "cross.png".
    \li \b addTooltip (string) - Tooltip for the add button. Default: "Add a row".
    \li \b removeTooltip (string) - Tooltip for the remove button. Default: "Remove a row".
    \li \b addBorder (bool) - Draw a border around the list. Default: true.
    \li \b headerLabels (array) - Column header labels for the component grid. Default: [].
    \li \b newItemValue (string) - Default value key for new rows. Default: "#".
    \li \b duplicateWhenAdding (bool) - Duplicate the last row when adding. Default: false.
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Signals

    \list
    \li \b addItem() - Emitted when a new row should be added.
    \li \b removeItem(int index) - Emitted when a row should be removed.
    \endlist

    \section1 Example

    \qml
    ComponentsList {
        name: "contrasts"
        title: qsTr("Contrasts")
        source: "fixedFactors"
		headerLabels: [qsTr("Contrast")]
		rowComponent: Row {
			Text { text: rowValue }	// rowValue contains the name of the variable
			DropDown {
				name: "contrast"
				source: [
					{ label: qsTr("None"),       value: "none"       },
					{ label: qsTr("Deviation"),  value: "deviation"  },
					{ label: qsTr("Helmert"),    value: "helmert"    }
				]
			}
        }
	}
	\endqml

	\qml
	ComponentsList { // Here no source is given, so addItemManually is true, and the user will see a '+' button to add more rows
		name: "extraValues"
		title: qsTr("Extra values")
		headerLabels: [qsTr("Alpha"), qtStr("Beta")]
		minimumItems: 2 // 2 rows will be uatomatically initialized. If more rows are added, a delete icon will be added beside each new row, so that the user can delete this row
		rowComponent: Row {
			IntegerField	{ name: "alphaValue" }
			DoubleValue		{ name: "betaValue" }
		}
	}
	\endqml
*/

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
	property double availableWidth		: itemFlickable.width - jaspTheme.scrollbarBoxWidth
	property double availableHeight		: itemFlickable.height - jaspTheme.scrollbarBoxWidth

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
			width	: rowComponentItem ? rowComponentItem.width  : 0

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
				QtControls.ToolTip.visible		: removeTooltip !== "" && deleteMouseArea.containsMouse
				QtControls.ToolTip.toolTip.background:		Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }

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
