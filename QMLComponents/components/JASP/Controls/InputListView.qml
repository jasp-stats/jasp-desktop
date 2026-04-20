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

/*!
    \qmltype InputListView
    \inqmlmodule JASP.Controls 1.0
    \brief An editable scrollable list for entering free-text values.

    Displays a scrollable grid of text fields where users can type custom values.
    New items are added by typing in a virtual placeholder row. Items can be
    deleted via a cross icon. Optionally supports a row component displayed
    alongside each entry.

    \section1 R Binding

    \list
    \li \b{R Type:} array of strings
    \li \b{Default:} [] (empty array, or defaultValues if set)
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Title displayed above the list. Alias: label. Default: "".
    \li \b rowComponentTitle (string) - Title for the row component column. Default: "".
    \li \b defaultValues (array) - Initial values to populate the list. Default: [].
    \li \b minRows (int) - Minimum number of rows. Default: 0.
    \li \b addVirtual (bool) - Show a placeholder row for adding new items. Default: true.
    \li \b placeHolder (string) - Placeholder text for new items. Default: "New Value".
    \li \b cellHeight (real) - Height of each row cell. Default: 20.
    \li \b cellWidth (real) - Width of each row cell. Default: list width.
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
    InputListView {
        name: "customValues"
        title: qsTr("Values")
        defaultValues: ["0.5", "1.0", "1.5"]
    }
    \endqml
*/
InputListBase
{
	id						: inputListView
	background				: itemRectangle
	implicitWidth 			: parent.width
	implicitHeight			: jaspTheme.defaultVariablesFormHeight
	shouldStealHover		: false
	innerControl			: itemGridView

	property alias	label				: inputListView.title
	property alias	itemGridView		: itemGridView
	property alias	cellHeight			: itemGridView.cellHeight
	property alias	cellWidth			: itemGridView.cellWidth
	property alias	itemTitle			: itemTitle
	property string	rowComponentTitle	: ""

	property var	defaultValues		: []
	property int	minRows				: 0
	property bool	addVirtual			: true
	property string placeHolder			: qsTr("New Value")

	readonly	property string deleteIcon			: "cross.png"

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

	Text
	{
		anchors.top		: parent.top
		anchors.right	: parent.right
		text			: rowComponentTitle
		height			: rowComponentTitle ? jaspTheme.variablesListTitle : 0
		font			: jaspTheme.font
		color			: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
	}

	Rectangle
	{
		id				: itemRectangle
		anchors.top		: itemTitle.bottom
		anchors.left	: parent.left
		height			: inputListView.height - itemTitle.height
		width			: parent.width
		color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.controlBackgroundColor
		border.width	: 1
		border.color	: jaspTheme.borderColor
		radius			: jaspTheme.borderRadius

		JASPScrollBar
		{
			id				: scrollBar
			flickable		: itemGridView
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

		GridView
		{
			id						: itemGridView
			cellHeight				: 20  * preferencesModel.uiScale
			cellWidth				: width
			clip					: true
			focus					: true
			anchors.fill			: parent
			anchors.margins			: 4 * preferencesModel.uiScale
			anchors.rightMargin		: scrollBar.width + anchors.margins
			model					: inputListView.model
			delegate				: itemInputComponent
			boundsBehavior			: Flickable.StopAtBounds
		}
	}

	Component
	{
		id: itemInputComponent

		FocusScope
		{
			id:		itemWrapper
			height: inputListView.cellHeight
			width:	scrollBar.visible ?  inputListView.cellWidth - scrollBar.width : inputListView.cellWidth

			property bool	isDeletable:		model.type.includes("deletable")
			property bool	isVirtual:			model.type.includes("virtual")
			property var	rowComponentItem:	model.rowComponent

			Component.onCompleted:
			{
				textField.fieldWidth = Qt.binding( function() { return itemWrapper.width - (rowComponentItem ? rowComponentItem.width : 0) - deleteIconID.width; })
				textField.focus = true;

				if (rowComponentItem)
				{
					rowComponentItem.parent					= itemWrapper
					rowComponentItem.anchors.verticalCenter	= itemWrapper.verticalCenter
					rowComponentItem.anchors.right			= itemWrapper.right
					rowComponentItem.anchors.rightMargin	= deleteIconID.width
					rowComponentItem.enabled				= !itemWrapper.isVirtual
				}
			}

			TextField
			{
				id:								textField
				isBound:						false
				value:							(!itemWrapper.isVirtual && model) ? model.name : ""
				placeholderText:				(itemWrapper.isVirtual && model) ? model.name : ""
				useExternalBorder:				false
				showBorder:						false
				selectValueOnFocus:				true
				control.horizontalAlignment:	TextInput.AlignLeft
				onEditingFinished:				inputListView.itemChanged(index, displayValue)
			}

			Image
			{
				id:						deleteIconID
				source:					jaspTheme.iconPath + deleteIcon
				anchors.right:			parent.right
				anchors.verticalCenter:	parent.verticalCenter
				visible:				itemWrapper.isDeletable
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
