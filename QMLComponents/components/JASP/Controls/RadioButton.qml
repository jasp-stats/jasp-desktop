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
import QtQuick.Controls	as QtC
import JASP.Controls


/*!
    \qmltype RadioButton
    \inqmlmodule JASP.Controls 1.0
    \brief A radio button option within a RadioButtonGroup.

    Backed by RadioButtonBase. When checked, its name is sent as the value
    of the parent RadioButtonGroup. Supports nested child controls that
    can be enabled only when this radio button is selected.

    \section1 R Binding

    \list
    \li \b{R Type:} Used as a value option within a RadioButtonGroup (string).
    \li \b{Default:} unchecked
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - Value sent to R when this option is selected. Alias: value. Default: "".
    \li \b label (string) - Text displayed next to the radio indicator. Alias: text. Default: "".
    \li \b checked (bool) - Whether this radio button is currently selected. Default: false.
    \li \b childrenOnSameRow (bool) - Place child controls on the same row. Default: false.
    \li \b enableChildrenOnChecked (bool) - Only enable child controls when checked. Default: true.
    \li \b indentChildren (bool) - Indent child controls below the label. Default: true.
    \li \b columns (int) - Number of columns in the child controls area. Default: 1.
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
    RadioButtonGroup {
        name: "hypothesis"
        RadioButton { value: "twoSided"; label: qsTr("≠ Test value"); checked: true }
        RadioButton { value: "greater";  label: qsTr("> Test value") }
        RadioButton { value: "less";     label: qsTr("< Test value") }
    }
	\endqml

	\qml
	RadioButtonGroup {
		title: qsTr("Operation")
		name: "operation"
		RadioButton {
			value: "plus"; label: qsTr("Plus"); checked: true
			DoubleField { label: "Extra Quantity"; name: "plusExtraQuantity"} // This DoubleField will be eanbled only if Plus option is checked
		}
		RadioButton { value: "Multiply";  label: qsTr("Multiply") }
		RadioButton { value: "Divide";    label: qsTr("Divide") }
	}
    \endqml
*/
RadioButtonBase
{
	id:						radioButton
	implicitWidth:			childrenOnSameRow
								? control.implicitWidth + (childControlsArea.children.length > 0 ? jaspTheme.columnGroupSpacing + childControlsArea.implicitWidth : 0)
								: Math.max(control.implicitWidth, childControlsArea.childControlsPadding + childControlsArea.implicitWidth)
	implicitHeight:			childrenOnSameRow
								?	Math.max(control.implicitHeight, childControlsArea.implicitHeight)
								:	control.implicitHeight + (childControlsArea.children.length > 0 ? jaspTheme.rowGroupSpacing + childControlsArea.implicitHeight : 0)
	focusIndicator:			focusIndicator
	childControlsArea:		childControlsArea
	innerControl:			control
	title:					label.text
	
	Accessible.role:			Accessible.RadioButton
	Accessible.name:			text
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("A radiobutton %1").arg(title) : info
	Accessible.onToggleAction:	control.toggle()
	Accessible.onPressAction:	click()

	property alias	control:				control
	default property alias	content:		childControlsArea.children

	property alias	childrenArea:			childControlsArea
	property alias	text:					control.text
	property alias	label:					control.text
	property alias	checked:				control.checked
	property alias	value:					radioButton.name
	property int	textFormat:				Text.AutoText
	property bool	childrenOnSameRow:		false
	property alias	columns:				childControlsArea.columns
	property bool	enableChildrenOnChecked: true
	property bool	indentChildren:			true

	function click() { clicked(); }
	onClicked: { radioButton.clickHandler(); }

	QtC.RadioButton
	{
		id:					control
		padding:			jaspTheme.jaspControlPadding
		focus:				true

		onCheckedChanged:	if (checked) radioButton.clicked()

		indicator: Rectangle
		{
			id:				radioIndicator
			width:			height
			height:			Math.floor(Math.round(label.height) / 2) * 2
			x:				control.padding
			y:				control.padding

			radius:			width
			color:			control.checked ? (control.enabled ? jaspTheme.buttonBackgroundColor : jaspTheme.controlDisabledBackgroundColor) : jaspTheme.controlBackgroundColor
			border.color:	control.enabled ? (control.checked ? jaspTheme.buttonBackgroundColor : jaspTheme.borderColor)					: jaspTheme.controlDisabledBackgroundColor
			border.width:	1

			Rectangle
			{
				anchors.centerIn:	parent
				width:				Math.round(parent.width / 4) * 2
				height:				width
				radius:				width
				visible:			control.checked
				color:				jaspTheme.controlBackgroundColor
			}
		}

		Rectangle
		{
			id:					focusIndicator
			anchors.centerIn:	radioIndicator
			width:				Math.floor(Math.round(radioIndicator.width  + jaspTheme.jaspControlHighlightWidth) / 2) * 2
			height:				Math.floor(Math.round(radioIndicator.height + jaspTheme.jaspControlHighlightWidth) / 2) * 2
			radius:				width
			color:				"transparent"
			border.width:		0

		}

		contentItem: Label
		{
			id:				label
			text:			control.text
			leftPadding:	radioIndicator.width + control.spacing
			font:			jaspTheme.font
			color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			textFormat:		radioButton.textFormat
		}

		background: Rectangle { color: "transparent" }
	}

	GridLayout
	{
		id:				childControlsArea
		anchors
		{
			top:		childrenOnSameRow ? control.top : control.bottom
			topMargin:	childrenOnSameRow ? 0 : jaspTheme.rowGroupSpacing
			left:		childrenOnSameRow ? control.right : control.left
			leftMargin: childrenOnSameRow ? jaspTheme.columnGroupSpacing : (indentChildren ? childControlsArea.childControlsPadding : 0)
		}

		enabled:		enableChildrenOnChecked ? control.checked : true
		visible:		children.length > 0
		columns:		childrenOnSameRow ? children.length : 1
		rowSpacing:		jaspTheme.rowGroupSpacing
		columnSpacing:	jaspTheme.columnGridSpacing

		property int childControlsPadding: childrenOnSameRow ? control.implicitWidth + jaspTheme.columnGroupSpacing : control.padding + radioIndicator.width + control.spacing
	}

	Component.onCompleted:
	{
		if (childControlsArea.children.length > 0)
		{
			if (childrenOnSameRow)
			{
				if (childControlsArea.implicitHeight < control.implicitHeight)
					childControlsArea.anchors.topMargin = control.padding - 1 // border width
			}
		}
		registerWithParent();
	}

	Component.onDestruction:
	{
		unregisterRadioButton();
	}
}

