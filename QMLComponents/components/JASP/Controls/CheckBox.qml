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
import QtQuick.Controls as QtC
import JASP.Controls

/*!
    \qmltype CheckBox
    \inqmlmodule JASP.Controls 1.0
    \brief A boolean toggle control that binds a true/false value to an R option.

    CheckBox can optionally contain child controls that become enabled when checked.

    \section1 R Binding

    \list
    \li \b{R Type:} \c logical
    \li \b{Default:} \c FALSE
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b checked (bool) - Current checked state. Default: false.
    \li \b label (string) - Text displayed next to the checkbox. Default: "".
    \li \b childrenOnSameRow (bool) - If true, child controls layout horizontally. Default: false.
    \li \b enableChildrenOnChecked (bool) - If true, children enabled only when checked. Default: true.
    \li \b columns (int) - Number of columns for child controls layout. Default: 1.
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
    \li \b clicked() - Emitted when user clicks the checkbox.
    \endlist

    \section1 Example

    \qml
    CheckBox {
        name: "includeCI"
        label: qsTr("Confidence interval")
        checked: true

        CIField {
            name: "ciWidth"
        }
    }
    \endqml
*/
CheckBoxBase
{
	id:					checkBox
	implicitWidth:		childrenOnSameRow
							? control.implicitWidth + (childControlsArea.hasChildren ? jaspTheme.columnGroupSpacing + childControlsArea.implicitWidth : 0)
							: Math.max(control.implicitWidth, control.padding + checkIndicator.width + (childControlsArea.hasChildren ? control.spacing + childControlsArea.implicitWidth : 0))
	implicitHeight:		childrenOnSameRow
							? Math.max(control.implicitHeight, childControlsArea.implicitHeight)
							: control.implicitHeight + (childControlsArea.hasChildren ? jaspTheme.rowGroupSpacing + childControlsArea.implicitHeight : 0)
	focusIndicator:		focusIndicator
	childControlsArea:	childControlsArea
	innerControl:		control
	mouseAreaZone:		control
	title:				text

	default property alias	content:				childControlsArea.content
			property alias	control:				control
			property alias	childrenArea:			childControlsArea
			property alias	text:					control.text
			property alias	font:					label.font
			property alias	fontInfo:				label.fontInfo
			property alias	label:					control.text
			property alias	checked:				control.checked
			property int	textFormat:				Text.AutoText
			property bool	childrenOnSameRow:		false
			property alias	columns:				childControlsArea.columns
			property bool	enableChildrenOnChecked: true
			property bool	forwardKeys:			false

	function click()	{ control.toggle(); }
	function toggle()	{ control.toggle(); }
	
	Accessible.role:			Accessible.CheckBox
	Accessible.name:			control.text
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("A checkbox %1").arg(title) : info
	Accessible.onPressAction:	click()
	Accessible.onToggleAction:	toggle()

	QtC.CheckBox
	{
		id:						control
		padding:				jaspTheme.jaspControlPadding
		focus:					true
		// When the checked is changed by a binding, run the clicked slot without emiting the clicked signal
		// The clicked signal should be emitted only when the user really clicks on the CheckBox.
		onCheckedChanged:		checkBox.clickedSlot()
		Keys.onReturnPressed:	(event)=>	checked = !checked
		Keys.onEnterPressed:	checked = !checked
		Keys.forwardTo:			forwardKeys ? [checkBox] : [] // If a forward is set on the parent we want to hook on that chain, eg modules menu

		// When the user clicks on the CheckBox, the clicked signal of the parent (CheckBoxBase) must be emitted.
		Component.onCompleted: control.clicked.connect(checkBox.clicked)

		indicator: Rectangle
		{
			id:		checkIndicator
			width:	height
			height:	label.height
			y:		control.padding
			x:		control.padding

			color:			control.checked ? (control.enabled ? jaspTheme.buttonBackgroundColor : jaspTheme.controlDisabledBackgroundColor) : jaspTheme.controlBackgroundColor
			border.color:	control.enabled ? (control.checked ? jaspTheme.buttonBackgroundColor : jaspTheme.borderColor)					: jaspTheme.controlDisabledBackgroundColor
			border.width:	1
			radius:			jaspTheme.borderRadius

			Text
			{
				visible:					control.checked ? true : false
				color:						jaspTheme.white
				text:						"\u2713"
				font:						jaspTheme.font
				anchors.horizontalCenter:	parent.horizontalCenter
				renderType:					Text.QtRendering //Prettier
			}
		}

		Rectangle
		{
			id:					focusIndicator
			anchors.centerIn:	checkIndicator
			width:				checkIndicator.width  + jaspTheme.jaspControlHighlightWidth
			height:				checkIndicator.height + jaspTheme.jaspControlHighlightWidth
			color:				"transparent"
			border.width:		0
			border.color:		"transparent"
			radius:				jaspTheme.jaspControlHighlightWidth
		}

		contentItem: Label
		{
			id:					label
			text:				control.text
			color:				enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
			font:				jaspTheme.font
			leftPadding:		checkIndicator.width + control.spacing
			verticalAlignment:	Text.AlignVCenter
			textFormat:			checkBox.textFormat
		}

		background: Rectangle
		{
			color: "transparent"
		}
	}

	Group // Use Group instead of GridLayout so that the label / control fields can be aligned.
	{
		id:				childControlsArea
		anchors
		{
			top:		childrenOnSameRow ? control.top : control.bottom
			topMargin:	childrenOnSameRow ? 0 : jaspTheme.rowGroupSpacing
			left:		childrenOnSameRow ? control.right : control.left
			leftMargin: childrenOnSameRow ? jaspTheme.columnGroupSpacing : control.padding + checkIndicator.width + control.spacing
		}
		enabled:		enableChildrenOnChecked ? control.checked : true
		visible:		hasChildren
		columns:		childrenOnSameRow ? childControlsArea.content.length : 1
		columnSpacing:	jaspTheme.columnGridSpacing
	}

	Component.onCompleted:
	{
		if (childControlsArea.hasChildren)
		{
			if (childrenOnSameRow)
			{
				if (childControlsArea.implicitHeight < control.implicitHeight)
					childControlsArea.anchors.topMargin = control.padding - 1 // border width
			}
		}
	}

}
