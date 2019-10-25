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

import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3 as L
import JASP.Theme 1.0

JASPControl
{
	id:					checkBox
	controlType:		"CheckBox"
	implicitWidth:		childrenOnSameRow
							? control.implicitWidth + (childControlsArea.children.length > 0 ? Theme.columnGroupSpacing + childControlsArea.implicitWidth : 0)
							: Math.max(control.implicitWidth, control.padding + checkIndicator.width + control.spacing + childControlsArea.implicitWidth)
	implicitHeight:		childrenOnSameRow
							? Math.max(control.implicitHeight, childControlsArea.implicitHeight)
							: control.implicitHeight + (childControlsArea.children.length > 0 ? Theme.rowGroupSpacing + childControlsArea.implicitHeight : 0)
	focusIndicator:		focusIndicator
	childControlsArea:	childControlsArea

	default property alias	content:				childControlsArea.children
			property alias	control:				control
			property alias	childrenArea:			childControlsArea
			property alias	text:					control.text
			property alias	font:					label.font
			property alias	label:					control.text
			property alias	checked:				control.checked
			property bool	childrenOnSameRow:		false
			property alias	columns:				childControlsArea.columns
			property bool	enableChildrenOnChecked: true
			property alias	alignChildrenTopLeft:	childControlsArea.alignChildrenTopLeft

	signal clicked();
	function click() { control.toggle(); }
	function toggle() { control.toggle(); }


	CheckBox
	{
		id:						control
		padding:				Theme.jaspControlPadding
		focus:					true
		onCheckedChanged:		checkBox.clicked()

		Keys.onReturnPressed:	checked = !checked
		Keys.onEnterPressed:	checked = !checked

		indicator: Rectangle
		{
			id:		checkIndicator
			width:	height
			height:	label.height
			y:		control.padding
			x:		control.padding

			color:			control.checked ? (control.enabled ? Theme.buttonBackgroundColor : Theme.disableControlBackgroundColor) : Theme.controlBackgroundColor
			border.color:	control.enabled ? (control.checked ? Theme.buttonBackgroundColor : Theme.borderColor)					: Theme.disableControlBackgroundColor
			border.width:	1
			radius:			Theme.borderRadius

			Text
			{
				visible:					control.checked ? true : false
				color:						Theme.white
				text:						"\u2713"
				font:						Theme.font
				anchors.horizontalCenter:	parent.horizontalCenter
				renderType:					Text.QtRendering //Prettier
			}
		}

		Rectangle
		{
			id: focusIndicator
			anchors.centerIn: checkIndicator
			width: checkIndicator.width + Theme.jaspControlHighlightWidth
			height: checkIndicator.height + Theme.jaspControlHighlightWidth
			color: "transparent"
			border.width: 0
			border.color: "transparent"
			radius: Theme.jaspControlHighlightWidth
		}

		contentItem: Label
		{
			id:					label
			text:				control.text
			font:				Theme.font
			leftPadding:		checkIndicator.width + control.spacing
			verticalAlignment:	Text.AlignVCenter
		}

		background: Rectangle
		{
			color: "transparent"
		}
	}

	GridLayout
	{
		id:				childControlsArea
		anchors
		{
			top:		childrenOnSameRow ? control.top : control.bottom
			topMargin:	childrenOnSameRow ? 0 : Theme.rowGroupSpacing
			left:		childrenOnSameRow ? control.right : control.left
			leftMargin: childrenOnSameRow ? Theme.columnGroupSpacing : control.padding + checkIndicator.width + control.spacing
		}
		enabled:		enableChildrenOnChecked ? control.checked : true
		visible:		children.length > 0
		columns:		childrenOnSameRow ? children.length : 1
		rowSpacing:		Theme.rowGroupSpacing
		columnSpacing:	Theme.columnGridSpacing
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
	}

}
