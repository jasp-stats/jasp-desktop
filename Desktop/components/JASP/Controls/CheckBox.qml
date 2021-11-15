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
import JASP				1.0


CheckBoxBase
{
	id:					checkBox
	implicitWidth:		childrenOnSameRow
							? control.implicitWidth + (childControlsArea.children.length > 0 ? jaspTheme.columnGroupSpacing + childControlsArea.implicitWidth : 0)
							: Math.max(control.implicitWidth, control.padding + checkIndicator.width + control.spacing + childControlsArea.implicitWidth)
	implicitHeight:		childrenOnSameRow
							? Math.max(control.implicitHeight, childControlsArea.implicitHeight)
							: control.implicitHeight + (childControlsArea.children.length > 0 ? jaspTheme.rowGroupSpacing + childControlsArea.implicitHeight : 0)
	focusIndicator:		focusIndicator
	childControlsArea:	childControlsArea
	innerControl:		control
	title:				text

	default property alias	content:				childControlsArea.children
			property alias	control:				control
			property alias	childrenArea:			childControlsArea
			property alias	text:					control.text
			property alias	font:					label.font
			property alias	fontInfo:				label.fontInfo
			property alias	label:					control.text
			property alias	labelTextFormat:		label.textFormat
			property alias	checked:				control.checked
			property bool	childrenOnSameRow:		false
			property alias	columns:				childControlsArea.columns
			property bool	enableChildrenOnChecked: true

	function click()	{ control.toggle(); }
	function toggle()	{ control.toggle(); }


	CheckBox
	{
		id:						control
		padding:				jaspTheme.jaspControlPadding
		focus:					true
		// When the checked is changed by a binding, run the clicked slot without emiting the clicked signal
		// The clicked signal should be emitted only when the user really clicks on the CheckBox.
		onCheckedChanged:		checkBox.clickedSlot()
		Keys.onReturnPressed: (event)=>	checked = !checked
		Keys.onEnterPressed:	checked = !checked

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
			topMargin:	childrenOnSameRow ? 0 : jaspTheme.rowGroupSpacing
			left:		childrenOnSameRow ? control.right : control.left
			leftMargin: childrenOnSameRow ? jaspTheme.columnGroupSpacing : control.padding + checkIndicator.width + control.spacing
		}
		enabled:		enableChildrenOnChecked ? control.checked : true
		visible:		children.length > 0
		columns:		childrenOnSameRow ? children.length : 1
		rowSpacing:		jaspTheme.rowGroupSpacing
		columnSpacing:	jaspTheme.columnGridSpacing
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
