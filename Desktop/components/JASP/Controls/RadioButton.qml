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
import QtQuick.Controls	2.4
import JASP				1.0


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

	property alias	control:				control
	default property alias	content:		childControlsArea.children

	property alias	childrenArea:			childControlsArea
	property alias	text:					control.text
	property alias	label:					control.text
	property alias	checked:				control.checked
	property alias	value:					radioButton.name
	property var	buttonGroup:			null
	property bool	childrenOnSameRow:		false
	property alias	columns:				childControlsArea.columns
	property bool	enableChildrenOnChecked: true
	property bool	indentChildren:			true

	function click()
	{
		if (buttonGroup) buttonGroup.clicked(control)
	}


	RadioButton
	{
		id:					control
		ButtonGroup.group:	buttonGroup
		padding:			jaspTheme.jaspControlPadding
		focus:				true

		onCheckedChanged:	if (buttonGroup && checked) buttonGroup.clicked(control)

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
	}
}

