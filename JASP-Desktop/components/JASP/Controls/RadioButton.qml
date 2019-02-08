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
import JASP.Theme		1.0

JASPControl
{
	id:							radioButton
	controlType:				"RadioButton"
	isBound:					false
	implicitWidth:			childrenOnSameRow
								? control.implicitWidth + (childControls.children.length > 0 ? Theme.columnGroupSpacing + childControls.implicitWidth : 0)
								: Math.max(control.implicitWidth, childControls.childControlsPadding + childControls.implicitWidth)
	implicitHeight:			childrenOnSameRow
								? Math.max(control.implicitHeight, childControls.implicitHeight)
								: control.implicitHeight + (childControls.children.length > 0 ? Theme.rowGroupSpacing + childControls.implicitHeight : 0)
	useDefaultBackground:		true
	subControls:				childControls.children.length > 0 ? childControls : null

	default property alias	content:				childControls.children
			property alias	childrenArea:			childControls	
			property alias	text:					control.text
			property alias	checked:				control.checked
			property alias	control:				control
			property alias	value:					radioButton.name
			property var	buttonGroup:			null
			property bool	childrenOnSameRow:	false
			property alias	columns:				childControls.columns
			property bool	enableChildrenOnChecked: true
			property bool	indentChildren:			true
			property alias	alignChildrenTopLeft:	childControls.alignChildrenTopLeft
	
	
	RadioButton
	{
		id:					control
		ButtonGroup.group:	buttonGroup
		padding:			Theme.jaspControlPadding

		indicator: Rectangle
		{
			id:				radioIndicator
			width:			height
			height:			label.height
			x:				control.padding
			y:				control.padding

			radius:			width
			color:			control.checked ? (control.enabled ? Theme.buttonBackgroundColor : Theme.disableControlBackgroundColor) : Theme.controlBackgroundColor
			border.color:	control.enabled ? (control.checked ? Theme.buttonBackgroundColor : Theme.borderColor)					: Theme.disableControlBackgroundColor
			border.width:	1

			Rectangle
			{
				anchors.centerIn:	parent
				width:				parent.width / 2
				height:				parent.height / 2
				radius:				width
				visible:			control.checked
				color:				Theme.controlBackgroundColor
            }
        }

		contentItem: Label
		{
			id:				label
			text:			control.text
			leftPadding:	radioIndicator.width + control.padding
			font:			Theme.font
			color:			enabled ? Theme.textEnabled : Theme.textDisabled
        }
    }
	
	GridLayout
	{
		id:				childControls
		enabled:		enableChildrenOnChecked ? control.checked : true
		visible:		children.length > 0
		columns:		childrenOnSameRow ? children.length : 1
		rowSpacing:		Theme.rowGroupSpacing
		columnSpacing:	Theme.columnGridSpacing
		
		property int childControlsPadding: childrenOnSameRow ? control.implicitWidth + Theme.columnGroupSpacing : control.padding + radioIndicator.width + control.spacing
    }

	Component.onCompleted:
	{
		if (childControls.children.length > 0)
		{
			if (childrenOnSameRow)
			{
				childControls.x = childControls.childControlsPadding
				childControls.anchors.top = control.top
				if (childControls.implicitHeight < control.implicitHeight)
					childControls.anchors.topMargin = control.padding - 1 // border width
			}
			else
			{
				childControls.anchors.top = control.bottom
				childControls.anchors.topMargin = Theme.rowGroupSpacing
				childControls.anchors.left = control.left
				childControls.anchors.leftMargin = indentChildren ? childControls.childControlsPadding : 0		
			}				
		}
		
	}
	
}
