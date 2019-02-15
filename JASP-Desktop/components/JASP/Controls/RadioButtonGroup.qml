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
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3 as L
import JASP.Theme		1.0

JASPControl
{
	id:				control
	controlType:	"RadioButtonGroup"
	hasTabFocus:	false
	childControlsArea:	contentArea

	default property alias	content:		contentArea.children
			property alias	buttons:		buttonGroup.buttons
			property bool	placeRadioButtonsOnSameRow:	false
			property alias	columns:		contentArea.columns
			property bool	alignChildrenTopLeft: true
			property string title:			""
			property alias	text:			control.title
			property int	leftPadding:	Theme.groupContentPadding
			property alias	alignChildrenTopLeft: contentArea.alignChildrenTopLeft

    signal clicked(var item)

	implicitWidth:	placeRadioButtonsOnSameRow
						? contentArea.x + contentArea.implicitWidth
						: Math.max(label.implicitWidth, contentArea.x + contentArea.implicitWidth)
	implicitHeight:	placeRadioButtonsOnSameRow
						? Math.max(label.implicitHeight, contentArea.implicitHeight)
						: contentArea.y + contentArea.implicitHeight	
    
	L.Layout.leftMargin:	indent ? Theme.indentationLength : 0
	
	Label
	{
		id:				label
		text:			control.title
		visible:		control.title && control.visible ? true : false
		anchors.top:	control.top
		anchors.left:	control.left
		font:			Theme.font
		color:			enabled ? Theme.textEnabled : Theme.textDisabled		
    }
    
	ButtonGroup { id: buttonGroup }

	GridLayout
	{
		id:					contentArea
		rowSpacing:			Theme.rowGroupSpacing
		columnSpacing:		Theme.columnGroupSpacing
		columns:			placeRadioButtonsOnSameRow ? children.length : 1
		anchors.top:		control.title && !placeRadioButtonsOnSameRow ? label.bottom : control.top
		anchors.topMargin:	control.title && !placeRadioButtonsOnSameRow ? Theme.titleBottomMargin : 0
		anchors.left:		control.title && placeRadioButtonsOnSameRow ? label.right : control.left
		anchors.leftMargin: control.title ? Theme.groupContentPadding : 0
    }
	
	function linkRadioButtons(item)
	{
		for (var i = 0; i < item.children.length; ++i)
		{
            var child = item.children[i];
			if (child instanceof JASPControl)
			{
				switch(child.controlType)
				{
				case "RadioButton":			child.buttonGroup = buttonGroup;	break;
				case "RadioButtonGroup":										break;
				default:					linkRadioButtons(child);			break;
                }
			} else
                linkRadioButtons(child)
        }        
    }

	Component.onCompleted:
	{
        buttonGroup.clicked.connect(clicked);
        linkRadioButtons(contentArea);		
    }

}
