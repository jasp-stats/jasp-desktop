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
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl
{
	id:					textField
	controlType:		"TextField"
	
	implicitHeight:		row.implicitHeight
	implicitWidth:		row.implicitWidth
	background:			useExternalBorder ? externalControlBackground : control.background
	cursorShape:		Qt.IBeamCursor
	
	property alias	control:			control	
	property alias	label:				beforeLabel.text
	property alias	text:				beforeLabel.text
	property alias	value:				control.text
	property alias	defaultValue:		control.text
	property int	fieldWidth:			Theme.textFieldWidth
	property int	fieldHeight:		0
	property bool	useExternalBorder:	true
	property alias	placeholderText:	control.placeholderText
	
	property alias	validator:			control.validator
	property alias	controlLabel:		beforeLabel
	property alias	afterLabel:			afterLabel.text
	property string	inputType:			"string"
	property int	labelSpacing:		4
	
	signal editingFinished()
	signal textEdited()
	signal pressed()
	signal released()
	
	function keyReturnPressed()
	{
		if (activeFocus)
			trillFocus()
		editingFinished();
	}
	
	Component.onCompleted:
	{
		if (!beforeLabel.text && textField.text)
			beforeLabel.text = textField.text;
		
		control.editingFinished.connect(keyReturnPressed);
		control.textEdited.connect(textEdited);
		control.pressed.connect(pressed);
		control.released.connect(released);        
	}
	
	RowLayout
	{
		id:			row
		spacing:	labelSpacing
		
		Rectangle
		{
			implicitWidth: beforeLabel.implicitWidth
			implicitHeight: control.implicitHeight
			color: debug ? Theme.debugBackgroundColor : "transparent"
			Label
			{
				id:			beforeLabel
				visible:	beforeLabel.text && textField.visible ? true : false
				font:		Theme.font
				anchors.verticalCenter: parent.verticalCenter				
				color:		enabled ? Theme.textEnabled : Theme.textDisabled
			}
		}
		
		TextField
		{
			id:						control
			text:					textField.value
			implicitWidth:			textField.fieldWidth
			font:					Theme.font
			focus:					true
			color:					enabled ? Theme.textEnabled : Theme.textDisabled
			Layout.leftMargin:		beforeLabel.visible ? 0 : -labelSpacing
			
			padding:				Theme.jaspControlPadding
			leftPadding:			4 * preferencesModel.uiScale
			selectByMouse:			true
			background: Rectangle
			{
				id:				controlBackground
				color:			Theme.controlBackgroundColor
				border.width:	textField.useExternalBorder && !control.activeFocus ? 1					: 0
				border.color:	textField.useExternalBorder							? Theme.borderColor : "transparent"
			}
			
			Rectangle
			{
				id:					externalControlBackground
				height:				parent.implicitHeight + Theme.jaspControlHighlightWidth
				width:				parent.implicitWidth + Theme.jaspControlHighlightWidth
				color:				"transparent"
				border.width: 1
				border.color: "transparent"
				anchors.centerIn: parent
				opacity: debug ? .3 : 1
				visible: textField.useExternalBorder
				radius: Theme.jaspControlHighlightWidth
			}
		}
		
		Binding
		{
			// This is a way to set the property implicitHeight only if fieldHeight is set
			// If not, implicitHeight should keep its implicit binding.
			target: control
			property: "implicitHeight"
			value: textField.fieldHeight
			when: textField.fieldHeight != 0
		}
		
		Rectangle
		{
			implicitWidth: afterLabel.implicitWidth
			implicitHeight: control.implicitHeight
			color: debug ? Theme.debugBackgroundColor : "transparent"
			Label
			{
				id:			afterLabel
				visible:	afterLabel.text && textField.visible ? true : false
				font:		Theme.font
				anchors.verticalCenter: parent.verticalCenter				
				color:		enabled ? Theme.textEnabled : Theme.textDisabled
			}
		}		
	}
}
