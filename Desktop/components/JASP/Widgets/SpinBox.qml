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
import QtQuick.Controls 2.5
import JASP.Widgets		1.0
import JASP				1.0

Item
{
					id:						root
	property alias	value:					valueField.text
	property double defaultValue:			0
	property alias	doubleValidator:		doubleValidator
	property bool	negativeValues:			false
	property double	min:					negativeValues ? -Infinity : 0
	property double	max:					Infinity
	property int	decimals:				0
	property alias  inclusive:				doubleValidator.inclusive
	property alias	to:						root.max
	property alias	from:					root.min
	property double	lastValidValue:			defaultValue
	property double	stepSize:				1
	property alias	text:					label.text
	property string toolTip:				""
	property alias	implicitWidthLabel:		label.implicitWidth
	property alias	widthLabel:				label.width

					width:					plus.x + plus.width
					height:					valueField.height

					Keys.onDownPressed:		{ minus.clicked(); event.accepted = true; }
					Keys.onUpPressed:		{ plus.clicked();  event.accepted = true; }
					Keys.onEnterPressed:	(event)=>   valueField.focus = !valueField.focus;
					Keys.onReturnPressed: 	(event)=>	valueField.focus = !valueField.focus;


	signal editingFinished()
	
	Component.onCompleted: valueField.onEditingFinished.connect(editingFinished);
	
	function setValue(val)
	{
		var pow				= Math.pow(10, decimals);
		val					= Math.round(val * pow) / pow;
		val					= Math.min(root.max, Math.max(root.min, val));
		valueField.text		= String(val);
		editingFinished()
	}

	Text
	{
		id:						label
		text:					""
		font:					jaspTheme.font
		color:					enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		width:					!visible ? 0 : implicitWidth
		visible:				text !== ""
		anchors
		{
			left:				parent.left
			verticalCenter:		parent.verticalCenter
		}
	}

	RectangularButton
	{
		id:						minus
		iconSource:				jaspTheme.iconPath + "/subtraction-sign-small.svg" //jaspTheme.iconPath + "/addition-sign-small.svg"
		onClicked:				root.setValue(Number(valueField.text) - root.stepSize)
		width:					height
		anchors
		{
			left:				label.right
			leftMargin:			label.visible ? jaspTheme.labelSpacing : 0
		}

		activeFocusOnTab: false
	}

	TextField
	{
		id:							valueField
		validator:					JASPDoubleValidator { id: doubleValidator; bottom: root.min; top: root.max ; decimals: root.decimals }
		anchors
		{
			left:					minus.right
			verticalCenter:			parent.verticalCenter
		}
		width:						jaspTheme.spinBoxWidth
		height:						plus.height
		font:						jaspTheme.font
		horizontalAlignment:		Text.AlignHCenter
		padding:					jaspTheme.jaspControlPadding
		Keys.onReturnPressed: (event)=>		valueField.processInput()
		Keys.onEnterPressed:		valueField.processInput()
		Keys.onEscapePressed: 		text = root.lastValidValue

		onTextChanged:				if(acceptableInput) root.lastValidValue = text
		selectByMouse:				true
		selectedTextColor:			jaspTheme.white
		selectionColor:				jaspTheme.itemSelectedColor
		color:						enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled

        activeFocusOnTab: false

		function processInput()
		{
			if (!acceptableInput)	text				= root.lastValidValue;
			else					root.lastValidValue = Number(text)
		}

		ToolTip.text:				root.toolTip
		ToolTip.timeout:			jaspTheme.toolTipTimeout
		ToolTip.delay:				jaspTheme.toolTipDelay
		ToolTip.visible:			root.toolTip !== "" && ( hoverMe.containsMouse || minus.hovered || plus.hovered )

		background: Rectangle
		{
			id:				controlBackground
			color:			jaspTheme.controlBackgroundColor
			border.width:	1
			border.color:	jaspTheme.borderColor
		}
	}

	RectangularButton
	{
		id:					plus
		iconSource:			jaspTheme.iconPath + "/addition-sign-small.svg"
		onClicked:			root.setValue(Number(valueField.text) + root.stepSize)
		width:				height

		anchors.left:		valueField.right

		activeFocusOnTab: false
	}

	Rectangle
	{
		anchors
		{
			top:			parent.top
			left:			minus.left
			right:			plus.right
			bottom:			parent.bottom
			margins:		-border.width
		}
		z:					-1
		border.color:		jaspTheme.focusBorderColor
		border.width:		jaspTheme.jaspControlHighlightWidth
		color:				"transparent"
		visible:			root.activeFocus
	}

     MouseArea
     {
        id:					hoverMe
        anchors.fill:		valueField
        hoverEnabled:		true
        acceptedButtons:	Qt.NoButton
        onWheel:			if(wheel.angleDelta > 0) plus.clicked(); else minus.clicked();
     }
}
