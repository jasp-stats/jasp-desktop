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

import QtQuick 2.9
import QtQuick.Controls 2.4


Item
{
	id: filterButtonRoot

	property string	text:				""
	property string	toolTip:			""
	property string textColor:			"default"
	property bool	selected:			activeFocus
	property string	iconSource:			""
	property real	buttonPadding:		6 * preferencesModel.uiScale
	property alias	hovered:			buttonMouseArea.containsMouse
	property bool	showIconAndText:	false
	property bool	centerText:			true
	property bool	iconLeft:			true

	property real	_scaledDim:			jaspTheme.defaultRectangularButtonHeight
	property alias	_pressed:			buttonMouseArea.pressed
	property alias  color:				rect.color
	property alias	border:				rect.border
	property alias	radius:				rect.radius
	property alias	font:				buttonText.font
	property alias	icon:				buttonIcon

	implicitWidth:	showIconAndText ?
						buttonText.implicitWidth + buttonPadding + _scaledDim + buttonPadding :
						buttonIcon.visible ? _scaledDim : buttonText.implicitWidth + ( 2 * buttonPadding)
	implicitHeight: _scaledDim
	width:			implicitWidth
	height:			implicitHeight


	ToolTip.text:				toolTip
	ToolTip.timeout:			jaspTheme.toolTipTimeout
	ToolTip.delay:				jaspTheme.toolTipDelay
	ToolTip.visible:			toolTip !== "" && buttonMouseArea.containsMouse

	Keys.onSpacePressed:	clicked();
	Keys.onEnterPressed:	clicked();
	Keys.onReturnPressed: (event)=>	clicked();

	signal clicked()

	Rectangle
	{
		id: rect

		color:			!enabled ? jaspTheme.buttonColorDisabled : (_pressed || selected) ? jaspTheme.buttonColorPressed :	filterButtonRoot.hovered ?					jaspTheme.buttonColorHovered		: jaspTheme.buttonColor
		border.color:	(filterButtonRoot.hovered || selected) ?	jaspTheme.buttonBorderColorHovered	: jaspTheme.buttonBorderColor
		border.width:	1
		width:			parent.width
		height:			parent.height

		MouseArea
		{
			id:							buttonMouseArea
			anchors.fill:				parent
			acceptedButtons:			Qt.LeftButton
			hoverEnabled:				true
			cursorShape:				Qt.PointingHandCursor
			onClicked:					filterButtonRoot.clicked();
			visible:					filterButtonRoot.enabled
			//propagateComposedEvents:	true
		}

		Image
		{
			id: buttonIcon
			x:	!filterButtonRoot.showIconAndText ?
					(parent.width / 2) - (width / 2) :
					filterButtonRoot.iconLeft ?
						filterButtonRoot.buttonPadding :
						parent.width - (width + filterButtonRoot.buttonPadding)

			y:	(parent.height / 2) - (height / 2)

			width:	Math.min(filterButtonRoot.width - (2 * buttonPadding), height)
			height: filterButtonRoot.height - (2 * buttonPadding)

		//	sourceSize.width:	Math.max(96, width  * 2)
		//	sourceSize.height:	Math.max(96, height * 2)

			visible:	filterButtonRoot.iconSource != "" || filterButtonRoot.showIconAndText
			source:		filterButtonRoot.iconSource
			mipmap:		true
			smooth:		true
		}

		Text
		{
			id: buttonText
			x:	filterButtonRoot.centerText ?
					(parent.width / 2) - (width / 2) :
					!buttonIcon.visible || !filterButtonRoot.iconLeft ?
						filterButtonRoot.buttonPadding :
						buttonIcon.x + buttonIcon.width


			y:	(parent.height / 2) - (height / 2)

			text:		filterButtonRoot.text
			visible:	filterButtonRoot.iconSource == "" || filterButtonRoot.showIconAndText
			color:		textColor == "default" ? (filterButtonRoot.enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled) : textColor


			font:	jaspTheme.font
			//font.pixelSize: jaspTheme. //Math.max(filterButtonRoot.height * 0.4, Math.min(12 * preferencesModel.uiScale, filterButtonRoot.height - 2))

			height: contentHeight
			width:	implicitWidth //Math.min(implicitWidth, parent.width - (( buttonIcon.visible ? buttonIcon.width : 0 ) + (filterButtonRoot.buttonPadding * 2)))


			elide:	Text.ElideMiddle
		}
	}
}
