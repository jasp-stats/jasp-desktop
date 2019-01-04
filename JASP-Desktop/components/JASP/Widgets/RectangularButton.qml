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
import QtQuick.Controls 2.2
import JASP.Theme 1.0

Rectangle
{
	id: filterButtonRoot

	color:			_pressed ? Theme.buttonColorPressed :	_showHovered ? Theme.buttonColorHovered			: Theme.buttonColor
	border.color:											_showHovered ? Theme.buttonBorderColorHovered	: Theme.buttonBorderColor
	border.width:	1


	property string	text:				""
	property string	toolTip:			""
	property string textColor:			"default"
	property bool	enabled:			true
	property bool	selected:			false
	property string	iconSource:			""
	property real	buttonPadding:		(buttonIcon.visible ? 4 : 16) * ppiScale
	property alias	hovered:			buttonMouseArea.containsMouse
	property bool	showIconAndText:	false

	property real	_scaledDim:			32 * ppiScale
	property bool	_showHovered:		(filterButtonRoot.enabled && filterButtonRoot.hovered) || filterButtonRoot.selected
	property alias	_pressed:			buttonMouseArea.pressed

	implicitWidth:	showIconAndText ? buttonText.implicitWidth + buttonPadding + _scaledDim + buttonPadding : buttonIcon.visible ? _scaledDim : buttonText.implicitWidth + buttonPadding
	implicitHeight: _scaledDim
	width:			implicitWidth
	height:			implicitHeight

	ToolTip
	{
		delay:		Theme.toolTipDelay
		timeout:	Theme.toolTipTimeout
		visible:	hovered && toolTip != ""
		text:		toolTip
	}

	signal clicked()

	MouseArea
	{
		id:					buttonMouseArea
		anchors.fill:		parent
		acceptedButtons:	Qt.LeftButton
		hoverEnabled:		true
		cursorShape:		parent.enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
		onClicked:			if(filterButtonRoot.enabled) filterButtonRoot.clicked()
	}

	Image
	{
		id: buttonIcon
		x:	filterButtonRoot.showIconAndText ? filterButtonRoot.buttonPadding : (parent.width / 2) - (width / 2)
		y:	(parent.height / 2) - (height / 2)

		width:	Math.min(filterButtonRoot.width - (2 * buttonPadding), height)
		height: filterButtonRoot.height - (2 * buttonPadding)

		sourceSize.width:	Math.max(48, width  * 2)
		sourceSize.height:	Math.max(48, height * 2)

		visible:	filterButtonRoot.iconSource != "" || filterButtonRoot.showIconAndText
		source:		filterButtonRoot.iconSource
	}

	Text
	{
		id: buttonText
		x:	filterButtonRoot.showIconAndText ? buttonIcon.x + buttonIcon.width + filterButtonRoot.buttonPadding : (parent.width / 2) - (width / 2)
		y:	(parent.height / 2) - (height / 2)

		text:		filterButtonRoot.text
		visible:	filterButtonRoot.iconSource == "" || filterButtonRoot.showIconAndText
		color:		textColor == "default" ? (filterButtonRoot.enabled ? Theme.textEnabled : Theme.textDisabled) : textColor


		font:	Theme.font
		//font.pixelSize: Theme. //Math.max(filterButtonRoot.height * 0.4, Math.min(12 * ppiScale, filterButtonRoot.height - 2))

		height: contentHeight
		width:	Math.min(implicitWidth, parent.width - ( buttonIcon.visible ? buttonIcon.width + buttonIcon.x : 0 ))


		elide:	Text.ElideMiddle
	}
}
