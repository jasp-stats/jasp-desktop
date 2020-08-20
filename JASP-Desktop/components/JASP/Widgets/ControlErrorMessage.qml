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
import JASP				1.0

Rectangle
{
	id				: controlErrorMessage
	color			: warning ? jaspTheme.controlWarningBackgroundColor : jaspTheme.controlErrorBackgroundColor
	visible			: opacity > 0
	opacity			: 0
	width			: messageText.width  + 2 * paddingWidth
	height			: messageText.height + 2 * paddingHeight
	z				: 10
	radius			: 4
	border.color	: foreCol
	border.width	: 1

	property color foreCol: warning ? jaspTheme.controlWarningTextColor : jaspTheme.controlErrorTextColor

	property var control
	property bool warning		: false
	property var form
	property var container		: parent
	property int containerWidth	: container ? (container === form ? form.availableWidth : container.width) : 0
	property int paddingWidth	: 10 * jaspTheme.uiScale
	property int paddingHeight	: 6 * jaspTheme.uiScale

	onContainerWidthChanged:	if (visible)				showMessage()
	onControlChanged:			if (!control)				controlErrorMessage.opacity = 0
	onVisibleChanged:			if (!visible && control)
	{
		control.hasError	= false;
		control.hasWarning	= false;
		parent				= null;
	}

	Timer
	{
		id				: messageTimer
		running			: false
		repeat			: false
		interval		: 4000
		onTriggered		: controlErrorMessage.opacity = 0
	}

	Behavior on opacity
	{
		enabled: preferencesModel.animationsOn;

		NumberAnimation
		{
			duration			: 300
			easing.type			: Easing.InOutQuad
		}
	}

	function showMessage(message, temporary)
	{
		messageTimer.stop();
		if (!message || !control || !container) return;

		messageText.text = message
		messageText.wrapMode = Text.NoWrap
		messageText.width = messageText.implicitWidth

		var controlPoint = control.mapToItem(container, control.width / 2, 0)

		var x = controlPoint.x - (controlErrorMessage.width / 2)
		var y = controlPoint.y - controlErrorMessage.height - 5

		var maxWidth = containerWidth

		if (x < 0) x = 0

		if (x + controlErrorMessage.width > maxWidth)
		{
			if (controlErrorMessage.width < maxWidth)
				x = maxWidth - controlErrorMessage.width
			else
			{
				x = 0
				messageText.wrapMode = Text.Wrap
				messageText.width = maxWidth
			}
		}

		if (y < 0) y = 0

		if (y + controlErrorMessage.height > controlPoint.y)
			y = controlPoint.y + controlErrorMessage.height + 5

		controlErrorMessage.x = x
		controlErrorMessage.y = y

		opacity = 1
		if (temporary) messageTimer.start();
	}

	CrossButton
	{
		onCrossClicked:
		{
			controlErrorMessage.opacity = 0
			if (controlErrorMessage.control)
				controlErrorMessage.control.forceActiveFocus()
		}
	}

	Text
	{
		id						: messageText
		font					: jaspTheme.font
		color					: controlErrorMessage.foreCol
		anchors.verticalCenter	: parent.verticalCenter
		anchors.left			: parent.left
		anchors.leftMargin		: 5
		textFormat				: Text.RichText
	}
}
