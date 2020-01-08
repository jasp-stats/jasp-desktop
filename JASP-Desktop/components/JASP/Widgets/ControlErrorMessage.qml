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
	color			: jaspTheme.controlErrorBackgroundColor
	visible			: opacity > 0
	opacity			: 0
	width			: messageText.width + 2 * paddingWidth
	height			: messageText.height + 2 * paddingHeight
	z				: 10
	radius			: 4
	border.color	: warning ? jaspTheme.rose : jaspTheme.controlErrorTextColor
	border.width	: 1

	property var control		: parent
	property bool warning		: false
	property var form
	property var container
	property int containerWidth	: container ? (container === form ? form.availableWidth : container.width) : 0
	property int paddingWidth	: 10 * jaspTheme.uiScale
	property int paddingHeight	: 6 * jaspTheme.uiScale

	onContainerWidthChanged: if (visible) showMessage()

	onControlChanged:
	{
		if (!control)
			controlErrorMessage.opacity = 0
	}

	onVisibleChanged:
	{
		if (!visible)
		{
			if (control)
			{
				control.hasError = false;
				control.hasWarning = false;
				parent = null;
			}
		}
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
		enabled: !preferencesModel.safeGraphics;

		NumberAnimation
		{
			duration			: 300
			easing.type			: Easing.InOutQuad
		}
	}

	function showMessage(message, temporary)
	{
		messageTimer.stop();
		if (!message || !control) return;

		messageText.text = message
		messageText.wrapMode = Text.NoWrap
		messageText.width = messageText.implicitWidth

		var x = (control.width / 2) - (controlErrorMessage.width / 2)
		var y = -controlErrorMessage.height - 5

		if (container)
		{
			var maxWidth = containerWidth
			var controlPoint = control.mapToItem(container, x, 0)

			if (controlPoint.x < 0) x = x - controlPoint.x
			controlPoint = control.mapToItem(container, x, 0)

			if (controlPoint.x + controlErrorMessage.width > maxWidth)
			{
				if (controlErrorMessage.width < maxWidth)
					x = x - (controlPoint.x + controlErrorMessage.width - maxWidth)
				else
				{
					x = x - controlPoint.x
					messageText.wrapMode = Text.Wrap
					messageText.width = maxWidth
				}
			}

			if (controlPoint.y + y < 0) y = -controlPoint.y
		}

		controlErrorMessage.x = x
		controlErrorMessage.y = y

		opacity = 1
		if (temporary) messageTimer.start();
	}

	Rectangle
	{
		id				: crossRectangle
		width			: 12
		height			: 12
		anchors.top		: parent.top
		anchors.right	: parent.right
		color			: "transparent"

		property int crossThickness		: 2
		property int crossLengthOffset	: -4

		function closeMessage()
		{
			controlErrorMessage.opacity = 0
			if (controlErrorMessage.control)
				controlErrorMessage.control.forceActiveFocus()
		}


		Rectangle
		{
			anchors.centerIn	: parent
			height				: crossRectangle.crossThickness
			width				: parent.width + crossRectangle.crossLengthOffset
			rotation			: 45
			color				: controlErrorMessage.warning ? jaspTheme.rose : jaspTheme.controlErrorTextColor
		}

		Rectangle
		{
			anchors.centerIn	: parent
			height				: crossRectangle.crossThickness
			width				: parent.width + crossRectangle.crossLengthOffset
			rotation			: -45
			color				: controlErrorMessage.warning ? jaspTheme.rose : jaspTheme.controlErrorTextColor
		}

		states:
		[
			State
			{
				when: crossArea.containsMouse
				PropertyChanges
				{
					target				: crossRectangle
					crossThickness		: 3
					crossLengthOffset	: -2
				}
			}
		]

		MouseArea
		{
			id				: crossArea
			anchors.fill	: parent
			onClicked		: crossRectangle.closeMessage()
			hoverEnabled	: true
			cursorShape		: Qt.PointingHandCursor
		}
	}

	Text
	{
		id						: messageText
		font					: jaspTheme.font
		color					: jaspTheme.controlErrorTextColor
		anchors.verticalCenter	: parent.verticalCenter
		anchors.left			: parent.left
		anchors.leftMargin		: 5
		textFormat				: Text.RichText
	}
}
