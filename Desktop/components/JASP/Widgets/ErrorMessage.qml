//
// Copyright (C) 2013-2022 University of Amsterdam
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
import QtQuick
import JASP
import JASP.Widgets
import JASP.Controls

//copied from ControlErrorMessage, but without all the JS
Rectangle
{
	id				: errorMsg
	color			: warning ? jaspTheme.controlWarningBackgroundColor : jaspTheme.controlErrorBackgroundColor
	visible			: opacity > 0 && text != ""
	opacity			: 1
	width			: implicitWidth
	height			: implicitHeight
	implicitWidth	: 200 * jaspTheme.uiScale
	implicitHeight	: messageText.contentHeight + (2 * paddingHeight) + (!dontShowAgain ? 0 : dontShowAgainButton.height + (paddingHeight * 2))
	z				: 10
	radius			: jaspTheme.borderRadius
	border.color	: foreCol
	border.width	: 1

	property color foreCol: warning ? jaspTheme.controlWarningTextColor : jaspTheme.controlErrorTextColor

	property bool dontShowAgain : false
	property bool warning		: false
	property int paddingWidth	: 10 * jaspTheme.uiScale
	property int paddingHeight	:  6 * jaspTheme.uiScale
	property alias wrapMode:	messageText.wrapMode
	property alias text:		messageText.text
	property alias textFormat:	messageText.textFormat

	signal dontShowAgainClicked()

	Behavior on opacity
	{
		enabled: preferencesModel.animationsOn;

		NumberAnimation
		{
			duration			: 300
			easing.type			: Easing.InOutQuad
		}
	}

	CrossButton { onCrossClicked: {	errorMsg.opacity = 0 } }

	Text
	{
		id						: messageText
		font					: jaspTheme.font
		color					: errorMsg.foreCol
		textFormat				: Text.StyledText
		wrapMode				: Text.Wrap
		anchors
		{
			top				: errorMsg.top
			topMargin		: paddingHeight
			margins			: paddingWidth
			left			: parent.left
			right			: parent.right
		}
	}

	RoundedButton
	{
		id:				dontShowAgainButton
		visible:		dontShowAgain
		textColor:		foreCol
		color:			errorMsg.color
		border.color:	errorMsg.border.color
		text:			qsTr("Don't show again")
		onClicked:
		{
			errorMsg.opacity = 0
			errorMsg.dontShowAgainClicked()
		}

		anchors
		{
			right:		parent.right
			bottom:		parent.bottom
			margins:	paddingHeight
		}
	}
}
