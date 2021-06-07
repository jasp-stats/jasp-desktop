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

import QtQuick				2.12
import QtQuick.Controls		2.2
import QtGraphicalEffects	1.12
import JASP.Controls		1.0
import JASP.Widgets			1.0

FocusScope
{
	id: osfLogin

	Rectangle
	{
		color:			jaspTheme.grayMuchLighter
		z:				-1
		anchors.fill:	parent
	}

	Component.onCompleted:
	{
		usernameText.focus = true
	}

	Image
	{
		id: osfLogo

		height: 100 * preferencesModel.uiScale
		width : 100 * preferencesModel.uiScale
		source: jaspTheme.iconPath + "osf-logo.png"
		smooth: true

		sourceSize.width : width  * 2
		sourceSize.height: height * 2

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.bottomMargin    : 20 * preferencesModel.uiScale
	}

	Label
	{
		id: labelOSF

		width : osfLoginBox.width
		height: 40 * preferencesModel.uiScale

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top: osfLogo.bottom
		anchors.bottomMargin: 20 * preferencesModel.uiScale

		text : qsTr("OSF")
		color: jaspTheme.black
		font : jaspTheme.fontLabel
	}

	Text
	{
		id: labelExplain

		text :	qsTr("Sign in with your OSF account to continue")
		color:	jaspTheme.textEnabled
		font.pointSize: 11 * preferencesModel.uiScale
		font.family:	jaspTheme.font.family

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top             : labelOSF.bottom
		anchors.topMargin       : 20 * preferencesModel.uiScale
	}

	Rectangle
	{
		id: osfLoginBox

		height: 240 * preferencesModel.uiScale
		width : 250 * preferencesModel.uiScale
		color : jaspTheme.grayMuchLighter

		border.width: 1
		border.color: jaspTheme.grayDarker

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top: labelExplain.bottom
		anchors.topMargin: 10 * preferencesModel.uiScale

		Rectangle
		{
			id: usernameInput

			anchors.left : parent.left
			anchors.right: parent.right
			anchors.top  : parent.top

			anchors.leftMargin : 20 * preferencesModel.uiScale
			anchors.rightMargin: 20 * preferencesModel.uiScale
			anchors.topMargin  : 30 * preferencesModel.uiScale

			height		: 35 * preferencesModel.uiScale
			width		: 100 * preferencesModel.uiScale
			clip		: true
			color		: jaspTheme.white
			border.width: usernameText.activeFocus ? 3 : 1
			border.color: usernameText.activeFocus ? jaspTheme.focusBorderColor : jaspTheme.grayDarker

			TextInput
			{
				id					: usernameText
				text				: fileMenuModel.osf.username

				anchors.fill		: parent
				anchors.leftMargin	: 10 * preferencesModel.uiScale
				selectByMouse		: true
				selectedTextColor	: jaspTheme.textDisabled
				selectionColor		: jaspTheme.itemSelectedColor
				color				: jaspTheme.textEnabled


				verticalAlignment	: Text.AlignVCenter
				font				: jaspTheme.fontRibbon

				onTextChanged		: fileMenuModel.osf.username = text
				onAccepted			: passwordText.focus = true

				KeyNavigation.down	: passwordText
				KeyNavigation.tab	: passwordText
				focus				: true
			}
		}

		Text
		{
			text			: qsTr("<sup>*</sup>")
			textFormat		: Text.RichText
			font.pointSize	: 12 * preferencesModel.uiScale
			font.family		: jaspTheme.font.family
			color			: jaspTheme.textEnabled

			anchors
			{
				top:				usernameInput.top
				left:				usernameInput.right
				leftMargin:			2 * jaspTheme.uiScale
			}
		}

		Rectangle
		{
			id: passwordInput

			anchors.left  : parent.left
			anchors.right : parent.right
			anchors.top   : usernameInput.bottom

			anchors.leftMargin : 20 * preferencesModel.uiScale
			anchors.rightMargin: 20 * preferencesModel.uiScale
			anchors.topMargin  : 15 * preferencesModel.uiScale

			height: 35 * preferencesModel.uiScale
			width : 100 * preferencesModel.uiScale
			clip  : true
			color : jaspTheme.white

			border.width: passwordText.activeFocus ? 3 : 1
			border.color: passwordText.activeFocus ? jaspTheme.focusBorderColor  : jaspTheme.grayDarker

			TextInput
			{
				id: passwordText

				text:fileMenuModel.osf.password

				anchors.fill		: parent
				anchors.leftMargin	: 10 * preferencesModel.uiScale
				verticalAlignment	: Text.AlignVCenter
				echoMode			: TextInput.Password
				selectByMouse		: true
				selectedTextColor	: jaspTheme.textDisabled
				selectionColor		: jaspTheme.itemSelectedColor
				color				: jaspTheme.textEnabled

				font.pixelSize    : 14 * preferencesModel.uiScale


				onTextChanged:	fileMenuModel.osf.password = text;
				onAccepted:		fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)

				KeyNavigation.up		: usernameText
				KeyNavigation.backtab	: usernameText
				KeyNavigation.down		: loginButton
				KeyNavigation.tab		: loginButton

			}
		}

		RectangularButton
		{
			id: loginButton

			height   : 35  * preferencesModel.uiScale
			text     : qsTr("Sign in")
			color    : jaspTheme.jaspGreen
			border.width: loginButton.activeFocus ? 3 : 1
			border.color: loginButton.activeFocus ? jaspTheme.focusBorderColor : jaspTheme.grayDarker

			textColor: jaspTheme.white

			anchors.top  : passwordInput.bottom
			anchors.right: parent.right
			anchors.left : parent.left

			anchors.topMargin  : 15  * preferencesModel.uiScale
			anchors.rightMargin: 20  * preferencesModel.uiScale
			anchors.leftMargin : 20  * preferencesModel.uiScale

			onClicked:			fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)

			KeyNavigation.up		: passwordText
			KeyNavigation.backtab	: passwordText
			KeyNavigation.down		: idRememberMe
			KeyNavigation.tab		: idRememberMe
		}

		CheckBox
		{
			id: idRememberMe

			checked: fileMenuModel.osf.rememberme
			label   : qsTr("Remember me")

			anchors.left  : parent.left
			anchors.right : parent.right
			anchors.bottom: parent.bottom

			anchors.bottomMargin: 30 * preferencesModel.uiScale
			anchors.leftMargin  : 20 * preferencesModel.uiScale
			anchors.topMargin   : 10 * preferencesModel.uiScale

			onCheckedChanged:	fileMenuModel.osf.rememberme = checked

			KeyNavigation.up		: loginButton
			KeyNavigation.backtab	: loginButton
		}
	}

	Item
	{
		id: linksBox

		width : osfLoginBox.width
		height: 60 * preferencesModel.uiScale

		anchors.top             : osfLoginBox.bottom
		anchors.horizontalCenter: parent.horizontalCenter
		anchors.topMargin       : 10 * preferencesModel.uiScale

		Text {
			id: linkOSF

			text			: qsTr("About the OSF")
			textFormat		: Text.StyledText
			font.pointSize	: 11 * preferencesModel.uiScale
			font.family		: jaspTheme.font.family
			font.underline	: true
			color			: jaspTheme.blueDarker

			anchors.left    : parent.left
			anchors.top	    : parent.top
			anchors.leftMargin: 5 * preferencesModel.uiScale

			MouseArea
			{
				anchors.fill: parent
				hoverEnabled: true
				cursorShape : Qt.PointingHandCursor
				onClicked   : Qt.openUrlExternally("http://help.osf.io")
			}
		}

		Text
		{
			id: linkRegister

			text			: qsTr("Register")
			textFormat		: Text.StyledText
			font.pointSize	: 11 * preferencesModel.uiScale
			font.family		: jaspTheme.font.family
			font.underline	: true
			color			: jaspTheme.blueDarker

			anchors.top        : parent.top
			anchors.right      : parent.right
			anchors.rightMargin: 5 * preferencesModel.uiScale

			MouseArea
			{
				anchors.fill: parent
				hoverEnabled: true
				cursorShape : Qt.PointingHandCursor
				onClicked   : Qt.openUrlExternally("https://osf.io")
			}
		}

		Text
		{
			text			: qsTr("<sup>*</sup>OSF account login only, ORCID or institutional login are not supported.")
			textFormat		: Text.RichText
			font.pointSize	: 8 * preferencesModel.uiScale
			font.family		: jaspTheme.font.family
			font.italic		: true
			color			: jaspTheme.textEnabled
			wrapMode		: Text.Wrap

			anchors
			{
				bottom:				parent.bottom
				left:				parent.left
				right:				parent.right
			}
		}
	}
}
