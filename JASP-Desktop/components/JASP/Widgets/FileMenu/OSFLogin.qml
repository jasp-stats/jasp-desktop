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

import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Controls 1.0
import JASP.Theme 1.0
import JASP.Widgets 1.0

Rectangle
{
	id: osfLogin

	color: Theme.grayMuchLighter

	Component.onCompleted:
	{
		usernameText.focus = true
	}

	Image
	{
		id: osfLogo

		height: 100
		width : 100
		source: "qrc:/images/osf-logo.png"

		sourceSize.width : width  * 2
		sourceSize.height: height * 2

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.bottomMargin    : 20
	}

	Label
	{
		id: labelOSF

		width : osfLoginBox.width
		height: 40

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top: osfLogo.bottom
		anchors.bottomMargin: 20

		text : qsTr("OSF")
		color: Theme.black
		font : Theme.fontLabel
	}

	Text
	{
		id: labelExplain

		text : qsTr("Sign in with your OSF account to continue")
		color:Theme.black
		font.pointSize: 11

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top             : labelOSF.bottom
		anchors.topMargin       : 20
	}

	Rectangle
	{
		id: osfLoginBox

		// TODO: Should be in Theme?
		height: 240
		width : 250
		color : Theme.grayMuchLighter

		border.width: 1
		border.color: Theme.grayDarker

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top: labelExplain.bottom
		anchors.topMargin: 10

		Rectangle
		{
			id: usernameInput

			anchors.left : parent.left
			anchors.right: parent.right
			anchors.top  : parent.top

			anchors.leftMargin : 20
			anchors.rightMargin: 20
			anchors.topMargin  : 30

			height: 35
			width : 100
			clip  : true
			color : Theme.white
			border.width: usernameText.activeFocus ? 3 : 1
			border.color: usernameText.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

			TextInput
			{
				id: usernameText

				text:fileMenuModel.osf.username

				anchors.fill      : parent
				anchors.leftMargin: 10
				selectByMouse     : true

				verticalAlignment : Text.AlignVCenter
				font.pixelSize    : 14

				onTextChanged:
				{
					fileMenuModel.osf.username = text
				}
				onAccepted:
				{
					passwordText.focus = true
				}
				KeyNavigation.down		: passwordText
				KeyNavigation.tab		: passwordText
			}
		}

		Rectangle
		{
			id: passwordInput

			anchors.left  : parent.left
			anchors.right : parent.right
			anchors.top   : usernameInput.bottom

			anchors.leftMargin : 20
			anchors.rightMargin: 20
			anchors.topMargin  : 15

			height: 35
			width : 100
			clip  : true
			color : Theme.white

			border.width: passwordText.activeFocus ? 3 : 1
			border.color: passwordText.activeFocus ? Theme.focusBorderColor  : Theme.grayDarker

			TextInput
			{
				id: passwordText

				text:fileMenuModel.osf.password

				anchors.fill      : parent
				anchors.leftMargin: 10
				verticalAlignment : Text.AlignVCenter
				echoMode          : TextInput.Password
				selectByMouse     : true
				font.pixelSize    : 14


				onTextChanged:
				{
					fileMenuModel.osf.password = text;
				}
				onAccepted:
				{
					fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)
				}

				KeyNavigation.up		: usernameText
				KeyNavigation.backtab	: usernameText
				KeyNavigation.down		: loginButton
				KeyNavigation.tab		: loginButton

			}
		}

		RectangularButton
		{
			id: loginButton

			height   : 35
			text     : qsTr("Sign in")
			color    : "#5cb85c"  // TODO: Move this to Theme.qml
			border.width: loginButton.activeFocus ? 3 : 1
			border.color: loginButton.activeFocus ? Theme.focusBorderColor : Theme.grayDarker

			textColor: "white"

			anchors.top  : passwordInput.bottom
			anchors.right: parent.right
			anchors.left : parent.left

			anchors.topMargin  : 15
			anchors.rightMargin: 20
			anchors.leftMargin : 20

			onClicked:
			{
				fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, fileMenuModel.osf.password)
			}

			KeyNavigation.up		: passwordText
			KeyNavigation.backtab	: passwordText
			KeyNavigation.down		: idRememberMe
			KeyNavigation.tab		: idRememberMe
		}

		CheckBox
		{
			id: idRememberMe

			checked: fileMenuModel.osf.rememberme
			text   : qsTr("Remember me")

			anchors.left  : parent.left
			anchors.right : parent.right
			anchors.bottom: parent.bottom

			anchors.bottomMargin: 30
			anchors.leftMargin  : 20
			anchors.topMargin   : 10

			onCheckedChanged:
			{
				fileMenuModel.osf.rememberme = checked
			}

			KeyNavigation.up		: loginButton
			KeyNavigation.backtab	: loginButton
		}
	}

	Rectangle
	{
		id: linksBox

		width : osfLoginBox.width
		height: 30
		color : "transparent"

		anchors.top             : osfLoginBox.bottom
		anchors.horizontalCenter: parent.horizontalCenter
		anchors.topMargin       : 5

		Text {
			id: linkOSF

			text          :'<font color="#257bb2"><u>About the OSF</u></font>'
			textFormat    : Text.StyledText
			font.pointSize: 11

			anchors.left      : parent.left
			anchors.bottom    : parent.bottom
			anchors.leftMargin: 5

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

			text          :'<font color="#257bb2"><u>Register</u></font>'
			textFormat    : Text.StyledText
			font.pointSize: 11

			anchors.bottom     : parent.bottom
			anchors.right      : parent.right
			anchors.rightMargin: 5

			MouseArea
			{
				anchors.fill: parent
				hoverEnabled: true
				cursorShape : Qt.PointingHandCursor
				onClicked   : Qt.openUrlExternally("https://osf.io")
			}
		}
	}
}
