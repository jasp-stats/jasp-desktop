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

Rectangle {
	id: osfLogin

	property bool rememberme: fileMenuModel.osf.rememberme
	property string username: fileMenuModel.osf.username
	property string password: fileMenuModel.osf.password

	color: Theme.grayMuchLighter

	Component.onCompleted: {
		usernameText.focus = true
		fileMenuModel.osf.updateLoginScreen()
		usernameText.text = username
		passwordText.text = password
	}

	Image {
		id: osfLogo

		height: 100
		width : 100
		source: "qrc:/images/osf-logo.png"

		sourceSize.width : width  * 2
		sourceSize.height: height * 2

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.bottomMargin    : 20
	}

	Label {
		id: labelOSF

		width : osfLoginBox.width
		height: 40

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top: osfLogo.bottom
		anchors.bottomMargin: 20

		text : "OSF"
		color: Theme.black
		font : Theme.fontLabel
	}

	Text {
		id: labelExplain

		text : qsTr("Sign in with your OSF account to continue")
		color: Theme.grayDarker
		font.pointSize: 9

		verticalAlignment  : Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		anchors.horizontalCenter: parent.horizontalCenter
		anchors.top             : labelOSF.bottom
		anchors.topMargin       : 20
	}

	Rectangle {
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

		Rectangle {
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

			TextInput {
				id: usernameText

				anchors.fill      : parent
				anchors.leftMargin: 10
				selectByMouse     : true

				verticalAlignment : Text.AlignVCenter
				font.pixelSize    : 14

				onTextChanged: {
					fileMenuModel.osf.username = usernameText.text
				}
				onAccepted: {
					passwordText.focus = true
				}
			}
		}

		Rectangle {
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

			TextInput {
				id: passwordText

				anchors.fill      : parent
				anchors.leftMargin: 10
				verticalAlignment : Text.AlignVCenter
				echoMode          : TextInput.Password
				selectByMouse     : true
				font.pixelSize    : 14

				onTextChanged: {
					fileMenuModel.osf.password = text;
				}
				onAccepted: {
					fileMenuModel.osf.loginRequested(username, password)
				}
			}
		}

		RectangularButton {
			id: loginButton

			height   : 35
			text     : qsTr("Sign in")
			color    : "#5cb85c"  // TODO: Move this to Theme.qml
			textColor: "white"

			anchors.top  : passwordInput.bottom
			anchors.right: parent.right
			anchors.left : parent.left

			anchors.topMargin  : 15
			anchors.rightMargin: 20
			anchors.leftMargin : 20

			onClicked: {
				fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, password)
			}
		}

		CheckBox {
			id: idRememberMe

			checked: rememberme
			text   : qsTr("Remember me")

			anchors.left  : parent.left
			anchors.right : parent.right
			anchors.bottom: parent.bottom

			anchors.bottomMargin: 30
			anchors.leftMargin  : 20
			anchors.topMargin   : 10

			onClicked: {
				fileMenuModel.osf.remembermeCheckChanged(checked)
			}
		}
	}

	Rectangle {
		id: linksBox

		width : osfLoginBox.width
		height: 30
		color : "transparent"

		anchors.top             : osfLoginBox.bottom
		anchors.horizontalCenter: parent.horizontalCenter
		anchors.topMargin       : 10

		Text {
			id: linkOSF

			text          :'<font color="#257bb2"><u>About the OSF</u></font>'
			textFormat    : Text.StyledText
			font.pointSize: 10

			anchors.left      : parent.left
			anchors.bottom    : parent.bottom
			anchors.topMargin : 10
			anchors.leftMargin: 20

			MouseArea {
				anchors.fill: parent
				hoverEnabled: true
				cursorShape : containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
				onClicked   : Qt.openUrlExternally("http://help.osf.io")
			}
		}

		Text {
			id: linkRegister

			text          :'<font color="#257bb2"><u>Register</u></font>'
			textFormat    : Text.StyledText
			font.pointSize: 10

			anchors.bottom     : parent.bottom
			anchors.right      : parent.right
			anchors.topMargin  : 10
			anchors.rightMargin: 20

			MouseArea {
				anchors.fill: parent
				hoverEnabled: true
				cursorShape : containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
				onClicked   : Qt.openUrlExternally("https://osf.io")
			}
		}
	}

	// TODO: Shadows don't look good when they overlap at the corners.
	// Item {
	// 	id    : dropShadowRight
	// 	y     : osfLoginBox.y - Theme.shadowRadius
	// 	x     : osfLoginBox.x + osfLoginBox.width
	// 	z     : -1
	// 	height: osfLoginBox.height + 2 * Theme.shadowRadius
	// 	width : Theme.shadowRadius
	//
	// 	Rectangle {
	//
	// 		anchors.centerIn: parent
	// 		rotation: -90
	// 		gradient: Gradient {
	// 			GradientStop { position: 0.0; color: Theme.shadow }
	// 			GradientStop { position: 1.0; color: "transparent" }
	// 		}
	// 		height  : dropShadowRight.width
	// 		width   : dropShadowRight.height
	// 	}
	// }
	//
	// Item {
	// 	id    : dropShadowLeft
	// 	y     : osfLoginBox.y - Theme.shadowRadius
	// 	x     : osfLoginBox.x - width
	// 	z     : -1
	// 	height: osfLoginBox.height + 2 * Theme.shadowRadius
	// 	width : Theme.shadowRadius
	//
	// 	Rectangle {
	//
	// 		anchors.centerIn: parent
	// 		rotation: -90
	// 		gradient: Gradient {
	// 			GradientStop { position: 1.0; color: Theme.shadow }
	// 			GradientStop { position: 0.0; color: "transparent" }
	// 		}
	// 		height  : dropShadowLeft.width
	// 		width   : dropShadowLeft.height
	// 	}
	// }
	//
	// Item {
	// 	id    : dropShadowTop
	// 	x     : osfLoginBox.x - Theme.shadowRadius
	// 	y     : osfLoginBox.y - Theme.shadowRadius
	// 	z     : -2
	//
	// 	height: Theme.shadowRadius
	// 	width : osfLoginBox.width + 2 * Theme.shadowRadius
	//
	// 	Rectangle {
	// 		anchors.centerIn: parent
	// 		// rotation: -90
	// 		gradient:	Gradient {
	// 			GradientStop { position: 1.0; color: Theme.shadow }
	// 			GradientStop { position: 0.0; color: "transparent" }
	// 		}
	// 		height:		dropShadowTop.height
	// 		width:		dropShadowTop.width
	// 	}
	// }
	//
	// Item {
	// 	id    : dropShadowBottom
	// 	x     : osfLoginBox.x - Theme.shadowRadius
	// 	y     : osfLoginBox.y + osfLoginBox.height
	// 	z     : -2
	//
	// 	height: Theme.shadowRadius
	// 	width : osfLoginBox.width + 2 * Theme.shadowRadius
	//
	// 	Rectangle {
	// 		anchors.centerIn: parent
	//
	// 		gradient:	Gradient {
	// 			GradientStop { position: 0.0; color: Theme.shadow }
	// 			GradientStop { position: 1.0; color: "transparent" }
	// 		}
	// 		height:		dropShadowBottom.height
	// 		width:		dropShadowBottom.width
	// 	}
	// }
}
