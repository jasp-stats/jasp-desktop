import QtQuick 2.0
import QtQuick.Controls 2.2
import JASP.Controls 1.0
import JASP.Theme 1.0


Rectangle {
	id : osfLogin

	property bool rememberme : fileMenuModel.osf.rememberme
	property string username : fileMenuModel.osf.username
	property string password : fileMenuModel.osf.password

	width :implicitWidth
	height : implicitHeight

	color: "#ececec"

	border.width: 1
	border.color: "darkgray"

	Component.onCompleted: {
		usernameText.focus = true
		fileMenuModel.osf.updateLoginScreen()
		usernameText.text = username
		passwordText.text = password

	}

	Label {
		id : loginToOSF

		width: implicitWidth
		height: 30
		anchors.top: parent.top
		anchors.left: parent.left
		anchors.leftMargin: 6
		anchors.topMargin: 12

		text : "Login to the OSF"
		font.family: "SansSerif"
		font.pixelSize: 16
		color: "black"
	}

	Label {
		id : usernameLabel

		width: 80
		height: 30
		anchors.top: loginToOSF.bottom
		anchors.left: parent.left
		anchors.leftMargin: 6
		anchors.topMargin: 6

		text : "Email"
		font.family: "SansSerif"
		font.pixelSize: 14
		color: "black"
		verticalAlignment: Text.AlignVCenter
	}

	Rectangle{

		id: usernameInput

		anchors.left: usernameLabel.right
		anchors.leftMargin: 6
		anchors.top: usernameLabel.top
		anchors.right: parent.right
		anchors.rightMargin: 12
		height: usernameLabel.height
		clip: true

		color: "white"
		border.width: usernameText.activeFocus ? 5 : 1
		border.color: usernameText.activeFocus ? Theme.focusBorderColor : "darkgray"

		TextInput {

			id: usernameText

			anchors.fill: parent
			anchors.leftMargin: 10
			selectByMouse: true

			verticalAlignment: Text.AlignVCenter
			font.pixelSize: 14

			onTextChanged: {
				fileMenuModel.osf.username = usernameText.text
			}

			onAccepted: {
				passwordText.focus = true
			}
		}
	}

	Label {
		id : passwordLabel

		height: 30
		width: 80
		anchors.top: usernameLabel.bottom
		anchors.left: parent.left
		anchors.leftMargin: 6
		anchors.topMargin: 6
		verticalAlignment: Text.AlignVCenter

		text : "Password"
		font.family: "SansSerif"
		font.pixelSize: 14
		color: "black"

	}

	Rectangle{

		id: passwordInput

		anchors.left: passwordLabel.right
		anchors.leftMargin: 6
		anchors.top: passwordLabel.top
		anchors.right: parent.right
		anchors.rightMargin: 12
		height: passwordLabel.height
		clip: true

		color: "white"
		border.width: passwordText.activeFocus ? 5 : 1
		border.color: passwordText.activeFocus ? Theme.focusBorderColor  : "darkgray"

		TextInput {
			id: passwordText

			anchors.fill: parent
			anchors.leftMargin: 10
			verticalAlignment: Text.AlignVCenter

			echoMode: TextInput.Password
			selectByMouse: true
			font.pixelSize: 14

			onTextChanged: {
				fileMenuModel.osf.password = text;
			}
			onAccepted:
			{
				fileMenuModel.osf.loginRequested(username, password)
			}
		}
	}

	CheckBox {
		id : idRememberMe

		checked: rememberme

		anchors.left: parent.left
		anchors.leftMargin: 6
		anchors.top: passwordInput.bottom
		anchors.topMargin: 12

		text: "Remember me"

		onClicked: {
			fileMenuModel.osf.remembermeCheckChanged(checked) }
	}

	Button {
		id: loginButton

	/*	background: Rectangle {
			anchors.fill: parent
			gradient: Gradient {
				GradientStop { position: 0 ; color:  "#e5e5e5" }
				GradientStop { position: 1 ; color:  "white" }
			}
			border.color: "gray"
			border.width: 1
		}*/

		width: 65
		height: 20
		anchors.top: passwordInput.bottom
		anchors.topMargin: 12
		anchors.right: parent.right
		anchors.rightMargin: 12

		text: "Login"

		onClicked: {
			fileMenuModel.osf.loginRequested(fileMenuModel.osf.username, password)
		}
	}

}
