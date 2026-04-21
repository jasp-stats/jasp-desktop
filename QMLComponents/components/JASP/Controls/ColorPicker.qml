import QtQuick
import JASP
import JASP.Controls
import QtQuick.Dialogs

Item {
	id: colorPicker
	property bool setButtonOnder:	false
	property alias textField:		inputField
	property alias button:			colorButton
	property alias colorDialog:		colorDialog
	property alias label:			inputField.label
	property alias name:			inputField.name
	property alias info:			inputField.info
	property alias buttonText:		colorButton.text
	property alias buttonInfo:		colorButton.info

	implicitHeight: colorButton.y + colorButton.height
	implicitWidth: colorButton.x + colorButton.width

	TextField
	{
		id:			inputField
		name:	"test"
		value:		jaspTheme.jaspBlue
		fieldWidth: 80 * jaspTheme.uiScale
		info: "jjjj"
		onValueChanged:
		{
			colorButton.control.color = inputField.value
			colorDialog.selectedColor = inputField.value
		}
	}

	Button
	{
		id:				colorButton
		anchors.top:	colorPicker.setButtonOnder ? inputField.bottom	: inputField.top
		anchors.left:	colorPicker.setButtonOnder ? inputField.left	: inputField.right
		anchors.leftMargin: colorPicker.setButtonOnder ? inputField.control.x : 0
		text:			qsTr("Click")
		control.color:	inputField.value
		height:			inputField.height
		width:			colorPicker.setButtonOnder ? inputField.fieldWidth : Math.max(implicitWidth, height)
		onClicked:		colorDialog.open()
		info:			qsTr("Click to pick up a color")
	}

	ColorDialog
	{
		id: colorDialog
		title: qsTr("Select the color")
		onAccepted: inputField.value = colorDialog.selectedColor
	}
}
