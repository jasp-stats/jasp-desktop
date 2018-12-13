import QtQuick 2.11
import QtQuick.Controls 2.4
//import QtQuick.Dialogs 1.2

Popup
//MessageDialog
{
	id: messageRoot

	x: (mainWindowRoot.width - width) / 2
	y: (mainWindowRoot.height - height) / 2

	modal: true
	//title: "A Message Box!"
	//standardButtons: Dialog.NoButton

	implicitWidth:  Math.min(Math.max(400, messageText.contentWidth), mainWindowRoot.width * 0.5)
	implicitHeight: 200

	//closePolicy:	Popup.CloseOnEscape //tweak this?

	Text
	{
		id:							title

		text: "???"
		horizontalAlignment:		Text.AlignHCenter

		anchors.horizontalCenter:	parent.horizontalCenter
		anchors.top:				parent.top

		height:						text !== "" ? implicitHeight : 0
	}

	TextArea
	{
		id:						messageText
		text:					"Hello!"
		readOnly:				true
		anchors.centerIn:		parent
		horizontalAlignment:	TextArea.AlignHCenter
		verticalAlignment:		TextArea.AlignVCenter

	}

	function showWarning(warningTitle, warningText)
	{

		messageText.text	= warningText
		title.text			= warningTitle

		open()
	}
}
