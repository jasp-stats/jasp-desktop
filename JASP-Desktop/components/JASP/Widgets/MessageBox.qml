import QtQuick 2.11
import QtQuick.Controls 2.4
//import QtQuick.Dialogs 1.2
import JASP.Theme 1.0

Popup
//MessageDialog
{
	id: messageRoot

	x: (mainWindowRoot.width - width) / 2
	y: (mainWindowRoot.height - height) / 2

	modal: true
	//title: "A Message Box!"
	//standardButtons: Dialog.NoButton

	//implicitWidth:  Math.min(Math.max(400, messageText.contentWidth), mainWindowRoot.width * 0.5)
	//implicitHeight: 200

	//closePolicy:	Popup.CloseOnEscape //tweak this?

	height:		Math.max(titleRect.height + messageText.height, 200)
	width:		Math.min(mainWindowRoot.width * 0.5, Math.max(500, title.width))

	property bool isWarning: false
	property bool iconVisible: isWarning
	property real marginIcon:  iconVisible ? Theme.generalAnchorMargin : 0

	Rectangle
	{
		id:				titleRect
		height:			title.text != "" ? title.height + 2 : 0
		width:			parent.width - Theme.generalAnchorMargin
		color:			Theme.blueMuchLighter
		border.color:	Theme.blue
		visible:		title.text != ""

		Text
		{
			id:							title

			text:						"I should be changed, and I will be!"
			horizontalAlignment:		Text.AlignHCenter
			anchors.horizontalCenter:	parent.horizontalCenter
			height:						text !== "" ? implicitHeight : 0
		}
	}


	Image
	{
		id:			icon

		source:		messageRoot.isWarning ? "qrc:/icons/exclamation.svg" : ""
		visible:	messageRoot.iconVisible

		//sizes divided by two to account for HiDpi systems
		width:		messageRoot.iconVisible ? sourceSize.width  / 2 : 0
		height:		messageRoot.iconVisible ? sourceSize.height / 2 : 0

		sourceSize.width:	160
		sourceSize.height:	sourceSize.width

		anchors
		{
			left:			parent.left
			leftMargin:		messageRoot.marginIcon
			verticalCenter:	parent.verticalCenter
		}
	}

	Item
	{
		anchors
		{
			left:			icon.right
			leftMargin:		messageRoot.marginIcon
			top:			titleRect.bottom
			topMargin:		Theme.generalAnchorMargin
			bottom:			parent.bottom
			right:			parent.right
		}

		TextArea
		{
			id:						messageText
			text:					"Hello, I am a title!"
			readOnly:				true
			anchors.fill:			parent
			horizontalAlignment:	TextArea.AlignHCenter
			verticalAlignment:		TextArea.AlignVCenter
			wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere

		}
	}

	function showWarning(warningTitle, warningText)
	{

		messageText.text	= warningText
		title.text			= warningTitle
		isWarning			= true

		open()
	}
}
