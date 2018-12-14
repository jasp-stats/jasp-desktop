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

	closePolicy: isQuestion ? Popup.NoAutoClose : 	Popup.CloseOnEscape | Popup.CloseOnPressOutside

	height:		Math.max(titleRect.height + Math.max(icon.height, messageText.height) + buttons.height, 200)
	width:		Math.min(mainWindowRoot.width * 0.5, Math.max(500, title.width))

	property bool isWarning:			false
	property bool isMessage:			false
	property bool isYesNo:				false
	property bool isSaveDiscardCancel:	false

	property bool isQuestion:			isYesNo || isSaveDiscardCancel

	property bool iconVisible:			isWarning || isYesNo || isSaveDiscardCancel
	property real marginIcon:			iconVisible ? Theme.generalAnchorMargin : 0

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

			text:						"Hello, I am a title!"
			horizontalAlignment:		Text.AlignHCenter
			anchors.horizontalCenter:	parent.horizontalCenter
			height:						text !== "" ? implicitHeight : 0
		}
	}

	Item
	{
		id: iconBox

		anchors
		{
			top:			titleRect.bottom
			left:			parent.left
			bottom:			buttons.top
			leftMargin:		messageRoot.marginIcon
		}

		width: icon.width

		Image
		{
			id:			icon

			property string sourceQuestion:		"qrc:/icons/QuestionMark.png"
			property string sourceWarning:		"qrc:/icons/exclamation.svg"

			source:		messageRoot.isWarning ? sourceWarning : messageRoot.isQuestion ? sourceQuestion : ""
			visible:	messageRoot.iconVisible

			//sizes divided by two to account for HiDpi systems
			width:		messageRoot.iconVisible ? sourceSize.width  / 2 : 0
			height:		messageRoot.iconVisible ? sourceSize.height / 2 : 0

			sourceSize.width:	160
			sourceSize.height:	sourceSize.width

			anchors.centerIn:	parent
		}


	}

	Item
	{
		anchors
		{
			left:			iconBox.right
			top:			titleRect.bottom
			bottom:			buttons.top
			right:			parent.right
			margins:		Theme.generalAnchorMargin
		}

		TextArea
		{
			id:						messageText
			text:					"I should be changed, and I will be!"
			readOnly:				true
			anchors.fill:			parent
			horizontalAlignment:	TextArea.AlignLeft
			verticalAlignment:		TextArea.AlignVCenter
			wrapMode:				TextArea.WrapAtWordBoundaryOrAnywhere

		}
	}

	Item
	{
		id:			buttons
		visible:	messageRoot.isQuestion
		height:		!visible ? 0 : Theme.messageBoxButtonHeight + (Theme.generalAnchorMargin * 2)

		anchors
		{
			left:		parent.left
			right:		parent.right
			bottom:		parent.bottom
			margins:	Theme.generalAnchorMargin
		}

		Item
		{
			id:				yesNoButtons
			anchors.fill:	parent

			FilterButton
			{
				id:				yes
				text:			"Yes"
				height:			Theme.messageBoxButtonHeight
				anchors
				{
					left:			parent.left
					right:			parent.horizontalCenter
					verticalCenter:	parent.verticalCenter
					rightMargin:	Theme.generalAnchorMargin / 2
				}

				onClicked: messageRoot.close()
			}

			FilterButton
			{
				id:				no
				text:			"No"
				height:			Theme.messageBoxButtonHeight
				anchors
				{
					left:			parent.horizontalCenter
					right:			parent.right
					verticalCenter:	parent.verticalCenter
					leftMargin:		Theme.generalAnchorMargin / 2
				}

				onClicked: messageRoot.close()
			}
		}

	}

	function showWarning(warningTitle, warningText)
	{

		messageText.text	= warningText
		title.text			= warningTitle
		isWarning			= true
		isYesNo				= false
		isSaveDiscardCancel	= false
		isMessage			= false

		open()
	}

	function showMessage(messageTitle, messageText)
	{

		messageText.text	= messageText
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= false
		isSaveDiscardCancel	= false
		isMessage			= true

		open()
	}


	function showYesNo(messageTitle, messageText)
	{

		messageText.text	= messageText
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= true
		isSaveDiscardCancel	= false
		isMessage			= false

		open()
	}

	function showSaveDiscardCancel(messageTitle, messageText)
	{

		messageText.text	= messageText
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= false
		isSaveDiscardCancel	= true
		isMessage			= false

		open()
	}

}
