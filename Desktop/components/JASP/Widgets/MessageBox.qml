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
	property real marginIcon:			iconVisible ? jaspTheme.generalAnchorMargin : 0

	Rectangle
	{
		id:				titleRect
		height:			title.text != "" ? title.height + 2 : 0
		width:			parent.width - jaspTheme.generalAnchorMargin
		color:			jaspTheme.blueMuchLighter
		border.color:	jaspTheme.blue
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

			property string sourceQuestion:		jaspTheme.iconPath + "/QuestionMark.png"
			property string sourceWarning:		jaspTheme.iconPath + "/exclamation.svg"

			source:		messageRoot.isWarning ? sourceWarning : messageRoot.isQuestion ? sourceQuestion : ""
			visible:	messageRoot.iconVisible

			//sizes divided by two to account for HiDpi systems
			width:		messageRoot.iconVisible ? sourceSize.width  / 2 : 0
			height:		messageRoot.iconVisible ? sourceSize.height / 2 : 0

			sourceSize.width:	160
			sourceSize.height:	160

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
			margins:		jaspTheme.generalAnchorMargin
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
		height:		!visible ? 0 : jaspTheme.messageBoxButtonHeight + (jaspTheme.generalAnchorMargin * 2)

		anchors
		{
			left:		parent.left
			right:		parent.right
			bottom:		parent.bottom
			margins:	jaspTheme.generalAnchorMargin
		}

		Item
		{
			id:				yesNoButtons
			anchors.fill:	parent

			RectangularButton
			{
				id:				yes
				text:			"Yes"
				height:			jaspTheme.messageBoxButtonHeight
				anchors
				{
					left:			parent.left
					right:			parent.horizontalCenter
					verticalCenter:	parent.verticalCenter
					rightMargin:	jaspTheme.generalAnchorMargin / 2
				}

				onClicked: messageRoot.close()
			}

			RectangularButton
			{
				id:				no
				text:			"No"
				height:			jaspTheme.messageBoxButtonHeight
				anchors
				{
					left:			parent.horizontalCenter
					right:			parent.right
					verticalCenter:	parent.verticalCenter
					leftMargin:		jaspTheme.generalAnchorMargin / 2
				}

				onClicked: messageRoot.close()
			}
		}

	}

	function showWarning(warningTitle, warningText)
	{
		console.log("Showing warning with title '" + warningTitle + "' and msg '" + warningText + "'")

		messageText.text	= warningText
		title.text			= warningTitle
		isWarning			= true
		isYesNo				= false
		isSaveDiscardCancel	= false
		isMessage			= false

		open()
	}

	function showMessage(messageTitle, message)
	{
		console.log("Showing message with title '" + messageTitle + "' and msg '" + message + "'")

		messageText.text	= message
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= false
		isSaveDiscardCancel	= false
		isMessage			= true

		open()
	}


	function showYesNo(messageTitle, message)
	{
		console.log("Showing messageBox with Yes/No with title '" + messageTitle + "' and msg '" + message + "'")

		messageText.text	= message
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= true
		isSaveDiscardCancel	= false
		isMessage			= false

		open()
	}

	function showSaveDiscardCancel(messageTitle, message)
	{
		console.log("Showing messageBox with SaveDiscardCancel with title '" + messageTitle + "' and msg '" + message + "'")

		messageText.text	= message
		title.text			= messageTitle
		isWarning			= false
		isYesNo				= false
		isSaveDiscardCancel	= true
		isMessage			= false

		open()
	}

}
