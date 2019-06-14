import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl
{
	id:					textArea
	controlType:		"TextArea"
	implicitHeight:		Theme.defaultTextAreaHeight
	implicitWidth:		parent.width
	focusIndicator:		flickableRectangle
	
	property alias	control:			control
	property alias	text:				control.text
    property string textType
	property string applyScriptInfo:	""
	property alias	infoText:			infoText.text
	property bool	hasScriptError:		false
	property alias	font:				control.font
	property alias	textDocument:		control.textDocument
	property alias	title:				textAreaTitle.text
	property bool	trim:				false
    
    signal applyRequest()
    

	Text
	{
		id: textAreaTitle
		visible: text !== ""
		font: Theme.font
	}

	Rectangle
	{
		id:				flickableRectangle
		anchors.top:	title !== "" ? textAreaTitle.bottom : parent.top
		anchors.topMargin: title !== "" ? Theme.titleBottomMargin : 0
		width:			parent.implicitWidth
		height:			parent.implicitHeight - (title !== "" ? (textAreaTitle.height + Theme.titleBottomMargin) : 0)
		color:			textArea.enabled ? Theme.white : Theme.whiteBroken
		border.width:	1
		border.color:	Theme.borderColor

		Flickable
		{
			id:				flickable
			clip:			true
			boundsBehavior: Flickable.StopAtBounds
			anchors.fill:	parent

			TextArea.flickable: TextArea
			{
				id:				control
				selectByMouse:	true
				font:			Theme.font
				color:			textArea.enabled ? Theme.textEnabled : Theme.textDisabled
				wrapMode:		TextArea.Wrap

				Keys.onPressed:
				{
					if (event.modifiers & Qt.ControlModifier)
					{
						if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
							applyRequest();
					}
					else if ( event.key === Qt.Key_Tab)
					{
						control.insert(control.cursorPosition, "  ")
						event.accepted = true;
					}
					else
					{
						infoText.text = textArea.applyScriptInfo;
						textArea.hasScriptError = false;
					}
				}
			}

			ScrollBar.vertical: ScrollBar { }
		}
	}

	Text
	{
		id:						infoText
		z:						2
		anchors.bottom:			parent.bottom
		anchors.right:			parent.right
		anchors.margins:		4 * preferencesModel.uiScale
		leftPadding:			5 * preferencesModel.uiScale
		rightPadding:			leftPadding
		bottomPadding:			3 * preferencesModel.uiScale
		topPadding:				bottomPadding
		text:					textArea.applyScriptInfo
		font:					Theme.font
		horizontalAlignment:	Text.AlignHCenter
		verticalAlignment:		Text.AlignVCenter
		color:					!enabled ? Theme.textDisabled : textArea.hasScriptError ? Theme.textEnabled : Theme.grayDarker

		Rectangle
		{
			z:				-1
			anchors.fill:	infoText
			color:			textArea.hasScriptError ? Theme.errorMessagesBackgroundColor : "transparent"
		}
	}
}
