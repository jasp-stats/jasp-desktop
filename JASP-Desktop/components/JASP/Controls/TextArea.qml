import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3
import JASP				1.0

JASPControl
{
	id:					textArea
	controlType:		JASPControlBase.TextArea
	height:				jaspTheme.defaultTextAreaHeight
	implicitHeight:		height
	width:				parent.width
	implicitWidth:		width
	focusIndicator:		flickableRectangle
	innerControl:		control
	
	property alias	control				: control
	property alias  text				: control.text
    property string textType
	property string applyScriptInfo		: Qt.platform.os == "osx" ? qsTr("\u2318 + Enter to apply") : qsTr("Ctrl + Enter to apply")
	property alias  infoText			: infoText.text
	property bool   hasScriptError		: false
	property alias  font				: control.font
	property alias  textDocument		: control.textDocument
	property bool   trim				: false
	property var    modelParameterView	: null
	property string separator			: "\n"
	property var	separators			: []
    
    
    signal applyRequest()
    
	function userEnteredInput() {
		if (textArea.trim)
			textArea.text = textArea.text.trim();

		applyRequest();
	}

	Text
	{
		id:			textAreaTitle
		visible:	text !== ""
		font:		jaspTheme.font
		color:		!enabled ? jaspTheme.textDisabled : jaspTheme.textEnabled
		text:		textArea.title
	}

	Rectangle
	{
		id:				flickableRectangle
		anchors.top:	title !== "" ? textAreaTitle.bottom : parent.top
		anchors.topMargin: title !== "" ? jaspTheme.titleBottomMargin : 0
		width:			parent.implicitWidth
		height:			parent.implicitHeight - (title !== "" ? (textAreaTitle.height + jaspTheme.titleBottomMargin) : 0)
		color:			textArea.enabled ? jaspTheme.white : jaspTheme.whiteBroken
		border.width:	1
		border.color:	jaspTheme.borderColor

		Flickable
		{
			id:				flickable
			clip:			true
			boundsBehavior: Flickable.StopAtBounds
			anchors.fill:	parent

			TextArea.flickable: TextArea
			{
				id:					control
				selectByMouse:		true
				selectedTextColor:	jaspTheme.white
				selectionColor:		jaspTheme.itemSelectedColor

				font:				["JAGSmodel", "lavaan", "Rcode", "model"].includes(textArea.textType) ? jaspTheme.fontCode : jaspTheme.font
				color:				textArea.enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
				wrapMode:			TextArea.Wrap

				Keys.onPressed:
				{
					if (event.modifiers & Qt.ControlModifier)
					{
						if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
							userEnteredInput();
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
		font:					jaspTheme.font
		horizontalAlignment:	Text.AlignHCenter
		verticalAlignment:		Text.AlignVCenter
		color:					!enabled ? jaspTheme.textDisabled : textArea.hasScriptError ? jaspTheme.textEnabled : jaspTheme.grayDarker

		Rectangle
		{
			z:				-1
			anchors.fill:	infoText
			color:			textArea.hasScriptError ? jaspTheme.errorMessagesBackgroundColor : "transparent"
		}
	}
}
