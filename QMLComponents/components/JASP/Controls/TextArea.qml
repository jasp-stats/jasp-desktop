import QtQuick			2.11
import QtQuick.Controls 2.4 as QTC
import QtQuick.Layouts	1.3
import JASP				1.0

TextAreaBase
{
	id:					textArea
	height:				jaspTheme.defaultTextAreaHeight
	implicitHeight:		height
	width:				parent.width
	implicitWidth:		width
	focusIndicator:		flickableRectangle
	innerControl:		control
	
	property alias	control				: control
	property alias	text				: control.text
	property string applyScriptInfo		: Qt.platform.os == "osx" ? qsTr("\u2318 + Enter to apply") : qsTr("Ctrl + Enter to apply")
	property alias  infoText			: infoText.text
	property alias  font				: control.font
	property alias  textDocument		: control.textDocument
	property bool   trim				: false
	property var    modelParameterView	: null
	property string separator			: "\n"
	property var	separators			: []
	property alias	radius				: flickableRectangle.radius
	property alias	placeholderText		: control.placeholderText
	property var	undoModel
	property bool	useTabAsSpaces		: true
	property var	nextTabItem
    
    signal applyRequest()
	signal editingFinished()

	Component.onCompleted: control.editingFinished.connect(editingFinished)
    
	function userEnteredInput() {
		if (textArea.trim)
			textArea.text = textArea.text.trim();

		applyRequest();
	}

	function undo() {
		if (undoModel) {
			undoModel.undo()
			return true
		}
		else
			return false
	}

	function redo()
	{
		if (undoModel) {
			undoModel.redo()
			return true
		}
		else
			return false
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
		id:					flickableRectangle
		anchors.top:		title !== "" ? textAreaTitle.bottom : parent.top
		anchors.topMargin:	title !== "" ? jaspTheme.titleBottomMargin : 0
		width:				parent.implicitWidth
		height:				parent.implicitHeight - (title !== "" ? (textAreaTitle.height + jaspTheme.titleBottomMargin) : 0)
		color:				textArea.enabled ? jaspTheme.white : jaspTheme.whiteBroken
		border.width:		1
		border.color:		jaspTheme.borderColor
		radius:				jaspTheme.borderRadius

		Flickable
		{
			id:				flickable
			clip:			true
			boundsBehavior: Flickable.StopAtBounds
			anchors.fill:	parent

			QTC.TextArea.flickable: QTC.TextArea
			{
				id:					control
				selectByMouse:		true
				selectedTextColor:	jaspTheme.white
				selectionColor:		jaspTheme.itemSelectedColor

				font:				textArea.textType === JASP.TextTypeDefault || textArea.textType === JASP.TextTypeSource ? jaspTheme.font : jaspTheme.fontCode
				color:				textArea.enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled

				Component.onCompleted:
				{
					if (textArea.nextTabItem)
					{
						control.KeyNavigation.priority = KeyNavigation.BeforeItem
						control.KeyNavigation.tab =	textArea.nextTabItem
					}
				}

				Keys.onPressed: (event) =>
				{
					var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier)
					var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  )

					switch (event.key)
					{
					case Qt.Key_Return:
					case Qt.Key_Enter:
						if (controlPressed)
						{
							userEnteredInput();
							event.accepted = true;
						}
						break;
					case Qt.Key_Tab:
						if (useTabAsSpaces)
						{
							control.insert(control.cursorPosition, "  ")
							event.accepted = true;
						}
						break;
					case Qt.Key_Z:
						if (controlPressed)
						{
							if (shiftPressed)
							{
								if (textArea.redo())
									event.accepted = true;
							}
							else if (textArea.undo())
									event.accepted = true;
						}
						break;
					default:
						infoText.text = textArea.applyScriptInfo;
						textArea.hasScriptError = false;
					}
				}
			}

			QTC.ScrollBar.vertical: QTC.ScrollBar { }
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
		wrapMode:				Text.Wrap
		width:					implicitWidth > textArea.width - 2 * anchors.margins ? textArea.width - 2 * anchors.margins : implicitWidth

		Rectangle
		{
			z:				-1
			anchors.fill:	infoText
			color:			textArea.hasScriptError ? jaspTheme.errorMessagesBackgroundColor : "transparent"
		}
	}
}
