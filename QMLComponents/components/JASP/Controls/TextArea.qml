import QtQuick
import QtQuick.Controls	as QTC
import QtQuick.Layouts
import JASP.Controls
import JASP

/*!
    \qmltype TextArea
    \inqmlmodule JASP.Controls 1.0
    \brief A multi-line text input with optional line numbers and syntax highlighting.

	Supports multiple text types (default, source,
    JAGS model, lavaan model) with corresponding syntax highlighting.
    Includes Ctrl+Enter to apply, undo/redo support, and scrollable editing.

    \section1 R Binding

    \list
    \li \b{R Type:} \c character (string)
    \li \b{Default:} ""
    \endlist

    \section1 Properties

    \list
    \li \b name (string) - R option name this control binds to. Default: "".
    \li \b title (string) - Label displayed above the text area. Default: "".
    \li \b text (string) - Current text content. Default: "".
    \li \b textType (enum) - Type of text (JASP.TextTypeDefault, JASP.TextTypeSource, JASP.TextTypeJAGSmodel, JASP.TextTypeLavaan). Default: JASP.TextTypeDefault.
    \li \b showLineNumber (bool) - Show line numbers in the gutter. Default: false.
    \li \b wrapMode (enum) - Text wrapping mode. Default: TextEdit.Wrap.
    \li \b separator (string) - Separator used to split text into list values. Default: "\\n".
    \li \b trim (bool) - Trim whitespace before applying. Default: false.
    \li \b useTabAsSpaces (bool) - Convert Tab key to spaces. Default: true.
    \li \b placeholderText (string) - Placeholder text. Default: "".
    \endlist

    \section1 Inherited Properties

    \list
    \li \b enabled (bool) - Whether the control is interactive. Default: true.
    \li \b visible (bool) - Whether the control is visible. Default: true.
    \li \b info (string) - Info that will be used by tooltip and to generate the help. Default: "".
    \li \b toolTip (string) - This property overwrite info property, in order to display a simpler tooltip text. Default: "".
    \endlist

    \section1 Example

    \qml
    TextArea {
        name: "rCode"
        title: qsTr("R Script")
        textType: JASP.TextTypeSource
        showLineNumber: true
    }
    \endqml
*/
TextAreaBase
{
	id:					textArea
	height:				jaspTheme.defaultTextAreaHeight
	implicitHeight:		height
	width:				parent.width
	implicitWidth:		width
	focusIndicator:		flickableRectangle
	innerControl:		control
	infoText:			applyScriptInfo

	
	property alias	control				: control
	property alias	wrapMode			: control.wrapMode
	property alias	text				: control.text
	property string applyScriptInfo		: Qt.platform.os === "osx" ? qsTr("\u2318 + Enter to apply") : qsTr("Ctrl + Enter to apply")
	property alias  font				: control.font
	property alias  textDocument		: control.textDocument
	property bool   trim				: false
	property var    modelParameterView	: null
	property string separator			: "\n"
	property alias	radius				: flickableRectangle.radius
	property alias	placeholderText		: control.placeholderText
	property var	undoModel
	property bool	useTabAsSpaces		: true
	property var	nextTabItem
	property bool   showLineNumber      : false
    
	Component.onCompleted: control.editingFinished.connect(editingFinished)
	
	Accessible.role:			Accessible.EditableText
	Accessible.name:			title
	Accessible.description:		info === undefined || info == "" ? toolTip !== undefined && toolTip != "" ? toolTip :  qsTr("A text area %1").arg(title) : info

	
    
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

			Rectangle
			{
				id:					lineNumbersRect
				anchors.top:		parent.top
				anchors.left:		parent.left
				anchors.topMargin:	jaspTheme.contentMargin
				anchors.leftMargin:	2
				visible:			textArea.showLineNumber
				width:				lineNumbersRect.visible ? lineNumbers.width : 0
				height:				Math.max(flickableRectangle.height, control.contentHeight) + 10
				color:				"transparent"
				
				FontMetrics
				{
					font:			jaspTheme.fontCode
					id:				lineNumberWidthDeterminer
				}

				ListView
				{
					id:				lineNumbers
					width:			lineNumberWidthDeterminer.advanceWidth(control.lineCount) + jaspTheme.itemPadding
					height:			parent.height
					model:			control.lineCount
					delegate:		Text 
					{
						text:					"<i>%1</i>".arg(index + 1)
						font:					jaspTheme.fontCode
						color:					jaspTheme.grayDarker
						height:					control.contentHeight / control.lineCount
						anchors.right:			parent.right
						anchors.rightMargin:	jaspTheme.itemPadding / 2
					}
				}

				Rectangle
				{
					id:             separator
					anchors.top:    parent.top
					anchors.left:   parent.right
					width:          2 * preferencesModel.uiScale
					height:         Math.max(flickableRectangle.height, control.contentHeight) + 10
					color:          jaspTheme.borderColor
				}
			}

			QTC.TextArea.flickable: QTC.TextArea
			{
				id:					control
				selectByMouse:		true
				selectedTextColor:	jaspTheme.white
				selectionColor:		jaspTheme.itemSelectedColor
				font:				textArea.textType === JASP.TextTypeDefault || textArea.textType === JASP.TextTypeSource ? jaspTheme.font : jaspTheme.fontCode
				color:				textArea.enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
				leftPadding:		!textArea.showLineNumber ? 2 * jaspTheme.contentMargin : lineNumbers.width + 2 * jaspTheme.contentMargin
				
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

	Rectangle
	{
		anchors.fill:			infoText
		opacity:				textArea.hasScriptError ? 1 : .8
		color:					textArea.hasScriptError ? jaspTheme.errorMessagesBackgroundColor : flickableRectangle.color
	}

	Text
	{
		id:						infoText
		text:					textArea.infoText
		z:						2
		anchors.bottom:			parent.bottom
		anchors.right:			parent.right
		anchors.margins:		4 * preferencesModel.uiScale
		leftPadding:			5 * preferencesModel.uiScale
		rightPadding:			leftPadding
		bottomPadding:			3 * preferencesModel.uiScale
		topPadding:				bottomPadding
		font:					jaspTheme.font
		horizontalAlignment:	Text.AlignHCenter
		verticalAlignment:		Text.AlignVCenter
		color:					!enabled ? jaspTheme.textDisabled : textArea.hasScriptError ? jaspTheme.textEnabled : jaspTheme.grayDarker
		wrapMode:				Text.Wrap
		width:					implicitWidth > textArea.width - 2 * anchors.margins ? textArea.width - 2 * anchors.margins : implicitWidth
	}
}
