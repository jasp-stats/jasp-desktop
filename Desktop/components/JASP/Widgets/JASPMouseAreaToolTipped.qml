import QtQuick 2.9
import QtQuick.Controls 2.2

MouseArea
{
	id:							theMouseArea
	//hoverEnabled:				true
	propagateComposedEvents:	true
	//preventStealing:			true

	cursorShape:	dragging ? Qt.ClosedHandCursor : Qt.OpenHandCursor

	property int toolTipDelay:		500
	property int toolTipTimeOut:	4000
	property string toolTipText:	"edit toolTipText!"

	property bool _toolTipVisible:	false
	property int _oldMouseX:		-1
	property int _oldMouseY:		-1
	property bool dragging:			false


	ToolTip
	{
		id:			theToolTip
		text:		theMouseArea.toolTipText
		delay:		0
		timeout:	theMouseArea.toolTipTimeOut
		visible:	theMouseArea._toolTipVisible && toolTipText != ""

		x:			theMouseArea.mouseX - (width / 2)
		y:			theMouseArea.mouseY + height
	}

	Timer
	{
		id:				theTimer
		interval:		theMouseArea.toolTipDelay
		running:		false

		onTriggered:	theMouseArea._toolTipVisible = theMouseArea.containsMouse
	}

	onPositionChanged:
	{
		_toolTipVisible	= false
		_oldMouseX		= mouseX
		_oldMouseY		= mouseY

		theTimer.restart()
	}
}
