import QtQuick
import QtQuick.Controls

MouseArea
{
	id:							theMouseArea
	//hoverEnabled:				true
	propagateComposedEvents:	true
	//preventStealing:			true

	cursorShape:	dragging ? Qt.ClosedHandCursor : Qt.OpenHandCursor

	property int toolTipDelay:		jaspTheme.toolTipDelay
	property int toolTipTimeOut:	jaspTheme.toolTipTimeout
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
		background:		Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }
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
