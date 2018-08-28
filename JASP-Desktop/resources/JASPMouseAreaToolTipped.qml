import QtQuick 2.9
import QtQuick.Controls 2.2


MouseArea
{
	id:				theMouseArea
	anchors.fill:	parent
	hoverEnabled:	true

	cursorShape:	!containsMouse ? Qt.ArrowCursor :  myFlickable.dragging ? Qt.ClosedHandCursor : Qt.OpenHandCursor

	property int toolTipDelay:		500
	property int toolTipTimeOut:	4000
	property string toolTipText:	"edit toolTipText!"

	property bool _toolTipVisible:	false
	property int _oldMouseX:		-1
	property int _oldMouseY:		-1


	ToolTip
	{
		id:			theToolTip
		text:		theMouseArea.toolTipText
		delay:		0
		timeout:	theMouseArea.toolTipTimeOut
		visible:	theMouseArea._toolTipVisible

		x:			theMouseArea.mouseX - (width / 2)
		y:			theMouseArea.mouseY + height
	}

	Timer
	{
		id:				theTimer
		interval:		theMouseArea.toolTipDelay
		running:		false

		onTriggered:	theMouseArea._toolTipVisible = true
	}

	onPositionChanged:
	{
		_toolTipVisible	= false
		_oldMouseX		= mouseX
		_oldMouseY		= mouseY

		theTimer.restart()
	}
}
