import QtQuick				2.11
import JASP.Theme			1.0
import QtQuick.Controls		2.4

FocusScope
{
	id: jaspControl

	property string	controlType:			"JASPControl"
	property string name:					""
	property bool	hasTabFocus:			true
	property bool	isBound:				true
	property bool	debug:					false
	property bool	useDefaultBackground:	false
	property var	controlBackground:		defaultBackground
	property int	backgroundWidth
	property int	backgroundHeight
	property bool   indent:                 false
	property alias	cursorShape:			controlMouseArea.cursorShape
	property alias	hovered:				controlMouseArea.containsMouse


	Rectangle
	{
		id:				defaultBackground
		visible:		useDefaultBackground
		anchors.fill:	parent
		color:			debug ? Theme.debugBackgroundColor : "transparent"
		border.width:	0
		border.color:	"transparent"
	}

	Component.onCompleted:
	{
		if (typeof(DEBUG_MODE) !== "undefined" && !DEBUG_MODE && debug)
			visible = false;

		if (typeof(control) !== "undefined")
		{
			if (useDefaultBackground)	control.background			= defaultBackground
			if (backgroundWidth)		control.background.width	= Qt.binding(function (){ return backgroundWidth;  })
			if (backgroundHeight)		control.background.height	= Qt.binding(function (){ return backgroundHeight; })
		}
	}

	states: [
		State
		{
			when: jaspControl.activeFocus && jaspControl.hasTabFocus
			PropertyChanges
			{
				target:			controlBackground
				border.color:	Theme.focusBorderColor
				border.width:	Theme.jaspControlHighlightWidth
				radius:			Theme.jaspControlPadding
			}
		}
	]

	transitions: [
		Transition
		{
			ParallelAnimation
			{
				NumberAnimation
				{
					target:				controlBackground
					properties:			"border.width";
					duration:			800;
					easing.type:		Easing.OutElastic;
					easing.amplitude:	1.5;
				}

				ColorAnimation
				{
					target:		controlBackground
					duration:	100
				}
			}
		}
	]

	property string	toolTip:				""

	ToolTip.text:				toolTip
	ToolTip.timeout:			Theme.toolTipTimeout
	ToolTip.delay:				Theme.toolTipDelay
	ToolTip.toolTip.font:		Theme.font
	ToolTip.visible:			toolTip !== "" && controlMouseArea.containsMouse
	ToolTip.toolTip.background: Rectangle { color:	Theme.tooltipBackgroundColor } //This does set it for ALL tooltips ever after

	MouseArea
	{
		z:					5
		anchors.fill:		parent
		id:					controlMouseArea
		hoverEnabled:		true
		acceptedButtons:	Qt.NoButton
		cursorShape:		Qt.PointingHandCursor
	}
}
