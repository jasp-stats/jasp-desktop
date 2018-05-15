import QtQuick 2.9
import QtQuick.Controls 2.2

Button
{
	property string toolTip: "This is a button"
	property bool disabled: false

	background: Rectangle
	{
		color: !parent.disabled && parent.hovered ? "white" : "lightGrey"
		border.color: !parent.disabled && parent.hovered ? "black" : "grey"
		border.width: 1
	}

	ToolTip.delay: 500
	ToolTip.timeout: 3500
	ToolTip.visible: hovered
	ToolTip.text: toolTip
}
