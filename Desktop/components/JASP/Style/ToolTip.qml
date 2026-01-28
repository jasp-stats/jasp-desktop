import QtQuick
import QtQuick.Controls.Basic as QtC

QtC.ToolTip {
	id: control
	implicitWidth: Math.min(jaspTheme.tooltipMaxWidth, contentItem.implicitWidth + leftPadding + rightPadding)

	background: Rectangle
	{
		color:	jaspTheme.tooltipBackgroundColor
		radius: jaspTheme.borderRadius
		border
		{
			color: jaspTheme.uiBorder
			width: 2
		}
	}
	contentItem: Text
	{
		text: control.text
		font: jaspTheme.font
		wrapMode: Text.Wrap
		color: jaspTheme.textEnabled
	}

	timeout:	jaspTheme.toolTipTimeout
	delay:		jaspTheme.toolTipDelay
}
