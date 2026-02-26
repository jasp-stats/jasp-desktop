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

	delay:		jaspTheme.toolTipDelay
	timeout:	jaspTheme.toolTipTimeout

	// Each time the ToolTip is used, QML resets the delay and timeout to the defualt values (delay: 0 and timeout: -1)
	onDelayChanged: if (delay === 0) delay = jaspTheme.toolTipDelay
	onTimeoutChanged: if (timeout === -1) timeout = jaspTheme.toolTipTimeout

}
