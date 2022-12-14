import QtQuick
import QtQuick.Controls
import JASP

//Tag shown when alt navigation mode is enabled.
ALTNavTagBase
{
	id:					tagRoot
	visible:			active
	width:				Math.max(jaspTheme.fontALTNavTag.pixelSize, textElem.contentWidth + jaspTheme.contentMargin * jaspTheme.uiScale)
	height:				textElem.height
	z:					99999

	Rectangle
	{
		color:				jaspTheme.altNavTagColor
		radius:				4 * jaspTheme.uiScale
		anchors.fill:		parent
		anchors.centerIn:	parent

		Text
		{
			id:							textElem
			text:						tagText
			color:						"white"
			font:						jaspTheme.fontALTNavTag
			anchors.centerIn:			parent
			horizontalAlignment:		Text.AlignHCenter
			verticalAlignment:			Text.AlignVCenter
		}

	}
}
