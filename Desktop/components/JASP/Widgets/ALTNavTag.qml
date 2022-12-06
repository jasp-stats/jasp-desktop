import QtQuick
import QtQuick.Controls
import JASP

//Text tag shown when alt navigation mode is enabled.
Item
{
	id:					tagRoot
	visible:			tagData.active
	width:				tagText.contentWidth + jaspTheme.contentMargin
	height:				tagText.height
	z:					99999
	activeFocusOnTab:	false

	required property ALTNavTag tagData

	Rectangle
	{
		color:				jaspTheme.altNavTagColor
		radius:				3 * jaspTheme.uiScale
		anchors.fill:		parent
		anchors.centerIn:	parent

		Text
		{
			id:							tagText
			text:						tagData.tagText
			color:						"white"
			font:						jaspTheme.fontLabel
			anchors.centerIn:			parent
			horizontalAlignment:		Text.AlignHCenter
			verticalAlignment:			Text.AlignVCenter
		}

	}
}
