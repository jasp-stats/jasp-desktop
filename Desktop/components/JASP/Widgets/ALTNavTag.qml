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
		color:				"black"
		anchors.fill:		parent

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
