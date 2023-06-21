import QtQuick
import QtQuick.Controls
import JASP

//Tag shown when alt navigation mode is enabled.
ALTNavTagBase
{
	//It is initialised before the theme so we have to work around it with !jaspTheme ? undefined :
	//That way there are less warnings...

	id:					tagRoot
	visible:			active
	width:				!jaspTheme ? 0 : Math.max(jaspTheme.fontALTNavTag.pixelSize, textElem.contentWidth + jaspTheme.contentMargin * jaspTheme.uiScale)
	height:				textElem.height
	z:					99999

	Rectangle
	{
		color:				!jaspTheme ? undefined	: jaspTheme.altNavTagColor
		radius:				!jaspTheme ? 4			: 4 * jaspTheme.uiScale
		anchors.fill:		parent
		anchors.centerIn:	parent

		Text
		{
			id:							textElem
			text:						tagText
			color:						"white"
			font:						!jaspTheme ? undefined : jaspTheme.fontALTNavTag
			anchors.centerIn:			parent
			horizontalAlignment:		Text.AlignHCenter
			verticalAlignment:			Text.AlignVCenter
		}

	}
}
