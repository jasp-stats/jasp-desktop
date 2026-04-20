import QtQuick
import QtQuick.Controls
import JASP
import JASP.Controls

//Tag shown when alt navigation mode is enabled.

/*!
    \qmltype ALTNavTag
    \inqmlmodule JASP.Controls 1.0
    \brief Internal tag displayed during ALT keyboard navigation.

    \note This is an internal UI component. Module developers do not use this control directly.
    It renders a small colored overlay tag showing the keyboard shortcut key for
    each focusable control when ALT navigation mode is active.

    \section1 Properties

    \list
    \li \b tagText (string) - The shortcut key character displayed in the tag.
    \li \b active (bool) - Whether the tag is currently visible. Default: false.
    \endlist
*/
ALTNavTagBase
{
	//It is initialised before(/besides?) the theme so we have to work around it with !jaspTheme ? undefined :
	//That way there are less warnings...

	id:					tagRoot
	visible:			active
	width:				!jaspTheme ? 0 : Math.max(jaspTheme.fontALTNavTag.pixelSize, textElem.contentWidth + jaspTheme.contentMargin * jaspTheme.uiScale)
	height:				textElem.height
	z:					99999

	Rectangle
	{
		color:				!jaspTheme ? "black"	: jaspTheme.altNavTagColor
		radius:				!jaspTheme ? 4			: 4 * jaspTheme.uiScale
		anchors.fill:		parent
		anchors.centerIn:	parent

		Text
		{
			id:							textElem
			text:						tagText
			color:						!jaspTheme ? "" : jaspTheme.white
			font:						!jaspTheme ? "sansserif" : jaspTheme.fontALTNavTag
			anchors.centerIn:			parent
			horizontalAlignment:		Text.AlignHCenter
			verticalAlignment:			Text.AlignVCenter
		}

	}
}
