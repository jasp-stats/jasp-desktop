import QtQuick 

/*!
    \qmltype ScrollMoreIndicator
    \inqmlmodule JASP.Controls 1.0
    \brief An internal gradient shadow indicating more content is available by scrolling.

    Displays a directional gradient overlay at the edge of scrollable areas to
    hint that more content exists beyond the visible bounds.

    \note This is an internal component. Module developers do not need to use this directly.
*/
Rectangle 
{
	z:			1000
	height:		horizontal  ? implicitHeight : Math.min(maxExtend, Math.max(0, extraSpace * 0.25))
	width:		!horizontal ? implicitWidth  : Math.min(maxExtend, Math.max(0, extraSpace * 0.25))
	
	property real extraSpace:	maxExtend
	property real maxExtend:	30 * jaspTheme.uiScale
	property bool upsideDown:	false
	property bool horizontal:	false
	property color shadowCol:	jaspTheme.shadow

	visible: 	extraSpace > 0

	gradient: Gradient 
	{
		orientation:		horizontal ? Gradient.Horizontal : Gradient.Vertical
		
		GradientStop { position: !upsideDown ? 0.0   : 1.0;   color: "transparent" }
		GradientStop { position: !upsideDown ? 0.666 : 0.333; color: shadowCol }
		GradientStop { position: !upsideDown ? 1.0   : 0.0;   color: shadowCol }
	}
}
