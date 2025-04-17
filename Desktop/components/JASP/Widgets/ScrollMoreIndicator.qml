import QtQuick 

Rectangle 
{
	id: 		scrollingGuideBottom
	z:			1000
	height:		Math.min(maxHeight, Math.max(0, extraSpace))
	
	property real extraSpace:	maxHeight
	property real maxHeight:	40 * jaspTheme.uiScale
	property bool upsideDown:	false

	visible: 	height > 0

	gradient: Gradient 
	{
		GradientStop { position: !upsideDown ? 0.0 : 1.0; color: "transparent" }
		GradientStop { position: !upsideDown ? 0.8 : 0.2; color: jaspTheme.shadow }
		GradientStop { position: !upsideDown ? 1.0 : 0.0; color: jaspTheme.shadow }
	}
}
