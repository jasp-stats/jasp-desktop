import QtQuick 

Rectangle 
{
	id: 		scrollingGuideBottom
	z:			3
	height:		Math.min(maxHeight, Math.max(0, extraSpace))
	
	property real extraSpace: maxHeight
	property real maxHeight:  40 * jaspTheme.uiScale

	visible: 	height > 0

	gradient: Gradient 
	{
		GradientStop { position: 0.0; color: "transparent" }
		GradientStop { position: 0.8; color: jaspTheme.shadow }
		GradientStop { position: 1.0; color: jaspTheme.shadow }
	}
}
