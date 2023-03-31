import QtQuick			2.11
import QtQuick.Controls 2.4
import JASP.Widgets		1.0

Item
{
	id:					uiScaleNotifier
	width:				125
	height:				60
	z:					100
	visible:			uiScaleRect.opacity > 0

	property bool		uiScaleIsLoadedFromSettings:	false

	Connections
	{
		target:				preferencesModel
		function onUiScaleChanged(scale)
		{
			if (uiScaleNotifier.uiScaleIsLoadedFromSettings)
				uiScaleTimer.restart()

			uiScaleNotifier.uiScaleIsLoadedFromSettings = true;
		}
	}

	Rectangle
	{
		id:					uiScaleRect
		anchors.fill:		parent
		radius:				20
		color:				jaspTheme.grayDarker
		opacity:			uiScaleNotifier.uiScaleIsLoadedFromSettings && uiScaleTimer.running ? 0.8 : 0

		Behavior on opacity	{ enabled: preferencesModel.animationsOn; PropertyAnimation { duration: 100 } }
	}

	Timer
	{
		id:				uiScaleTimer
		running:		false
		repeat:			false
		interval:		750
	}

	Text
	{
		color:				jaspTheme.white
		font.family: 		jaspTheme.fontLabel.family
		font.bold:			jaspTheme.fontLabel.bold
		font.pixelSize:		26
		anchors.centerIn:	parent
		text:				Math.round(preferencesModel.uiScale * 100) + "%"
		z:					2
	}
}
