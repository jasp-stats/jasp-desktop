import QtQuick
import QtQuick.Controls
import JASP.Controls	as  JC
import JASP
Item
{
	id: onboardingOverlay

	property var	steps:			[]
	property int	currentIndex:	-1
	property bool	active:			false
	property bool	dontShowAgain:	false

	readonly property var	currentStep:	(currentIndex >= 0 && currentIndex < steps.length) ? steps[currentIndex] : null
	readonly property bool	lastStep:		currentIndex === steps.length - 1

	signal finished()
	signal skipped()

	anchors.fill:	parent
	z:				1000
	visible:		active && currentStep && currentStep.target && currentStep.target.width > 0 && currentStep.target.height > 0


	function start(fromStep)
	{
		if (steps.length === 0)
			return;

		dontShowAgain	= false;
		active			= true;
		_goToStep(fromStep !== undefined ? fromStep : 0);
	}

	function _runStepCallback(index, callbackName)
	{
		if (index >= 0 && index < steps.length && steps[index][callbackName])
			steps[index][callbackName]();
	}

	function _goToStep(index)
	{
		_runStepCallback(currentIndex, "onExit");
		currentIndex = index;
		_runStepCallback(currentIndex, "onEnter");
	}

	function next()
	{
		if (lastStep)
			finish();
		else
		{
			_goToStep(currentIndex + 1);
			preferencesModel.onboardingStep = currentIndex;
		}
	}

	function finish()
	{
		_runStepCallback(currentIndex, "onExit");
		active			= false;
		currentIndex	= -1;

		if (dontShowAgain)
			preferencesModel.onboardingCompleted	= true;
		else
			preferencesModel.onboardingStep		= 0;

		onboardingOverlay.finished();
	}

	function skip()
	{
		_runStepCallback(currentIndex, "onExit");
		active			= false;
		currentIndex	= -1;

		// preferencesModel.onboardingCompleted = true;	// should not showing it again?
		onboardingOverlay.skipped();
	}

	property rect targetRect:
	{
		if (!currentStep || !currentStep.target || currentStep.target.width <= 0 || currentStep.target.height <= 0)
			return Qt.rect(0, 0, 0, 0);

		var t		= currentStep.target;
		var dummy	= t.x + t.y + t.width + t.height + onboardingOverlay.width + onboardingOverlay.height;
		var pt		= t.mapToItem(onboardingOverlay, 0, 0);

		if (isNaN(pt.x) || isNaN(pt.y))
			return Qt.rect(0, 0, 0, 0);

		return Qt.rect(pt.x, pt.y, t.width, t.height);
	}

	Rectangle
	{
		color:		"#000000"
		opacity:	0.8
		x:			0
		y:			0
		width:		parent.width
		height:		Math.max(0, targetRect.y) 
	}

	Rectangle 
	{
		color:		"#000000"
		opacity:	0.8
		x:			0
		y:			Math.max(0, targetRect.y + targetRect.height)
		width:		parent.width
		height:		Math.max(0, parent.height - (targetRect.y + targetRect.height))
	}

	Rectangle
	{
		color:		"#000000"
		opacity:	0.8
		x:			0
		y:			Math.max(0, targetRect.y)
		width:		Math.max(0, targetRect.x); height: Math.max(0, targetRect.height) 
	}

	Rectangle
	{
		color:		"#000000"
		opacity:	0.8
		x:			Math.max(0, targetRect.x + targetRect.width)
		y:			Math.max(0, targetRect.y)
		width:		Math.max(0, parent.width - (targetRect.x + targetRect.width))
		height:		Math.max(0, targetRect.height)
	}

	Rectangle
	{
		x:			targetRect.x
		y:			targetRect.y
		width:		targetRect.width
		height:		targetRect.height
		color:		jaspTheme.blue
		opacity:	0.1

		Behavior on x		{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on y		{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on width	{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on height	{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
	}

	Rectangle
	{
		x:				targetRect.x - 4
		y:				targetRect.y - 4
		width:			Math.max(0, targetRect.width + 8)
		height:			Math.max(0, targetRect.height + 8)
		radius:			4
		color:			"transparent"
		border.width:	2
		border.color:	jaspTheme.blue

		Behavior on x		{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on y		{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on width	{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on height	{ enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
	}

	MouseArea { anchors.fill: parent }

	Rectangle
	{
		id:				callout
		width:			320
		height:			calloutColumn.implicitHeight + 24
		radius:			6
		color:			jaspTheme.white
		border.width:	1
		border.color:	jaspTheme.borderColor

		readonly property string placement: currentStep && currentStep.placement ? currentStep.placement : "bottom"

		x:
		{
			switch (placement)
			{
				case "right":	return Math.min(targetRect.x + targetRect.width + 12, onboardingOverlay.width - width - 8);
				case "left":	return Math.max(8, targetRect.x - width - 12);
				default:		return Math.min(Math.max(targetRect.x, 8), onboardingOverlay.width - width - 8);
			}
		}

		y:
		{
			switch (placement)
			{
				case "left":
				case "right":	return Math.min(Math.max(targetRect.y, 8), onboardingOverlay.height - height - 8);
				case "top":		return Math.max(8, targetRect.y - height - 12);
				default:
					return (targetRect.y + targetRect.height + height + 12 <= onboardingOverlay.height)
						   ? targetRect.y + targetRect.height + 12
						   : Math.max(8, targetRect.y - height - 12);
			}
		}
		

		Behavior on x { enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }
		Behavior on y { enabled: preferencesModel.animationsOn; NumberAnimation { duration: 200 } }

		Column
		{
			id:					calloutColumn
			anchors.fill:		parent
			anchors.margins:	12
			spacing:			8

			Text
			{
				width:			parent.width
				text:			qsTr("Getting Started")
				font.bold:		true
				font.pixelSize:	16
				color:			jaspTheme.textEnabled
				wrapMode:		Text.WordWrap
			}

			Text
			{
				width:			parent.width
				text:			currentStep ? currentStep.title : ""
				font.bold:		true
				font.pixelSize:	14
				color:			jaspTheme.textEnabled
				wrapMode:		Text.WordWrap
			}

			Text
			{
				width:			parent.width
				text:			currentStep ? currentStep.text : ""
				font.pixelSize:	12
				color:			jaspTheme.textEnabled
				wrapMode:		Text.WordWrap
			}

			Text
			{
				text:			(onboardingOverlay.currentIndex + 1) / onboardingOverlay.steps.length
				font.pixelSize:	11
				color:			jaspTheme.textDisabled
			}

			JC.CheckBox
			{
				visible:			onboardingOverlay.lastStep
				text:				qsTr("Don't show it on the next startup")
				checked:			onboardingOverlay.dontShowAgain
				onCheckedChanged:	onboardingOverlay.dontShowAgain = checked
			}

			Row
			{
				width:				parent.width
				spacing:			8
				layoutDirection:	Qt.RightToLeft

				JC.Button
				{
					text:			onboardingOverlay.lastStep ? qsTr("Finish") : qsTr("The Next Step")
					onClicked:		onboardingOverlay.next()
				}

				JC.Button
				{
					text:			qsTr("Skip the Tutorial")
					onClicked:		onboardingOverlay.skip()
				}
			}
		}
	}
}
