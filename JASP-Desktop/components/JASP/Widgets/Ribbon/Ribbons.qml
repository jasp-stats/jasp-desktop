import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Theme 1.0

Item
{
	id:				jaspRibbons
	objectName:		"jaspRibbon"
	width:			500
	height:			80

	ListView
	{
		z:				0
		model:			ribbonModelFiltered
		orientation:	ListView.Horizontal
		currentIndex:	ribbonModelFiltered.highlightedModuleIndex

		highlightFollowsCurrentItem:	true
		highlightMoveDuration:			20

		anchors
		{
			top:	parent.top
			left:	fadeOutLeft.right
			right:	fadeOutRight.left
			bottom:	parent.bottom
		}


		delegate: Ribbon
		{
			id:				jaspRibbon
			model:			ribbonButtonModel
			separateMe:		index > 0
			highlighted:	ribbonModelFiltered.highlightedModuleIndex === index
		}
	}

	Item
	{
		id:		fadeOutLeft
		width:	height * 0.2
		z:		1
		anchors
		{
			top:	parent.top
			bottom:	parent.bottom
			left:	parent.left
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: Theme.uiBackground	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			-90
		}
	}

	Item
	{
		id:		fadeOutRight
		width:	height * 0.2
		z:		1
		anchors
		{
			top:	parent.top
			bottom:	parent.bottom
			right:	parent.right
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: Theme.uiBackground	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			90
		}
	}
}
