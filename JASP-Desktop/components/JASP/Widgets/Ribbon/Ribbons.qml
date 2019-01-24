import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Theme 1.0

Item
{
	id:				jaspRibbons
	objectName:		"jaspRibbon"
	width:			500
	height:			Theme.ribbonButtonHeight

	ListView
	{
		z:				0
		model:			ribbonModelFiltered
		orientation:	ListView.Horizontal
		currentIndex:	ribbonModelFiltered.highlightedModuleIndex
		height:			parent.height

		highlightFollowsCurrentItem:	true
		highlightMoveDuration:			20

		anchors
		{
			left:			fadeOutLeft.right
			right:			fadeOutRight.left
			verticalCenter:	parent.verticalCenter
		}


		delegate: Ribbon
		{
			id:				jaspRibbon
			model:			ribbonButtonModel
			height:			jaspRibbons.height
			separateMe:		index > 0
			highlighted:	ribbonModelFiltered.highlightedModuleIndex === index
			module:			moduleName
			moduleTitle:	ribbonButtonModel.title
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
