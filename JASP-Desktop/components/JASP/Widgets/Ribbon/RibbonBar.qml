import QtQuick 2.11
import JASP.Widgets 1.0
import JASP.Theme 1.0

FocusScope
{
	id:		ribbonBar
	height: ribbonMenu.height
	y:		ribbonMenu.border.width


	HamburgerButton
	{
		id:		fileMenuOpenButton

		width:	height

		anchors
		{
			top:	parent.top
			left:	parent.left
			bottom:	parent.bottom
		}
	}

	Ribbon
	{
		id: ribbonMenu

		anchors
		{
			top:	parent.top
			right:	modulesPlusButton.left
			left:	fileMenuOpenButton.right
		}

	}

	RectangularButton
	{
		id:			modulesPlusButton
		iconSource: "qrc:/icons/addition-sign.svg"
		width:		height

		anchors
		{
			top:	parent.top
			right:	parent.right
			bottom:	parent.bottom
		}

		//onClicked:
	}

	Rectangle
	{
		id:		shadow
		y:		ribbonMenu.height
		z:		1
		height:	Theme.shadowRadius

		anchors {
			left:	parent.left
			right:	parent.right
		}

		gradient:	Gradient {
			GradientStop { position: 0.0; color: Theme.shadow }
			GradientStop { position: 1.0; color: "transparent" } }

		Rectangle // a line underneath ribbonbar, just like in filemenu
		{
			z:		3
			height:	1
			color:	Theme.uiBorder

			anchors
			{
				top: parent.top
				left: parent.left
				right: parent.right
			}
		}

	}
}
