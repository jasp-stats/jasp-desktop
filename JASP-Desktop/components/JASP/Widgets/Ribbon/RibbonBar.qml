import QtQuick 2.11
import JASP.Widgets 1.0
import JASP.Theme 1.0

FocusScope
{
	id:		ribbonBar
	height: ribbonMenu.height
	y:		ribbonMenu.border.width

	Rectangle
	{
		color:			Theme.uiBackground
		anchors.fill:	parent
		z:				-1
	}


	MenuArrowButton
	{
		id:			fileMenuOpenButton
		width:		height
		showArrow:	fileMenuModel.visible
		onClicked:	fileMenuModel.visible = !fileMenuModel.visible
		toolTip:	showArrow ? "Close Menu" : "Open Menu"
		hamburger:	true
		z:			2

		anchors
		{
			top:	parent.top
			left:	parent.left
			bottom:	parent.bottom
		}

	}

	Ribbons
	{
		id: ribbonMenu
		z:	0

		anchors
		{
			top:	parent.top
			right:	modulesPlusButton.left
			left:	fileMenuOpenButton.right
		}

	}

	MenuArrowButton
	{
		id:			modulesPlusButton
		width:		height
		hamburger:	false
		onClicked:	modulesMenu.opened = !modulesMenu.opened
		showArrow:	modulesMenu.opened
		z:			2

		anchors
		{
			top:	parent.top
			right:	parent.right
			bottom:	parent.bottom
		}


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
