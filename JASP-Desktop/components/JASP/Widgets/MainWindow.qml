import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0
import JASP.Theme 1.0

Window
{
	id:			mainWindowRoot
	title:		mainWindow.windowTitle
	visible:	true
	width:		Theme.formWidth * 2.2
	height:		768

	minimumWidth:	Theme.formWidth + Theme.minPanelWidth
	minimumHeight:	480

	RibbonBar
	{
		id:		ribbon
		z:		1

		anchors
		{
			top:	parent.top
			left:	parent.left
			right:	parent.right
		}
	}

	FileMenu
	{
		id:			filemenu
		z:			2

		anchors
		{
			top:	ribbon.bottom
			left:	parent.left
			bottom:	parent.bottom
		}
	}

	MainPage
	{
		id: mainpage
		z:	0

		anchors
		{
			top:	ribbon.bottom
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
	}

	MouseArea
	{
		visible:		fileMenuModel.visible || modulesMenu.opened
		z:				5
		hoverEnabled:	true

		onContainsMouseChanged: if(containsMouse) ribbonModel.highlightedModuleIndex = -1

		anchors
		{
			top:	ribbon.bottom
			left:	filemenu.right
			right:	modulesMenu.left
			bottom:	parent.bottom
		}

		onClicked:
		{
			fileMenuModel.visible		= false
			modulesMenu.opened			= false

			mouse.accepted = false
		}
	}

	ModulesMenu
	{
		id:			modulesMenu
		z:			1

		anchors
		{
			top:	ribbon.bottom
			right:	parent.right
			bottom:	parent.bottom
		}
	}

	/*MessageBox
	{
		id:	msgBox
		z:	2

		Connections
		{
			target:			mainWindow
			onShowWarning:	msgBox.showWarning(title, message)
		}

	}*/
}
