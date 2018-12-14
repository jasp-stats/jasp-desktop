import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0


Window
{
	id:			mainWindowRoot
	title:		mainWindow.windowTitle
	visible:	true
	width:		1800
	height:		900

	RibbonBar
	{
		id:		ribbon

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

		anchors
		{
			top:	ribbon.bottom
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
	}

	MessageBox
	{
		id:	msgBox
		z:	2

		Connections
		{
			target:			mainWindow
			onShowWarning:	msgBox.showWarning(title, message)
		}

	}
}
