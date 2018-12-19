import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0
import JASP.Theme 1.0

Window
{
	id:			mainWindowRoot
	title:		mainWindow.windowTitle
	visible:	true
	width:		1800
	height:		900

	minimumWidth:	640
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
