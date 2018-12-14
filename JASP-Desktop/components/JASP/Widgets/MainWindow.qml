import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0
import "qrc:/components/JASP/Widgets/FileMenu/"

Window
{
	id:			mainWindowRoot
	title:		mainWindow.windowTitle
	visible:	true
	width:		1800
	height:		900

	Item
	{
		id:		ribbon
		height: ribbonMenu.height
		anchors
		{
			top:	parent.top
			left:	parent.left
			right:	parent.right
		}

		FilterButton
		{
			id:		fileMenuOpenButton
			text:	"File"
			color:	"blue"
			width:	height

			onClicked: fileMenuModel.visible = !fileMenuModel.visible

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

		FilterButton
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

			onClicked: msgBox.showWarning("PLUS", "I am a horrible warning!")
		}
	}

	FileMenu //This should be done differently actually, but now here for testing
	{
		id:			filemenu

		width:		visible ? 600 : 0
		visible:	fileMenuModel.visible

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
			left:	filemenu.right
			right:	parent.right
			bottom:	parent.bottom
		}
	}

	MessageBox
	{
		id:	msgBox


		Connections
		{
			target:			mainWindow
			onShowWarning:	msgBox.showWarning(title, message)
		}

	}
}
