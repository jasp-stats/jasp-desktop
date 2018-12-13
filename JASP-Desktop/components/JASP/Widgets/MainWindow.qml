import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0


Window
{
	title:		"JASP"
	visible:	true
	width:		1900
	height:		600

	Ribbon
	{
		id: ribbon
		anchors
		{
			top:	parent.top
			left:	parent.left
			right:	parent.right
		}
	}

	FileMenu
	{
		id: filemenu

		width: 600

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
}
