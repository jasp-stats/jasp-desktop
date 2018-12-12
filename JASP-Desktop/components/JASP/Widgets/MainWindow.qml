import QtQuick 2.11
import QtQuick.Window 2.11
import JASP.Widgets 1.0


Window
{
	title:		"JASP"
	visible:	true
	width:		900
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

	MainPage
	{
		anchors
		{
			top:	ribbon.bottom
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}
	}
}
