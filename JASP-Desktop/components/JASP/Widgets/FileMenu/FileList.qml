import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0
import JASP.Theme 1.0

ListView
{
	property var cppModel: undefined


	id:						listView
	maximumFlickVelocity:	Theme.maximumFlickVelocity
	clip:					true

	spacing:				10
	model:					cppModel

	delegate:	ListItem
	{
		width:		listView.width -  (rightscrollbar.width > 0 ? rightscrollbar.width + listView.spacing : 0)
		cppModel:	listView.cppModel
	}

	JASPScrollBar {
		id:				rightscrollbar
		flickable:		parent
	}


}
