import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0
import JASP.Theme 1.0

FocusScope
{
	property alias  cppModel: listView.cppModel
	property alias  hasBreadCrumbs: listView.hasBreadCrumbs

	ListView
	{
		property var cppModel: undefined
		property bool hasBreadCrumbs: false

		id:						listView
		maximumFlickVelocity:	Theme.maximumFlickVelocity
		clip:					true
		anchors.fill: parent

		spacing:				10
		model:					cppModel

		delegate:	ListItem
		{
			width:		listView.width -  (rightscrollbar.width > 0 ? rightscrollbar.width + listView.spacing : 0)
			cppModel:	listView.cppModel
			hasBreadCrumbs: listView.hasBreadCrumbs
		}

		JASPScrollBar {
			id:				rightscrollbar
			flickable:		parent
		}
	}
}
