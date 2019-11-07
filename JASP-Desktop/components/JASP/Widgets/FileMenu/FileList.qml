import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0


ListView
{
	property var	cppModel:		undefined
	property var	breadCrumbs:	null

	id:						listView
	maximumFlickVelocity:	jaspTheme.maximumFlickVelocity
	clip:					true
	boundsBehavior:			Flickable.StopAtBounds

	spacing:				10
	model:					cppModel


	Keys.onLeftPressed:
		if(breadCrumbs !== null)
		{
			event.accepted = breadCrumbs.count > 1;

			if(event.accepted)
				breadCrumbs.crumbButtonClicked(breadCrumbs.count - 2)
		}
		else
			event.accepted = false;

	Connections
	{
		target:				listView.model
		onModelReset:		listView.currentIndex = 0;
	}

	delegate:	ListItem
	{
		width:					listView.width -  (rightscrollbar.width > 0 ? rightscrollbar.width + listView.spacing : 0)
		cppModel:				listView.cppModel
		hasBreadCrumbs:			listView.breadCrumbs !== null
		onAllHoveredChanged:	if(allHovered) { listView.currentIndex = index; forceActiveFocus(); }
	}

	JASPScrollBar
	{
		id:				rightscrollbar
		flickable:		parent
		bigBar:			true
	}
}
