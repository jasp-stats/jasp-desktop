import QtQuick
import QtQuick.Controls
import JASP.Widgets
import JASP.Controls


Item 
{
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();
	
	default property alias content:		stackEmHere.data
	property alias scrollView:			scrollView
	property Item flickable:			stackEmHere.parent ? stackEmHere.parent.parent : undefined //hacky way to get to the flickable inside ScrollView
	
	ScrollView 
	{
		id:						scrollView
		focus:					true
		anchors.fill:			parent
		hoverEnabled:			false
		
		Column
		{
			id:		stackEmHere
		}
	}
	
	ScrollMoreIndicator
	{
		anchors
		{
			top:		parent.top
			left:		parent.left
			right:		parent.right
		}
		
		upsideDown:	true
		extraSpace:	flickable.contentY
	}
	
	ScrollMoreIndicator
	{
		anchors
		{
			left:		parent.left
			right:		parent.right
			bottom:		parent.bottom
		}
		
		upsideDown:	false
		extraSpace:	flickable.contentHeight - (flickable.contentY + flickable.height)
	}
}
