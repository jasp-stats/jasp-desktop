import QtQuick
import QtQuick.Controls
import JASP.Widgets
import JASP.Controls


Item 
{
	id: viewRoot
	Keys.onLeftPressed:		resourceMenu.forceActiveFocus();

	default property alias content:		stackEmHere.children
	property alias scrollView:			scrollView
	property Item flickable:			stackEmHere.parent ? stackEmHere.parent.parent : undefined //hacky way to get to the flickable inside ScrollView

	function setMaxImplicitWidth()
	{
		implicitWidth = Qt.binding(function() {
			var m = 0
			for (var i = 0; i < stackEmHere.children.length; i++)
				m = Math.max(stackEmHere.children[i].implicitWidth, m)
			return m + 2 * jaspTheme.generalMenuMargin
		})
	}
	
	Component.onCompleted: setMaxImplicitWidth()

	ScrollView 
	{
		id:						scrollView
		focus:					true
		anchors.fill:			parent
		hoverEnabled:			false
		
		Column
		{
			id:			stackEmHere
			spacing:	jaspTheme.rowSpacing
			width:		viewRoot.width - 2 * jaspTheme.generalMenuMargin
			x:			jaspTheme.generalMenuMargin
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
