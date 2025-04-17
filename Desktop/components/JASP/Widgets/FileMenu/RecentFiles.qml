import QtQuick
import QtQuick.Controls
import QtQuick.Layouts
import JASP.Controls


Item
{
	id:						rect
	focus:					true
	onActiveFocusChanged:	if(activeFocus) recentFilesList.forceActiveFocus()

	MenuHeader
	{
		id:					menuHeader
		headertext:			qsTr("Recent Files")
	}
	
	ScrollMoreIndicator
	{
		anchors
		{
			top:			menuHeader.bottom
			left:			parent.left
			right:			parent.right
			topMargin:		-menuHeader.separatorHalve
		}
		
		upsideDown:	true
		extraSpace:	recentFilesList.contentY
	}
	
	ScrollMoreIndicator
	{
		anchors
		{
			left:			 parent.left
			right:			 parent.right
			bottom:			 parent.bottom
		}
		
		upsideDown:	false
		extraSpace:	recentFilesList.contentHeight - (recentFilesList.contentY + recentFilesList.height)
	}


	FileList
	{
		id:					recentFilesList
		cppModel:			fileMenuModel.recentFiles.listModel
		keyNavigationWraps:	true

		anchors
		{
			top:			menuHeader.bottom
			left:			menuHeader.left
			right:			menuHeader.right
			bottom:			parent.bottom
			bottomMargin:	jaspTheme.generalAnchorMargin
		}
	}
}
