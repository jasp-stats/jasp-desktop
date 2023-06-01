import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3


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
			topMargin:		2 * jaspTheme.generalMenuMargin
			bottomMargin:	jaspTheme.generalAnchorMargin
		}
	}
}
