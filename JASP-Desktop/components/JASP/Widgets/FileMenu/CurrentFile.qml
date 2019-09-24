import QtQuick 2.9
import QtQuick.Controls 2.2
import JASP.Theme 1.0

Item
{
	id:						rect
	focus:					true
	onActiveFocusChanged:	if(activeFocus) currentFileList.forceActiveFocus()
		
	Label
	{
		id:					headLabel
		width:				400 * preferencesModel.uiScale
		height:				30 * preferencesModel.uiScale
		text:				"Current data file for synchronization"
		font:				Theme.fontLabel
		color:				Theme.black
		anchors
		{
			top:			parent.top
			left:			parent.left  //Position Recent Files label
			topMargin:		Theme.generalMenuMargin
			leftMargin:		Theme.generalMenuMargin
			rightMargin:	Theme.generalMenuMargin
		}
	}
	
	ToolSeparator
	{
		id:				firstSeparator
		anchors.top:	headLabel.bottom
		width:			rect.width
		orientation:	Qt.Horizontal
	}
		
	Label
	{
		id:					headListLabel
		anchors.top:		firstSeparator.bottom
		anchors.left:		parent.left  //Position Recent Files label
		anchors.leftMargin:	12 * preferencesModel.uiScale
		height:				20 * preferencesModel.uiScale
		text:				fileMenuModel.currentFile.getHeaderText()	//For shorcut key
		font:				Theme.font
	}
	 
	FileList
	{
		id:				currentFileList
		cppModel:		fileMenuModel.currentFile.listModel

		anchors
		{
			top:			headListLabel.bottom
			bottom:			parent.bottom
			left:			parent.left
			right:			parent.right
			leftMargin:		12  * preferencesModel.uiScale
			topMargin:		Theme.generalAnchorMargin
			bottomMargin:	Theme.generalAnchorMargin
			rightMargin:	Theme.generalAnchorMargin
		}
	}
}
