import QtQuick 2.9
import QtQuick.Controls 2.2


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
		text:				qsTr("Current data file for synchronization")
		font:				jaspTheme.fontLabel
		color:				jaspTheme.black
		anchors
		{
			top:			parent.top
			left:			parent.left  //Position Recent Files label
			topMargin:		jaspTheme.generalMenuMargin
			leftMargin:		jaspTheme.generalMenuMargin
			rightMargin:	jaspTheme.generalMenuMargin
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
		font:				jaspTheme.font
	}
	 
	FileList
	{
		id:					currentFileList
		cppModel:			fileMenuModel.currentFile.listModel
		keyNavigationWraps:	true

		anchors
		{
			top:			headListLabel.bottom
			bottom:			parent.bottom
			left:			parent.left
			right:			parent.right
			leftMargin:		12  * preferencesModel.uiScale
			topMargin:		jaspTheme.generalAnchorMargin
			bottomMargin:	jaspTheme.generalAnchorMargin
			rightMargin:	jaspTheme.generalAnchorMargin
		}
	}
}
