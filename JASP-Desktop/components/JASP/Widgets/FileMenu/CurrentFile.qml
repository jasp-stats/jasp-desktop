import QtQuick 2.9
import QtQuick.Controls 2.2
import JASP.Theme 1.0

Item
{
	id:rect
		
	Label
	{
		id:headLabel
		width:400
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Current data file for sycnchronization"
		font.family: "SansSerif"
		font.pixelSize: 18
		color: Theme.black
	}
	
	ToolSeparator
	{
		id: firstSeparator
		anchors.top: headLabel.bottom
		width: rect.width
		orientation: Qt.Horizontal
	}
		
	Label {
		id: headListLabel
		anchors.top:firstSeparator.bottom 
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12		
		height: 20
		text: fileMenuModel.currentFile.getHeaderText()	//For shorcut key
	}
	 
	FileList {
		id:			currentFileList
		cppModel:	fileMenuModel.currentFile.listModel
		hasBreadCrumbs: false

		anchors
		{
			top:			headListLabel.bottom
			bottom:			parent.bottom
			left:			parent.left
			right:			parent.right
			leftMargin:		12  //Position datalibrary items
			topMargin:		Theme.generalAnchorMargin
			bottomMargin:	Theme.generalAnchorMargin
			rightMargin:	Theme.generalAnchorMargin
		}
	}
}
