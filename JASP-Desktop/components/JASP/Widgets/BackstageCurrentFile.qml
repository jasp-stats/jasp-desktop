import QtQuick 2.9
import QtQuick.Controls 2.2
//import QtQuick.Layouts 1.3

Rectangle
{
	id:rect
	objectName: "rect"
	color: "#ececec"
		
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
		color: "black"
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
		text: backstageCurrentFile.getHeaderText()	//For shorcut key		
	}
	 
	CurrentFileList {
		id: currentFileList
		anchors.top: headListLabel.bottom
		anchors.bottom: parent.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary items
		anchors.topMargin: 6 
		anchors.bottomMargin: 6
	}
}
