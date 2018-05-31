import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

Rectangle
{
	id:rect
	objectName: "rect"
	color: "#ececec"
	
	Label
	{
		id:headlabel
		width:400
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Datalibrary label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Data Library"
		font.family: "SansSerif"
		font.pixelSize: 18
		color: "black"
	}
	
	DataLibraryBreadCrumbs
	{
		id:datalibrarybreadcrumbs
		width: rect.width
		height:40
		anchors.top: headlabel.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary breadcrumbs
	}
	
	ToolSeparator
	{
		id: secondseparator
		anchors.top: datalibrarybreadcrumbs.bottom
		width: rect.width
		orientation: Qt.Horizontal
	}
	
	DataLibraryList
	{
		id: datalibrarylist
		anchors.top: secondseparator.bottom
		anchors.bottom: parent.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary items
		anchors.topMargin: 6 
		anchors.bottomMargin: 6
	}
}

