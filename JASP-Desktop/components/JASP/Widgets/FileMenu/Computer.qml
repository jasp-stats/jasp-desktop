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
		id:headLabel
		
		width:implicitWidth
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Recent Folders"
		verticalAlignment: Text.AlignVCenter
		font.family: "SansSerif"
		font.pixelSize: 18
		color: "black"
	}
	
	Button {
		id: browseButton
		
		background: Rectangle {
			anchors.fill: parent
			gradient: Gradient {
				GradientStop { position: 0 ; color:  "#e5e5e5" }
				GradientStop { position: 1 ; color:  "white" }
			}
			border.color: "gray"
			border.width: 1
		}
					
		text: "Browse"
		width: 80
		height: 20
		anchors.left: parent.left
		anchors.top: headLabel.bottom
		anchors.leftMargin: 10
		anchors.topMargin: 10
		
		onClicked: {
			backstagecomputer.browseMostRecent();		
		}
	}
	
	ToolSeparator
	{
		id: firstSeparator
		
		anchors.top: browseButton.bottom
		anchors.topMargin: 10
		width: rect.width
		orientation: Qt.Horizontal
	}
	
	ComputerList {
		id: computerList
		
		anchors.top: firstSeparator.bottom
		anchors.bottom: parent.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary items
		anchors.topMargin: 6 
		anchors.bottomMargin: 6
		
	}
	
}
