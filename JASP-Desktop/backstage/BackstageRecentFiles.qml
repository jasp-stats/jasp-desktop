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
		width:400
		height:30
		anchors.top: parent.top
		anchors.left: parent.left  //Position Recent Files label
		anchors.leftMargin: 12
		anchors.topMargin: 12
		text: "Recent Files"
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

	RecentFilesList {
		id: recentFilesList

		anchors.top: firstSeparator.bottom
		anchors.bottom: parent.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.leftMargin: 12  //Position datalibrary items
		anchors.topMargin: 6
		anchors.bottomMargin: 6

	}
}
