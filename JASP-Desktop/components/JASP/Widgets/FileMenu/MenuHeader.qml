import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0


Item {

	property bool toolseparator: true
	property string headertext: ""

	height:30
	anchors.top: parent.top
	anchors.left: parent.left
	anchors.right: parent.right
	anchors.leftMargin: Theme.generalMenuMargin
	anchors.rightMargin: Theme.generalMenuMargin
	anchors.topMargin: Theme.generalMenuMargin

	Label
	{
		id:headLabel
		text: headertext

		anchors.fill: parent
		font.family: "SansSerif"
		font.pixelSize: 18
		color: Theme.black
	}

	ToolSeparator
	{
		id: firstSeparator

		anchors.top: headLabel.bottom
		width: parent.width

		orientation: Qt.Horizontal
		visible: toolseparator
	}

}
