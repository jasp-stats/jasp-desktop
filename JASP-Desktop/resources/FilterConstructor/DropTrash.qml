import QtQuick 2.9
import QtQuick.Controls 2.2

//! [0]
DropArea {
	id: trashCan
	objectName: "DropTrash"
	property string __debugName: "DropTrash"

	keys: [ "all" ]

	onDropped: if(drop.drag.source !== null) drop.drag.source.destroy()
	property real aspect: 1.5
	width: height / aspect
	property real iconPadding: 0.7

	property bool somethingHovers: false

	onEntered: somethingHovers = true
	onExited: somethingHovers = false

	Image
	{
		id:	trashIcon
		anchors.centerIn: parent

		property real sizer: (trashCan.height < trashCan.width * aspect ? trashCan.height : trashCan.width * aspect)

		height: (sizer) * parent.iconPadding
		width: (sizer) * parent.iconPadding

		// Flaticon basic license
		// Icon made by [Smashicons] (https://www.flaticon.com/authors/smashicons) from www.flaticon.com
		source: somethingHovers ? "qrc:/icons/trashcan_open.svg" : "qrc:/icons/trashcan.svg"
		sourceSize.width: 160 / aspect
		sourceSize.height: 160
		//mipmap: true

		smooth: true
	}

	MouseArea
	{
		anchors.fill: parent

		onDoubleClicked: parent.destroyAll()

		ToolTip.delay: 500
		//ToolTip.timeout: 1000
		ToolTip.visible: containsMouse
		ToolTip.text: "Dump unwanted formula snippets here.<br>Doubleclick to clean up the entire slate."

		hoverEnabled: true

		cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
	}

	function destroyAll(apply)
	{
		for(var i=scriptColumn.children.length-1; i >= 0; i--)
			scriptColumn.children[i].destroy()

		scriptColumn.children = ""

		if(apply === undefined || apply)
			filterConstructor.checkAndApplyFilter()
	}


}
//! [0]
