import QtQuick 2.0
import QtQuick.Controls 2.2

//! [0]
DropArea {
	id: trashCan
	objectName: "DropTrash"
	property string __debugName: "DropTrash"

	keys: [ "number", "boolean", "string", "variable" ]

	onDropped: if(drop.drag.source !== null) drop.drag.source.destroy()
	property real aspect: 1.7
	width: height / aspect
	property real iconPadding: 0.9

	property bool somethingHovers: false

	onEntered: somethingHovers = true
	onExited: somethingHovers = false


	Image
	{
		id:	trashIcon
		anchors.centerIn: parent

		property real sizer: (trashCan.height < trashCan.width * aspect ? trashCan.height : trashCan.width * aspect)

		height: sizer * parent.iconPadding
		width: (sizer / aspect) * parent.iconPadding

		source: somethingHovers ? "qrc:/icons/trashcan_open.svg" : "qrc:/icons/trashcan.svg"

		smooth: true
	}

	MouseArea
	{
		anchors.fill: parent

		onDoubleClicked: parent.destroyAll()

		ToolTip.delay: 500
		//ToolTip.timeout: 1000
		ToolTip.visible: containsMouse
		ToolTip.text: "Dump unwanted formula snippets here; doubleclick to clean up the entire slate"

		hoverEnabled: true

	}

	function destroyAll()
	{
		for(var i=scriptColumn.children.length-1; i >= 0; i--)
			scriptColumn.children[i].destroy()
	}


}
//! [0]
