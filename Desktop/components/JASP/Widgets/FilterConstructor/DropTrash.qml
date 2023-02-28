import QtQuick			2.9
import QtQuick.Controls	2.2

DropArea
{
	id:							trashCan
	objectName:					"DropTrash"
	keys:						[ "all" ]
	onDropped:					if(drop.drag.source !== null) drop.drag.source.destroy()
	width:						height / aspect

	property string __debugName:		"DropTrash"
	property real	aspect:				1.6
	property real	iconPadding:		0.9
	property alias	somethingHovers:	trashMouse.containsMouse

	Image
	{
		id:	trashIcon
		property real sizer: (trashCan.height < trashCan.width * aspect ? trashCan.height : trashCan.width * aspect)

		anchors.centerIn:	parent
		height:				sizer * parent.iconPadding
		width:				(sizer / aspect) * parent.iconPadding
		source:				somethingHovers ? jaspTheme.iconPath + "/trashcan_open.png" : jaspTheme.iconPath + "/trashcan.png"
		sourceSize.width:	160 / aspect
		sourceSize.height:	160
		smooth:				true
	}

	MouseArea
	{
		id:					trashMouse
		anchors.fill:		parent
		onDoubleClicked:	parent.destroyAll()
		ToolTip.delay:		500
		ToolTip.visible:	containsMouse
		ToolTip.text:		qsTr("Dump unwanted snippets here; double-click to erase the entire slate")
		hoverEnabled:		true
		cursorShape:		Qt.PointingHandCursor
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
