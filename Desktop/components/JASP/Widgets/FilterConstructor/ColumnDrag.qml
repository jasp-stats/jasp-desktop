import QtQuick

DragGeneric 
{
	property alias columnName:		showMe.columnName
	property alias  columnTypeUser:	showMe.columnTypeUser
	property alias  columnTypeDrop:	showMe.columnTypeDrop
					shownChild:		showMe
					dragKeys:		showMe.dragKeys
    property bool	acceptsDrops:	true
    property alias	maxSize:		showMe.maxSize
					
					toolTipText:	acceptsDrops ? showMe.toolTip : ""

	JASPColumn
	{
		id:						showMe
		changeTypeAllowed:		parent.acceptsDrops

		x:						parent.dragX
		y:						parent.dragY
	}
}
