import QtQuick 2.0

DragGeneric 
{
	property string columnName:		"?"
	property int    columnTypeUser:	-1

					shownChild:		showMe
					dragKeys:		showMe.dragKeys
    property bool	acceptsDrops:	true
    property alias	maxSize:		showMe.maxSize
					
					toolTipText:	acceptsDrops ? showMe.toolTip : ""

	JASPColumn
	{
		id:						showMe
		columnName:				parent.columnName
		columnTypeUser:			parent.columnTypeUser
		changeTypeAllowed:		parent.acceptsDrops

		x:						parent.dragX
		y:						parent.dragY
	}
}
