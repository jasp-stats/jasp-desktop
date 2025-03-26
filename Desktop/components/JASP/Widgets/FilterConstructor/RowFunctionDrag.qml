import QtQuick

DragGeneric 
{
	id:					dragMe
	shownChild:			showMe
	property string __debugName: "RowFunctionDrag"

	property alias functionName:			showMe.functionName
	property alias friendlyFunctionName:	showMe.friendlyFunctionName
	property alias droppedItems:			showMe.droppedItems

	property bool acceptsDrops:				true
	dragKeys:								showMe.dragKeys

	function getParameterDropSpot(param)		{ return showMe.getParameterDropSpot(param) }
	
	Connections
	{
		target:		showMe
		enabled:	showMe != undefined
		function onJsonChanged()
		{
			dragMe.jsonChanged();	
		}
	}

	RowFunction
	{
		id:						showMe
		//functionName:			parent.functionName
		//friendlyFunctionName:	parent.friendlyFunctionName

		x:						parent.dragX
		y:						parent.dragY

		isNested:				parent.nested
		acceptsDrops:			parent.acceptsDrops
	}
}
