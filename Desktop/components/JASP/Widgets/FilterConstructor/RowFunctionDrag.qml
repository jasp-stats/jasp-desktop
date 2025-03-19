import QtQuick

DragGeneric {
	shownChild: showMe
	property string __debugName: "RowFunctionDrag"

	property string functionName: "rowSum"
	property string friendlyFunctionName: functionName

	property bool acceptsDrops: true
	dragKeys: showMe.dragKeys

	function getParameterDropSpot(param)		{ return showMe.getParameterDropSpot(param) }

	RowFunction
	{
		id:						showMe
		functionName:			parent.functionName
		friendlyFunctionName:	parent.friendlyFunctionName

		x:						parent.dragX
		y:						parent.dragY

		isNested:				parent.nested
		acceptsDrops:			parent.acceptsDrops
	}
}
