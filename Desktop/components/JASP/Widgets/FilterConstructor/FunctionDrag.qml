import QtQuick 2.0

DragGeneric {
	shownChild: showMe
	property string __debugName: "FunctionDrag"

	property string functionName: "sum"
	property string friendlyFunctionName: functionName
	property var parameterNames: functionName != "!" ? ["y"] : []
	property var parameterDropKeys: ["???"]

	property bool acceptsDrops: true
	dragKeys: showMe.dragKeys

	function getParameterDropSpot(param)		{ return showMe.getParameterDropSpot(param) }

	Function
	{
		id:						showMe
		functionName:			parent.functionName
		friendlyFunctionName:	parent.friendlyFunctionName
		parameterNames:			parent.parameterNames
		parameterDropKeys:		parent.parameterDropKeys //parent.convertedParameterDropKeys //parameterDropKeys.indexOf("any") >= 0 ? [filterConstructor.allKeys] : parent.parameterDropKeys

		x:						parent.dragX
		y:						parent.dragY

		isNested:				parent.nested
		acceptsDrops:			parent.acceptsDrops
	}
}
