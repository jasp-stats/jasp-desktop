import QtQuick 2.0

Item
{
	objectName: "String"
	property string __debugName: "String " + text
	property alias text: stringObj.text

	height:	filterConstructor.blockDim
	width:	stringObj.contentWidth

	id: stringRoot


	TextInput
	{
		id: stringObj
		text: ""

		anchors.horizontalCenter:	parent.horizontalCenter
		anchors.verticalCenter:		parent.verticalCenter

		font.pixelSize: filterConstructor.fontPixelSize
		color:			jaspTheme.textEnabled

		onAccepted: focus = false
		onActiveFocusChanged:	if(!activeFocus) { if(text === "") destroyMe(); else filterConstructor.somethingChanged = true; }

		function destroyMe()
		{
			if(stringRoot.parent.objectName === "DropSpot")
				stringRoot.parent.releaseHere(scriptColumn)

			stringRoot.parent.destroy()
			filterConstructor.somethingChanged = true
		}
	}

	function shouldDrag(mouseX, mouseY)			{ return false }
	function returnEmptyRightMostDropSpot()		{ return null }
	function returnFilledRightMostDropSpot()	{ return null }
	function returnR()							{ return "'" + text + "'"; }
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType":"String", "text":text }
		return jsonObj
	}
}
