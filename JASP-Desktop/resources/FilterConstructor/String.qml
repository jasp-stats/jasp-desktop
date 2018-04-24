import QtQuick 2.0

Item
{
	objectName: "String"
	property string __debugName: "String " + text
	property string text: ""

	height:	filterConstructor.blockDim
	width:	stringObj.contentWidth

	id: stringRoot


	TextInput
	{
		id: stringObj
		text: parent.text

		anchors.horizontalCenter:	parent.horizontalCenter
		anchors.verticalCenter:		parent.verticalCenter

		font.pixelSize: filterConstructor.fontPixelSize


		onAccepted:	if(text === "") destroyMe()
		onActiveFocusChanged:	if(!activeFocus && text === "") destroyMe()


		function destroyMe()
		{
			if(stringRoot.parent.objectName === "DropSpot")
				stringRoot.parent.releaseHere(scriptColumn)

			stringRoot.parent.destroy()
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
