import QtQuick 2.0

Item
{
	objectName: "Number"
	property string __debugName: "Number " + value
	property real value: 0

	height:	filterConstructor.blockDim
	width:	nummer.contentWidth

	id: numberRoot


	TextInput
	{
		id:							nummer
		text:						value

		anchors.horizontalCenter:	parent.horizontalCenter
		anchors.verticalCenter:		parent.verticalCenter

		font.pixelSize:				filterConstructor.fontPixelSize
		color:						jaspTheme.textEnabled


		onAccepted: focus = false
		onActiveFocusChanged:	if(!activeFocus) { if(textIsWrong()) destroyMe(); else { text = parseFloat(text); value = text; filterConstructor.somethingChanged = true } }

		function textIsWrong() { return (text === "" || isNaN(parseFloat(text))) }

		function destroyMe()
		{
			if(numberRoot.parent.objectName === "DropSpot")
				numberRoot.parent.releaseHere(scriptColumn)

			numberRoot.parent.destroy()
			filterConstructor.somethingChanged = true
		}
	}

	function shouldDrag(mouseX, mouseY)			{ return false }
	function returnEmptyRightMostDropSpot()		{ return null }
	function returnFilledRightMostDropSpot()	{ return null }
	function returnR()							{ return value; }
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType":"Number", "value":value }
		return jsonObj
	}
}
