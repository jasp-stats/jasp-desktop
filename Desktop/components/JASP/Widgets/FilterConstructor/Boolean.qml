import QtQuick
import JASP.Controls

Item
{
	objectName: "Boolean"
	property string __debugName: "Boolean " + value
	property alias value: waarheid.checked

	height:	filterConstructor.blockDim
	width:	waarheid.implicitWidth

	id: boolRoot


	CheckBox
	{
		id:							waarheid

		anchors.horizontalCenter:	parent.horizontalCenter
		anchors.verticalCenter:		parent.verticalCenter

		//font.family:				jaspTheme.font.family
		//font.pixelSize:				filterConstructor.fontPixelSize
		//color:						jaspTheme.textEnabled


		onCheckedChanged:		filterConstructor.somethingChanged = true 
	}

	function shouldDrag(mouseX, mouseY)			{ return false }
	function returnEmptyRightMostDropSpot()		{ return null }
	function returnFilledRightMostDropSpot()	{ return null }
	function returnR()							{ return value ? "TRUE" : "FALSE"; }
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType":"Boolean", "value": value ? "TRUE" : "FALSE" }
		return jsonObj
	}
}
