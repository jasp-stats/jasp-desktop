import QtQuick 2.9

Item
{
	id: jaspColumnRoot
	objectName: "Column"
	property string __debugName: "JASPColumn " + columnName
	property string columnName: "?"
	property string columnIcon: ""

	property real maxSize: 200
	property real colScaler: 0.8
	height:	filterConstructor.blockDim * colScaler
	width:	colIcon.width + colName.width
	property bool isNumerical: columnIcon.indexOf("scale") >= 0
		//columnIcon.indexOf("scale") >= 0 || columnIcon.indexOf("ordinal") >= 0

	property var dragKeys: isNumerical ? ["number"] : ["string"]

	Image
	{
		id: colIcon
		source: columnIcon
		width: height
		sourceSize.width: width
		sourceSize.height: height

		anchors.top: parent.top
		anchors.left: parent.left
		anchors.bottom: parent.bottom

		anchors.margins: 4
	}

	TextMetrics { id: columnNameMeasure; text: columnName}

	Text
	{
		id:colName
		anchors.top: parent.top
		anchors.left: colIcon.right
		anchors.bottom: parent.bottom
		width: Math.min(columnNameMeasure.width + 10, jaspColumnRoot.maxSize)

		font.pixelSize: filterConstructor.fontPixelSize * colScaler

		leftPadding: 2

		text: columnName
		elide: Text.ElideMiddle

	}

	function shouldDrag(mouseX, mouseY)			{ return true }
	function returnEmptyRightMostDropSpot()		{ return null }
	function returnFilledRightMostDropSpot()	{ return null }
	function returnR()							{ return columnName	}
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType":"Column", "columnName":columnName, "columnIcon":columnIcon }
		return jsonObj
	}
}
