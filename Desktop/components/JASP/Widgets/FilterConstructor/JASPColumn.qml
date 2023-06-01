import QtQuick 2.9

Item
{
					id:				jaspColumnRoot
					objectName:		"Column"
	property string __debugName:	"JASPColumn " + columnName
	property string columnName:		"?"
	property string columnIcon:		""

	property real	maxSize:		baseFontSize * 10 * preferencesModel.uiScale
					height:			filterConstructor.blockDim
					width:			colIcon.width + colName.width
	property bool	isNumerical:	columnIcon.indexOf("scale") >= 0
	property bool	isOrdinal:		columnIcon.indexOf("ordinal") >= 0


	property var	dragKeys:		isOrdinal ? ["string", "ordered"] : isNumerical ? ["number"] : ["string"]

	Image
	{
		id:				colIcon
		source:			columnIcon
		width:			height
		sourceSize
		{
			width:		width * 2
			height:		height * 2
		}
		anchors
		{
			top:		parent.top
			left:		parent.left
			bottom:		parent.bottom
			margins:	4 * preferencesModel.uiScale
		}
	}

	TextMetrics
	{
		id:				columnNameMeasure
		font.pixelSize:	colName.font.pixelSize
		text:			colName.text
	}

	Text
	{
		id:				colName
		anchors
		{
			top:		parent.top
			left:		colIcon.right
			bottom:		parent.bottom
		}

		width:			Math.min(columnNameMeasure.width + 10, jaspColumnRoot.maxSize)
		font.pixelSize: baseFontSize * preferencesModel.uiScale
		color:			jaspTheme.textEnabled
		leftPadding:	2

		text:			columnName
		elide:			Text.ElideMiddle

	}

	function shouldDrag(mouseX, mouseY)			{ return true }
	function returnEmptyRightMostDropSpot()		{ return null }
	function returnFilledRightMostDropSpot()	{ return null }
	function returnR()							{ return columnName	}
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType": "Column", "columnName": columnName, "columnIcon": columnIcon }
		return jsonObj
	}
}
