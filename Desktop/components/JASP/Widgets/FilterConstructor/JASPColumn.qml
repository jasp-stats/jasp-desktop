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
					implicitWidth:	colName.x + colName.width
                    width:          implicitWidth
	property bool	isNumerical:	filterConstructor.forceColumnInputs === "scale"		|| (filterConstructor.forceColumnInputs === "" && columnIcon.indexOf("scale")		>= 0 )
	property bool	isOrdinal:		filterConstructor.forceColumnInputs === "ordinal"	|| (filterConstructor.forceColumnInputs === "" && columnIcon.indexOf("ordinal")		>= 0 )
	property var	dragKeys:		isNumerical ? ["number"] : isOrdinal ? ["string", "ordered"] : ["string"]

	Image
	{
		id:				colIcon
		source:			filterConstructor.forceColumnInputs === "" ? columnIcon : computedColumnsInterface.computeColumnIconSource
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
		font:			colName.font
		text:			colName.text
	}

	Text
	{
		id:				colName
		anchors
		{
			verticalCenter:	parent.vertivalCenter
			left:			colIcon.right
		}

        width:          Math.min(columnNameMeasure.width + 10 + leftPadding, jaspColumnRoot.maxSize - (colIcon.width + 2*colIcon.anchors.margins))
		font.pixelSize: baseFontSize * preferencesModel.uiScale
		font.family:	jaspTheme.font.family
		color:			jaspTheme.textEnabled
		leftPadding:	4 * preferencesModel.uiScale

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
