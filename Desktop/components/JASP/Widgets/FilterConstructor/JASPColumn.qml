import QtQuick
import QtQuick.Controls

Item
{
					id:					jaspColumnRoot
					objectName:			"Column"
	property string __debugName:		"JASPColumn " + columnName
	property string columnName:			"?"
	property string columnIcon:			columnsModel.getColumnIcon(columnTypeUser == -1 ? columnType : columnTypeUser, columnTypeUser != -1 && columnTypeUser != columnType)
	property int	columnType:			columnsModel.getColumnType(columnName)
	property int	columnTypeUser:		-1
	property int	columnTypeHere:		columnTypeUser != -1 ? columnTypeUser : columnType
	property string toolTip:			qsTr("Click icon to change column type") + 
											(columnType == columnTypeUser || columnTypeUser == -1 ? "" : ("\n\n%1:\n\n").arg(qsTr("Transformed to")) + columnsModel.getColumnTransformedToolTip(columnName, columnTypeUser))

	property real	maxSize:			baseFontSize * 10 * preferencesModel.uiScale
					height:				filterConstructor.blockDim
					implicitWidth:		colName.x + colName.width
                    width:				implicitWidth
	property bool	isNumerical:		columnTypeHere == columnTypeScale
	property bool	isOrdinal:			columnTypeHere == columnTypeOrdinal
	property bool	changeTypeAllowed:	true
	property var	dragKeys:			isNumerical ? ["number"]	: isOrdinal ? ["string", "ordered"] : ["string"]
	property string typeString:			isNumerical ? "scale"		: isOrdinal ? "ordinal"				: "nominal"
					
	Connections
	{
		target:								columnsModel
		function onColumnTypeChanged(name)	
		{ 
			if(columnName == name)
			{
				columnType = columnsModel.getColumnType(columnName); 
				filterConstructor.somethingChanged = true;
			}
		}
	}
	
	onColumnTypeHereChanged:		filterConstructor.somethingChanged = true;
	
	

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
		}
		
		MouseArea
		{
			enabled:			changeTypeAllowed
			anchors.fill:		parent
			onClicked:
			{
				var functionCall      = function (index)
				{
					columnTypeUser = columnTypesModel.getType(index)
					customMenu.hide()
				}

				var props = {
					"model":		columnTypesModel,
					"functionCall": functionCall
				};

				customMenu.scrollOri.x	= 0;
				customMenu.scrollOri.y	= 0;

				customMenu.toggle(parent, props, 0, height);

				customMenu.menuScroll.x	= 0;
				customMenu.menuScroll.y	= 0;
				customMenu.menuMinIsMin	= true
			}

			cursorShape:		enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
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
	function returnR()							{ return columnTypeUser == -1 ? columnName : columnName + "." + typeString }
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType": "Column", "columnName": columnName, "columnTypeUser": columnTypeUser }
		return jsonObj
	}
}
