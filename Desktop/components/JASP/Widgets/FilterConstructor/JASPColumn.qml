import QtQuick
import QtQuick.Controls

Item
{
					id:					jaspColumnRoot
					objectName:			"Column"
	property string __debugName:		"JASPColumn " + columnName
	property string columnName:			"?"
	property string columnIcon:			columnsModel.getColumnIcon(columnTypeHere, columnTypeHere != columnType)
	property int	columnType:			columnsModel.getColumnType(columnName)
	property int	columnTypeUser:		-1
	property int	columnTypeDrop:		-1	//If set to anything, it is what is allowed by whatever dropspot this column is in
	property int	columnTypeHere:		columnTypeDrop != -1 ? columnTypeDrop : columnTypeUser != -1 ? columnTypeUser : columnType
	property string preview:			(columnType == columnTypeHere ? "" : columnsModel.getColumnTransformedToolTip(columnName, columnTypeUser))
	property string toolTip:			formatToolTip(changeTypeAllowed, colName.contentWidth > colName.width, columnsModel.getColumnDescription(columnName), preview)
	property real	maxSize:			baseFontSize * 10 * preferencesModel.uiScale
					height:				filterConstructor.blockDim
					implicitWidth:		colName.x + colName.width
                    width:				implicitWidth
	property bool	isNumerical:		columnTypeHere == columnTypeScale
	property bool	isOrdinal:			columnTypeHere == columnTypeOrdinal
	property bool	changeTypeAllowed:	true
	property var	dragKeys:			columnTypeDrop == -1 ? ["number", "string", "ordered"] : isNumerical ? ["number"]	: isOrdinal ? ["string", "ordered"] : ["string"]
	property string typeString:			isNumerical ? "scale"		: isOrdinal ? "ordinal"				: "nominal"
					
	onColumnTypeHereChanged: {
		filterConstructor.somethingChanged = true
	}
	
	onColumnTypeUserChanged:		if(dropHandler.enabled) dropHandler.onWasDroppedOn(); else columnTypeDrop = -1;
	onColumnTypeDropChanged:		filterConstructor.somethingChanged = true;
	
	Connections
	{
		id:			dropHandler
		target:		parent
		enabled:	parent.objectName == "DragGeneric" && parent.parent != undefined && parent.parent.objectName == "DropSpot"
		
		function onEnabledChanged() {	if(!enabled) columnTypeDrop = 1; }
		
		function	onWasDroppedOn() 
		{
			columnTypeDrop = -1;
			

			if(columnTypeUser != -1 && dropAccepts(columnTypeToRelevantString(columnTypeUser)))
			{
				columnTypeDrop = columnTypeUser;
				return;
			}
			
			if(dropAccepts(columnTypeToRelevantString(columnType)))
			{
				columnTypeDrop = columnType;
				return;
			}
						
			var listTypes =  [ columnTypeScale, columnTypeOrdinal, columnTypeNominal ]
			for(var i=0; i<listTypes.length; i++)
				if(dropAccepts(columnTypeToRelevantString(listTypes[i])))
				{
					columnTypeDrop = listTypes[i];
					return;
				}
		}
		
		function dropAccepts(dragKeyToCheck)
		{
			var dropSpot = parent.parent;
			
			if(dropSpot == undefined || dropSpot.objectName != "DropSpot")
				return false;
			
			for(dragKey in dragKeyToCheck)
				if(dropSpot.dropKeys.indexOf(dragKeyToCheck))
					return true;
			return false;
		}
		
		function columnTypeToRelevantString(columnType)
		{
			return columnType == columnTypeOrdinal ? ["string", "ordered"] : columnType != columnTypeScale ? ["string"] : ["number"]
		}
	}
	//colName can elide
	function formatToolTip(typeChangeAble, colNameTrunc, descriptionV, previewV)
	{
		var stringList = []
		
		if(colNameTrunc)
			stringList.push(columnName)
		
		if(typeChangeAble)
			stringList.push(qsTr("Click icon to change column type"))
		
		if(descriptionV !== "")
			stringList.push(qsTr("Column description: ") + descriptionV)

		if(previewV !== "")
			stringList.push(previewV)
		
		return stringList.join("\n\n");
		
	}
					
					
	Connections
	{
		target:		columnsModel
		function onColumnTypeChanged(name)	
		{ 
			if(columnName == name)
			{
				columnType = columnsModel.getColumnType(columnName);
				filterConstructor.somethingChanged = true;
				
				if(dropHandler.enabled)
					dropHandler.onWasDroppedOn()
			}
		}
	}


	Image
	{
		id:				colIcon
		source:			filterConstructor.forceColumnInputs === "" ? columnIcon : computedColumnsInterface.computeColumnIconSource
		width:			height
		scale:			iconMouseArea.containsMouse ? jaspTheme.columnTypeScaleHovered : 1
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
			id:					iconMouseArea
			enabled:			changeTypeAllowed
			anchors.fill:		parent
			hoverEnabled:		true
			onClicked:
			{
				var functionCall      = function (index)
				{
					columnTypeUser = columnTypesModel.getType(index)
					customMenu.hideMenus()
				}

				var props = {
					"model":		columnTypesModel,
					"functionCall": functionCall
				};

				customMenu.scrollOri.x	= 0;
				customMenu.scrollOri.y	= 0;

				customMenu.toggle(parent, props);

				customMenu.menuScroll.x	= 0;
				customMenu.menuScroll.y	= 0;
				customMenu.menuMinIsMin	= true

			}

			cursorShape:		enabled ? Qt.PointingHandCursor : Qt.OpenHandCursor
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
	function returnR()							{ return columnName + "." + typeString }
	function checkCompletenessFormulas()		{ return true }
	function convertToJSON()
	{
		var jsonObj = { "nodeType": "Column", "columnName": columnName, "columnTypeUser": columnTypeUser, "columnTypeDrop": columnTypeDrop }
		return jsonObj
	}
}
