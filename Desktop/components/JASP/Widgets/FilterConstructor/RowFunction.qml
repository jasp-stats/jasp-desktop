import JASP
import QtQuick

Item
{
	id: funcRoot
	objectName: "RowFunction"
	property string __debugName: "RowFunction " + functionName

	property int initialWidth: filterConstructor.blockDim * 6
	property string functionName: "rowSum"
	property string friendlyFunctionName: functionName.endsWith("NaRm") ? functionName.substring(0, functionName.length-4) : functionName //By default exactly the same unless we need to add some fancy unicode
	property bool acceptsDrops: true

	property int parameterCount: droppedItems.length
	property list<string> droppedItems: ["null"]
	
	onDroppedItemsChanged: {
		filterConstructor.somethingChanged = true
		funcRoot.jsonChanged()
	}

	signal jsonChanged();

	property variant functionNameToBaseFunc: {
	"rowMean":				"mean",
	"rowMeanNaRm":			"mean",
	"rowSum":				"sum",
	"rowSumNaRm":			"sum",
	"rowSD":				"sd",
	"rowSDNaRm":			"sd",
	"rowVariance":			"variance",
	"rowVarianceNaRm":		"variance",
	"rowMedian":			"median",
	"rowMedianNaRm":		"median",
	"rowMin":				"min",
	"rowMinNaRm":			"min",
	"rowMax":				"max",
	"rowMaxNaRm":			"max"
	}


	property variant functionNameToImageSource: { "sum": jaspTheme.iconPath + "/sum.png", "prod": jaspTheme.iconPath + "/product.png", "sd": jaspTheme.iconPath + "/sigma.png", "var": jaspTheme.iconPath + "/variance.png", "!": jaspTheme.iconPath + "/negative.png", "sqrt":jaspTheme.iconPath + "/rootHead.png"}
	property string functionImageSource: functionNameToBaseFunc[functionName] !== undefined && functionNameToImageSource[functionNameToBaseFunc[functionName]] !== undefined ? functionNameToImageSource[functionNameToBaseFunc[functionName]] : ""
	property bool isNested: false


	property var dragKeys: [ "number" ]


	readonly property bool isMean: functionName === "mean"
	readonly property bool drawMeanSpecial: false
	readonly property bool showParentheses: !drawMeanSpecial && (parameterCount > 1 || functionImageSource === "")

	property real extraMeanWidth: (drawMeanSpecial ? 10 * preferencesModel.uiScale : 0)

	//property var addNARMFunctions: ["mean", "sd", "var", "sum", "prod", "min", "max", "mean", "median"]
	//property string extraParameterCode: addNARMFunctions.indexOf(functionName) >= 0 ? ", na.rm=TRUE" : ""
	//I guess we just only do the NaRm version here.

	height: meanBar.height + Math.max(dropRow.height, filterConstructor.blockDim)
	width: functionDef.width + haakjesLinks.width + dropRow.width + haakjesRechts.width + extraMeanWidth

	function shouldDrag(mouseX, mouseY)
	{
		if(!acceptsDrops)
			return true

		return mouseX <= functionDef.x + functionDef.width || ( showParentheses && ( mouseX <= haakjesLinks.width + haakjesLinks.x || mouseX > haakjesRechts.x)) || (meanBar.visible  && mouseY < meanBar.height + 6);
	}

	function returnR()
	{
		var compounded = functionName + "NaRm("

		var filledEntries = []
		
		for(var i=0; i<parameterCount; i++)
				if(dropRepeat.itemAt(i) !== null && dropRepeat.dropped[i] != "null"  && dropRepeat.dropped[i] != "")
					filledEntries.push(dropRepeat.itemAt(i).returnR())
		
		for(var i=0; i<filledEntries.length; i++)
			compounded += (i > 0 ? ", " : "") + filledEntries[i]

		compounded += ")"

		return compounded
	}

	function returnEmptyRightMostDropSpot()		{ return dropRepeat.rightMostEmptyDropSpot()	}
	function returnFilledRightMostDropSpot()	{ return dropRepeat.leftMostFilledDropSpot()	}
	function checkCompletenessFormulas()		{ return dropRepeat.checkCompletenessFormulas() }
	function convertToJSON()					{ return dropRepeat.convertToJSON()				}
	function getParameterDropSpot(param)		{ return dropRepeat.getParameterDropSpot(param) }

	Item
	{
		id: meanBar
		visible: funcRoot.drawMeanSpecial
		height: visible ? 6 * preferencesModel.uiScale : 0

		anchors.left: parent.left
		anchors.right: parent.right
		anchors.top: parent.top

		Rectangle
		{

			color: jaspTheme.black

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.top: parent.top
			anchors.topMargin: Math.max(1, 3 * preferencesModel.uiScale)

			height: Math.max(1, 3 * preferencesModel.uiScale)
		}
	}

	Row
	{
		id: functionDef
		anchors.top: funcRoot.isRoot ? parent.top : meanBar.bottom
		anchors.bottom: parent.bottom

		x: extraMeanWidth / 2

		Text
		{
			id:						functionText


			color:					jaspTheme.textEnabled

			verticalAlignment:		Text.AlignVCenter
			horizontalAlignment:	Text.AlignHCenter

			text:					functionImg.visible ? "row" : funcRoot.drawMeanSpecial ? "" : friendlyFunctionName
			font.pixelSize:			filterConstructor.fontPixelSize
			font.family:			jaspTheme.font.family
		}


		Image
		{
			id:						functionImg

			visible:				functionImageSource !== ""

			source:					functionImageSource


			height:					filterConstructor.blockDim
			width:					height
			sourceSize.width:		filterConstructor.blockDim * 2
			sourceSize.height:		filterConstructor.blockDim * 2

			anchors.verticalCenter: parent.verticalCenter

		}
	}

	Text
	{
		id:						haakjesLinks
		anchors.top:			meanBar.top
		anchors.bottom:			parent.bottom

		x:						functionDef.width + functionDef.x

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		width:					showParentheses ? filterConstructor.blockDim / 3 : 0
		text:					! showParentheses ? "" : "("
		font.pixelSize:			filterConstructor.fontPixelSize
		font.family:			jaspTheme.font.family
		color:					jaspTheme.textEnabled
	}


	Row
	{
		id: dropRow
		anchors.top: meanBar.bottom
		//anchors.bottom: parent.bottom

		x: haakjesLinks.width + haakjesLinks.x

	
		property real implicitWidthDrops: parent.acceptsDrops ? funcRoot.initialWidth / 4 : 0
		


		Repeater
		{
			id:				dropRepeat
			model:			parameterCount
			//anchors.fill: parent
					
			property alias dropped: funcRoot.droppedItems
			


			///This also goes down the tree
			property var rightMostEmptyDropSpot: function()
			{
				var dropSpot = null

				for(var i=parameterCount-1; i>=0; i--)
				{
					dropSpot = dropRepeat.itemAt(i).getDropSpot()


					if(dropSpot.containsItem !== null)
					{
						var subResult = dropSpot.containsItem.returnEmptyRightMostDropSpot()
						if(subResult !== null) // cant put anything there but maybe we can return the previous (and thus empty dropspot?)
							return subResult
					}
					else 
						return dropSpot;
				}
				
				return null
			}

			//this does not go down the tree
			property var leftMostFilledDropSpot: function()
			{
				var dropSpot = null
				
				for(var i=0; i<parameterCount; i++)
				{
					var prevDropSpot = dropSpot
					dropSpot = dropRepeat.itemAt(i).getDropSpot()

					if(dropSpot.containsItem === null)
						return prevDropSpot //its ok if it is null. we just cant find anything here
				}
				return dropSpot.containsItem !== null ? dropSpot : null
			}

			function checkCompletenessFormulas()
			{
				var thereIsOne = false
				for(var i=0; i<dropRepeat.count; i++)
					if(dropRepeat.itemAt(i).checkCompletenessFormulas(true))
						thereIsOne = true

				return thereIsOne
			}

			function convertToJSON()
			{
				var jsonObj = { "nodeType":"RowFunction", "functionName": functionName, "droppedItems": funcRoot.droppedItems.length > 0 ?  funcRoot.droppedItems : ["null"] }
				return jsonObj
			}

			function getParameterDropSpot(param)
			{
				for(var i=0; i<parameterCount; i++)
					if(i == param)
						return dropRepeat.itemAt(i).getDropSpot()
				
				if(param > 0 && param < parameterCount)
					return dropRepeat.itemAt(param).getDropSpot()

				return null

			}
			
			onItemAdded:
			{
				//rebindSize()
				//messages.log("dropRepeat.dropped is: ")
				for(var i=0; i<parameterCount; i++)
				{
					messages.log(dropRepeat.dropped[i])
					if(dropRepeat.itemAt(i) != null)
					{
						var dropSpot = dropRepeat.itemAt(i).getDropSpot()
						if(dropSpot.containsItem == null && dropRepeat.dropped[i] != "" && dropRepeat.dropped[i] != "null")
						{
							//messages.log("Converting onItemAdded stored json to item: " + dropRepeat.dropped[i] + " to fill " + dropSpot.containsItem)
							var jsonObjHere = JSON.parse(dropRepeat.dropped[i])
							jsonConverter.convertJSONtoItem(jsonObjHere, dropSpot) 	
						}
					}
				}
			
			
			}
			//onItemRemoved:  rebindSize()
			
		
			


			Item
			{
				implicitWidth:		spot.width + comma.width
				implicitHeight:		spot.height
				width:				implicitWidth
				height:				implicitHeight

				function returnR()
				{
					if(spot.containsItem != null)
						return spot.containsItem.returnR();
					else
						return ""
				}

				function getDropSpot() { return spot }

				function checkCompletenessFormulas()
				{
					return spot.checkCompletenessFormulas()
				}
				
				Component.onCompleted: 
				{
	
					if(spot.containsItem == null && dropRepeat.dropped[index] != "" && dropRepeat.dropped[index] != "null")
					{
						//messages.log("Converting onCompleted stored json to item: " + dropRepeat.dropped[index] + " to fill " + spot.containsItem)
						var jsonObjHere = JSON.parse(dropRepeat.dropped[index])
						jsonConverter.convertJSONtoItem(jsonObjHere, spot) 	
					}
				}
				
				
				DropSpot 
				{
					id:					spot

					acceptsDrops:		funcRoot.acceptsDrops

					defaultText:		"..."
					dropKeys:			["number"]

					droppedShouldBeNested: funcRoot.parameterCount === 1 && !funcRoot.drawMeanSpecial
					shouldShowX:		false
					ignoreEmpty:		true
					
					onSomethingDropped:		spot.handleContainsItemChange()
					onContainsItemChanged:	if(!containsItem) spot.handleContainsItemChange()
					onJsonChanged:			spot.handleContainsItemChange()
						
					function handleContainsItemChange()
					{
						if(spot.containsItem != null)
						{
							//First make the list of what is there now:
							var itsFull = true;
							
							var jsonStr = JSON.stringify(spot.containsItem.convertToJSON())
							
							//messages.log("Converted dropped thing to '" + jsonStr + "' and it was: " + dropRepeat.dropped[index] + " at index " + index)
							
							if(dropRepeat.dropped[index] == jsonStr)
							{
								//messages.log("Already there ")
							}
							else
							{
								/*messages.log("Before insert dropped is: ")
								for(var i=0; i<parameterCount; i++)
									messages.log(dropRepeat.dropped[i])*/
								
								dropRepeat.dropped[index] = jsonStr
								
								/*messages.log("After insert dropped is: ")
								for(var i=0; i<parameterCount; i++)
									messages.log(dropRepeat.dropped[i])*/
							}
	
							for(var i=0; i<parameterCount; i++)
								if(dropRepeat.dropped[i] == "null")
									itsFull = false;

							if(itsFull) //Then apparently this one just got filled?
							{
								dropRepeat.dropped.push("null")								
							}
						}
						else
						{
							dropRepeat.dropped[index] = "null"
						}
					}
				}

				Text
				{
					id:					comma
					text:				index < parameterCount - 1 ? ", " : ""

					font.pixelSize:		filterConstructor.fontPixelSize
					font.family:		jaspTheme.font.family
					color:				jaspTheme.textEnabled
					anchors.top:		parent.top
					anchors.bottom:		parent.bottom

					anchors.left:		spot.right
				}
			}
		}
	}

	Text
	{
		id:				haakjesRechts
		anchors.top:	meanBar.top
		anchors.bottom: parent.bottom
		x:				dropRow.x + dropRow.width

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		width:					 showParentheses ? filterConstructor.blockDim / 3 : 0
		text:					!showParentheses ? "" : ")"
		font.pixelSize:			filterConstructor.fontPixelSize
		font.family:			jaspTheme.font.family
		color:					jaspTheme.textEnabled

	}
}
