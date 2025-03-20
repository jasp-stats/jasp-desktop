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

	property int parameterCount: 1
	property list<Item> droppedItems: []



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
		var compounded = functionName + "("

		for(var i=0; i<parameterCount; i++)
				compounded += (i > 0 ? ", " : "") + (dropRepeat.itemAtIndex(i) === null ? "null" : dropRepeat.itemAtIndex(i).returnR())

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

	Item
	{
		id: functionDef
		anchors.top: funcRoot.isRoot ? parent.top : meanBar.bottom
		anchors.bottom: parent.bottom

		x: extraMeanWidth / 2
		width: functionText.visible ? functionText.width : functionImg.width

		Text
		{
			id:						functionText

			anchors.top:			parent.top
			anchors.bottom:			parent.bottom
			color:					jaspTheme.textEnabled

			verticalAlignment:		Text.AlignVCenter
			horizontalAlignment:	Text.AlignHCenter

			text:					funcRoot.drawMeanSpecial ? "" : friendlyFunctionName
			font.pixelSize:			filterConstructor.fontPixelSize
			font.family:			jaspTheme.font.family

			visible:				!functionImg.visible
		}


		Image
		{
			id:						functionImg

			visible:				(!funcRoot.acceptsDrops) && functionImageSource !== ""

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

		width:	dropRepeat.implicitWidth
		height: dropRepeat.implicitHeight

		property real implicitWidthDrops: parent.acceptsDrops ? funcRoot.initialWidth / 4 : 0



		ListView
		{
			id:				dropRepeat
			model:			funcRoot.parameterCount
			reuseItems:		false
			cacheBuffer:	400
			//anchors.fill: parent
			
			width:			contentWidth
			height:			contentHeight
			orientation:	Qt.Horizontal

			property var rowWidthCalc: function()
			{
				var widthOut = 0
				for(var i=0; i<parameterCount; i++)
					widthOut += dropRepeat.itemAtIndex(i).width
				return widthOut
			}

			property var rowHeightCalc: function()
			{
				var heightOut = filterConstructor.blockDim
				for(var i=0; i<parameterCount; i++)
					heightOut = Math.max(dropRepeat.itemAtIndex(i).height, heightOut)

				return heightOut
			}

			///This also goes down the tree
			property var rightMostEmptyDropSpot: function()
			{
				var dropSpot = null

				for(var i=parameterCount-1; i>=0; i--)
				{
					var prevDropSpot = dropSpot
					dropSpot = dropRepeat.itemAtIndex(i).getDropSpot()

					if(dropSpot.containsItem !== null)
					{
						var subResult = dropSpot.containsItem.returnEmptyRightMostDropSpot()
						if(subResult === null) // cant put anything there but maybe we can return the previous (and thus empty dropspot?)
							return prevDropSpot //its ok if it is null. we just cant find anything here
						else
							return subResult
					}
					//else dropSpot now contains a DropSpot with space, but lets loop back to the beginning to see if we can go further left
				}
				return dropSpot
			}

			//this does not go down the tree
			property var leftMostFilledDropSpot: function()
			{
				var dropSpot = null

				for(var i=0; i<parameterCount; i++)
				{
					var prevDropSpot = dropSpot
					dropSpot = dropRepeat.itemAtIndex(i).getDropSpot()

					if(dropSpot.containsItem === null)
						return prevDropSpot //its ok if it is null. we just cant find anything here
				}
				return dropSpot.containsItem !== null ? dropSpot : null
			}

			function checkCompletenessFormulas()
			{
				var thereIsOne = false
				for(var i=0; i<dropRepeat.count; i++)
					if(dropRepeat.itemAtIndex(i).checkCompletenessFormulas())
						thereIsOne = true

				return thereIsOne
			}

			function convertToJSON()
			{
				var jsonObj = { "nodeType":"RowFunction", "functionName": functionName, "arguments":[], "parameterCount": parameterCount }

				for(var i=0; i<parameterCount; i++)
				{
					var dropSpot = dropRepeat.itemAtIndex(i).getDropSpot()
					
					if(dropSpot.containsItem !== null)
						jsonObj.arguments.push({ "name": i, "argument": dropSpot.containsItem.convertToJSON()})
				}
				return jsonObj
			}

			function getParameterDropSpot(param)
			{
				for(var i=0; i<parameterCount; i++)
					if(i == param)
						return dropRepeat.itemAtIndex(i).getDropSpot()

				return null

			}

			delegate:	Item
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
						return "null"
				}

				function getDropSpot() { return spot }

				function checkCompletenessFormulas()
				{
					return spot.checkCompletenessFormulas()
				}



				DropSpot 
				{
					id:					spot

					acceptsDrops:		funcRoot.acceptsDrops

					defaultText:		"..."
					dropKeys:			["number"]

					droppedShouldBeNested: funcRoot.parameterCount === 1 && !funcRoot.drawMeanSpecial
					shouldShowX: false

					onContainsItemChanged:
					{
						//First make the list of what is there now:
						var itsFull = true;
						funcRoot.droppedItems = []

						for(var i=0; i<parameterCount; i++)
							if(dropRepeat.itemAtIndex(i).getDropSpot().containsItem != null)
								funcRoot.droppedItems.push(dropRepeat.itemAtIndex(i).getDropSpot().containsItem)
							else
								itsFull = false;

						if(itsFull) //Then apparently this one just got filled?
						{
							for(var i=0; i<funcRoot.parameterCount; i++)
								dropRepeat.itemAtIndex(i).getDropSpot().containsItem = null;

							funcRoot.parameterCount++; //Is this enough to trigger the creation of new dropSpots?
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
