import QtQuick 2.9


Item
{
	id: funcRoot
	objectName: "Function"
	property string __debugName: "Function " + functionName

	property int initialWidth: filterConstructor.blockDim * 3
	property string functionName: "sum"
	property string friendlyFunctionName: functionName //By default exactly the same unless we need to add some fancy unicode
	property bool acceptsDrops: true

	property var parameterNames: []
	property var parameterDropKeys: [[]]

	property variant functionNameToImageSource: { "sum": jaspTheme.iconPath + "/sum.png", "prod": jaspTheme.iconPath + "/product.png", "sd": jaspTheme.iconPath + "/sigma.png", "var": jaspTheme.iconPath + "/variance.png", "!": jaspTheme.iconPath + "/negative.png", "sqrt":jaspTheme.iconPath + "/rootHead.png"}
	property string functionImageSource: functionNameToImageSource[functionName] !== undefined ? functionNameToImageSource[functionName] : ""
	property bool isNested: false
	property var booleanReturningFunctions: ["!", "hasSubstring"]

	readonly property bool isIfElse: functionName === "ifelse"
	property var ifElseReturn: ["string", "number", "boolean"]
	property var dragKeys: isIfElse ? ifElseReturn : booleanReturningFunctions.indexOf(functionName) >= 0 ? ["boolean"] : [ "number" ]

	readonly property bool isMean: functionName === "mean"
	readonly property bool isAbs:  functionName === "abs"
	readonly property bool isRoot: functionName === "sqrt"
	readonly property bool drawMeanSpecial: false
	readonly property bool showParentheses: !drawMeanSpecial && (parameterNames.length > 1 || isAbs || functionImageSource === "")

	property real extraMeanWidth: (drawMeanSpecial ? 10 * preferencesModel.uiScale : 0)

	property var addNARMFunctions: ["mean", "sd", "var", "sum", "prod", "min", "max", "mean", "median"]
	property string extraParameterCode: addNARMFunctions.indexOf(functionName) >= 0 ? ", na.rm=TRUE" : ""

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

		for(var i=0; i<funcRoot.parameterNames.length; i++)
				compounded += (i > 0 ? ", " : "") + (dropRepeat.itemAt(i) === null ? "null" : dropRepeat.itemAt(i).returnR())

		compounded += extraParameterCode + ")"

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
		visible: funcRoot.drawMeanSpecial || funcRoot.isRoot
		height: visible ? 6 * preferencesModel.uiScale : 0

		anchors.left: funcRoot.isRoot ? functionDef.right : parent.left
		anchors.right: parent.right
		anchors.top: parent.top

		Rectangle
		{

			color: jaspTheme.black

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.top: parent.top
			anchors.topMargin: funcRoot.isRoot ? 0 : Math.max(1, 3 * preferencesModel.uiScale)

			height: Math.max(1, 3 * preferencesModel.uiScale)
		}
	}

	Item
	{
		id: functionDef
		anchors.top: funcRoot.isRoot ? parent.top : meanBar.bottom
		anchors.bottom: parent.bottom

		x: extraMeanWidth / 2
		width: functionImgRoot.visible ? functionImgRoot.width : functionText.visible ? functionText.width : functionImg.width

		Text
		{
			id: functionText

			anchors.top: parent.top
			anchors.bottom: parent.bottom
			color:					jaspTheme.textEnabled

			verticalAlignment: Text.AlignVCenter
			horizontalAlignment: Text.AlignHCenter

			text: funcRoot.drawMeanSpecial || funcRoot.isAbs || funcRoot.isRoot ? "" : friendlyFunctionName
			font.pixelSize: filterConstructor.fontPixelSize

			visible: !functionImg.visible
		}


		Image
		{
			id: functionImg

			visible: (!funcRoot.isRoot || !funcRoot.acceptsDrops) && functionImageSource !== ""

			source: functionImageSource


			height: filterConstructor.blockDim
			width: height
			sourceSize.width: filterConstructor.blockDim * 2
			sourceSize.height: filterConstructor.blockDim * 2

			anchors.verticalCenter: parent.verticalCenter

		}

		Image
		{
			id: functionImgRoot

			visible: funcRoot.isRoot &&  funcRoot.acceptsDrops

			source: functionImageSource
			anchors.top: parent.top
			anchors.bottom: parent.bottom
			width: filterConstructor.blockDim
			sourceSize.width: filterConstructor.blockDim * 2
			sourceSize.height: filterConstructor.blockDim * 3
			smooth: true
		}
	}

	Text
	{
		id:				haakjesLinks
		anchors.top:	meanBar.top
		anchors.bottom: parent.bottom

		x:				functionDef.width + functionDef.x

		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignHCenter

		width: showParentheses ? filterConstructor.blockDim / 3 : 0
		text: ! showParentheses || funcRoot.isAbs || funcRoot.isRoot ? "" : "("
		font.pixelSize: filterConstructor.fontPixelSize
		color:					jaspTheme.textEnabled

		Rectangle
		{
			anchors.top: parent.top
			anchors.bottom: parent.bottom
			anchors.margins: 2
			anchors.horizontalCenter: parent.horizontalCenter

			color: jaspTheme.black
			width: 2

			visible: funcRoot.isAbs
		}

	}


	Row
	{
		id: dropRow
		anchors.top: meanBar.bottom
		//anchors.bottom: parent.bottom

		x: haakjesLinks.width + haakjesLinks.x

		width:	0
		height: 0

		property real implicitWidthDrops: parent.acceptsDrops ? funcRoot.initialWidth / 4 : 0



		Repeater
		{
			id:		dropRepeat
			model:	funcRoot.parameterNames
			//anchors.fill: parent

			property var rowWidthCalc: function()
			{
				var widthOut = 0
				for(var i=0; i<funcRoot.parameterNames.length; i++)
					widthOut += dropRepeat.itemAt(i).width
				return widthOut
			}

			property var rowHeightCalc: function()
			{
				var heightOut = filterConstructor.blockDim
				for(var i=0; i<funcRoot.parameterNames.length; i++)
					heightOut = Math.max(dropRepeat.itemAt(i).height, heightOut)

				return heightOut
			}

			///This also goes down the tree
			property var rightMostEmptyDropSpot: function()
			{
				var dropSpot = null

				for(var i=funcRoot.parameterNames.length-1; i>=0; i--)
				{
					var prevDropSpot = dropSpot
					dropSpot = dropRepeat.itemAt(i).getDropSpot()

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

				for(var i=0; i<funcRoot.parameterNames.length; i++)
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
				var allComplete = true
				for(var i=0; i<dropRepeat.count; i++)
					if(!dropRepeat.itemAt(i).checkCompletenessFormulas())
						allComplete = false

				return allComplete
			}

			function convertToJSON()
			{
				var jsonObj = { "nodeType":"Function", "functionName": functionName, "arguments":[] }

				for(var i=0; i<funcRoot.parameterNames.length; i++)
				{
					var dropSpot = dropRepeat.itemAt(i).getDropSpot()
					var argJson = dropSpot.containsItem === null ? null : dropSpot.containsItem.convertToJSON()
					jsonObj.arguments.push({ "name": funcRoot.parameterNames[i], "dropKeys": funcRoot.parameterDropKeys[i], "argument": argJson})
				}
				return jsonObj
			}

			function getParameterDropSpot(param)
			{
				for(var i=0; i<funcRoot.parameterNames.length; i++)
					if(funcRoot.parameterNames[i] === param)
						return dropRepeat.itemAt(i).getDropSpot()

				return null

			}

			function rebindSize()
			{
				dropRow.width	= Qt.binding(rowWidthCalc)
				dropRow.height	= Qt.binding(rowHeightCalc)
			}

			onItemAdded:	rebindSize()
			onItemRemoved:	rebindSize()

			Item
			{
				width: spot.width + comma.width
				height: spot.height

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



				DropSpot {
					id: spot

					height: implicitHeight
					implicitWidth: originalWidth
					implicitHeight: filterConstructor.blockDim

					acceptsDrops: funcRoot.acceptsDrops

					defaultText: funcRoot.parameterNames[index]
					dropKeys: funcRoot.parameterDropKeys[index]

					droppedShouldBeNested: funcRoot.parameterNames.length === 1 && !funcRoot.isAbs && !funcRoot.drawMeanSpecial
					shouldShowX: funcRoot.parameterNames <= 1
				}

				Text
				{
					id: comma
					text: index < funcRoot.parameterNames.length - 1 ? "," : ""

					font.pixelSize: filterConstructor.fontPixelSize
					color:					jaspTheme.textEnabled
					anchors.top: parent.top
					anchors.bottom: parent.bottom

					anchors.left: spot.right
				}
			}
		}
	}

	Text
	{
		id: haakjesRechts
		anchors.top: meanBar.top
		anchors.bottom: parent.bottom
		x: dropRow.x + dropRow.width

		verticalAlignment: Text.AlignVCenter
		horizontalAlignment: Text.AlignHCenter

		width: showParentheses ? filterConstructor.blockDim / 3 : 0
		text: !showParentheses || funcRoot.isAbs || funcRoot.isRoot ? "" : ")"
		font.pixelSize: filterConstructor.fontPixelSize
		color:					jaspTheme.textEnabled

		Rectangle
		{
			anchors.top: parent.top
			anchors.bottom: parent.bottom
			anchors.margins: 2
			anchors.horizontalCenter: parent.horizontalCenter

			color: jaspTheme.black
			width: 2

			visible: funcRoot.isAbs
		}
	}
}
