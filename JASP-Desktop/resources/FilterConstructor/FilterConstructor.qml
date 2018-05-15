import QtQuick.Controls 2.3
import QtQuick 2.9

Item {
	id: filterConstructor
	property real blockDim: 20
	property real fontPixelSize: 14
	property var allKeys: ["number", "boolean", "string", "variable"]
	readonly property real desiredMinimumHeight: columnsRow.height + hints.height + applyFilter.height + (blockDim * 3)
	signal rCodeChanged(string rScript)
	property real extraSpaceUnderColumns: 0
	property bool somethingChanged: false
	property bool lastCheckPassed: true

	onVisibleChanged: if(visible) initializeFromJSON(jsonConverter.jaspsFilterConstructorJSON)
	property string __debugName: "FilterConstructor"


	function checkAndApplyFilter()
	{
		filterConstructor.somethingChanged = false
		var allCorrect = true
		var allBoolean = true
		var noFormulas = true

		for (var i = 0; i < scriptColumn.children.length; ++i)
		{
			if(!scriptColumn.children[i].checkCompletenessFormulas())
				allCorrect = false

			if(scriptColumn.children[i].dragKeys.indexOf("boolean") < 0)
				allBoolean = false

			noFormulas = false
		}

		hints.filterText = ""

		if(allCorrect && allBoolean)
		{
			if(noFormulas)
				hints.filterText += "Filter cleared.<br>"
			else
				hints.filterText += "Filter applied!<br>"

			filterConstructor.rCodeChanged(scriptColumn.convertToR())
			mainWindow.setFilterConstructorJSON(JSON.stringify(filterConstructor.returnFilterJSON()))
		}

		if(!allCorrect)
			hints.filterText += "You did not fill in all the arguments yet, see the fields marked in red.<br>"

		if(!allBoolean)
			hints.filterText += (!allCorrect ? "<br>" : "" ) + "Not every formula returns a set of logicals and thus cannot be used in the filter, to remedy this try to place comparison-operators such as '=' or '<' and the like as the roots of each formula.<br>"

		lastCheckPassed = allCorrect && allBoolean
		return lastCheckPassed
	}

	OperatorSelector
	{
		id: columnsRow
		anchors.top: parent.top
		anchors.left: parent.left
		anchors.right: parent.right

		height: filterConstructor.blockDim * 1.75

		z: 3

		horizontalCenterX: filterHintsColumns.x + (filterHintsColumns.width * 0.5)

	}

	Rectangle {
		id: background

		color: "white"
		border.width: 1
		border.color: "lightGrey"

		anchors.fill: parent
		z: -3
	}

	Item
	{
		id: columnList

		//anchors.top: columnsRow.bottom
		anchors.top: columnsRow.bottom
		anchors.left: parent.left
		anchors.bottom: parent.bottom
		anchors.bottomMargin: filterConstructor.extraSpaceUnderColumns + filterConstructor.blockDim

		width: columns.width

		ElementView
		{
			id: columns
			model: columnsModel
			anchors.top: parent.top
			anchors.left: parent.left
			anchors.bottom: parent.bottom
		}
	}

	Item
	{
		id: filterHintsColumns

		anchors.top: columnsRow.bottom
		anchors.left: columnList.right
		anchors.right: funcVarLists.left
		anchors.bottom: parent.bottom
		//border.width: 1
		//border.color: "grey"

		z: -1
		//clip: true



		Rectangle
		{
			id: rectangularColumnContainer
			z: parent.z + 1
			anchors.top: parent.top
			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: hints.top

			border.width: 1
			border.color: "grey"
			color: "transparent"

			//clip: true

			ScrollView
			{
				id: scrollScriptColumn
				anchors.fill: parent
				anchors.margins: 4
				clip: true

				contentWidth: scriptColumn.childrenRect.width
				contentHeight: scriptColumn.childrenRect.height

				Item {

					Column
					{
						z: parent.z + 1
						id: scriptColumn
						objectName: "scriptColumn"
						property string __debugName: "scriptColumn"

						anchors.fill: parent
						anchors.margins: 4

						function convertToR()
						{
							var uit = ""
							for (var i = 0; i < children.length; ++i)
								uit += ( i > 0 ? "& ": "") + children[i].returnR() + "\n"

							return uit
						}

						function convertToJSON()
						{
							var jsonObj = { "formulas": [] }
							for (var i = 0; i < children.length; ++i)
								jsonObj.formulas.push(children[i].convertToJSON())

							return jsonObj
						}
					}
				}
			}

			MouseArea
			{
				anchors.fill: parent
				onPressed: { scriptColumn.focus = true; mouse.accepted = false; }
			}

			DropTrash
			{
				id: trashCan

				anchors.bottom: parent.bottom
				anchors.right: parent.right

				height: Math.min(60, scrollScriptColumn.height)
			}


		}

		Text
		{
			property string filterText: "Welcome to the filterconstructor of JASP!<br>"
			id: hints
			text: filterText + (filterErrorText !== "" ? "<br><i><font color=\"red\">"+filterErrorText+"</font></i>" : "")

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			height: font.pixelSize + contentHeight

			wrapMode: TextArea.WordWrap
			horizontalAlignment: TextArea.AlignHCenter

			textFormat: Text.StyledText
		}

	}

	Item
	{
		id: funcVarLists

		anchors.top: columnsRow.bottom
		anchors.right: parent.right
		anchors.bottom: parent.bottom
		anchors.rightMargin: 4
		//border.width: 1
		//border.color: "grey"

		width: functieLijst.width + 4

		ElementView
		{
			id: functieLijst
			model: ListModel
			{

				ListElement	{ type: "function";	functionName: "abs";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "absolute value" }
				ListElement	{ type: "function";	functionName: "sd";		functionParameters: "values";	functionParamTypes: "number";			toolTip: "standard deviation" }
				ListElement	{ type: "function";	functionName: "var";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "variance" }
				ListElement	{ type: "function";	functionName: "sum";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "summation" }
				ListElement	{ type: "function";	functionName: "prod";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "product of values" }

				ListElement	{ type: "function";	functionName: "min";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "returns minimum of values" }
				ListElement	{ type: "function";	functionName: "max";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "returns maximum of values" }
				ListElement	{ type: "function";	functionName: "mean";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "mean" }
				ListElement	{ type: "function";	functionName: "round";	functionParameters: "y,n";		functionParamTypes: "number,number";	toolTip: "rounds y to n decimals" }
				ListElement	{ type: "function";	functionName: "length";	functionParameters: "y";		functionParamTypes: "any";				toolTip: "returns number of elements in y" }
				ListElement	{ type: "function";	functionName: "median";	functionParameters: "values";	functionParamTypes: "number";			toolTip: "median" }
/*
				ListElement	{ type: "separator" }
				ListElement	{ type: "function";	functionName: "rnorm";	functionParameters: "n,mean,sd";functionParamTypes: "number,number,number";	toolTip: "generates a guassian distribution of n elements with specified mean and standard deviation sd" }
				ListElement	{ type: "function";	functionName: "rexp";	functionParameters: "n,rate";	functionParamTypes: "number,number,number";	toolTip: "generates a exponential distribution of n elements with specified rate" }

				ListElement	{ type: "separator" }

				ListElement	{ type: "function";	functionName: "sin";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "sine" }
				ListElement	{ type: "function";	functionName: "cos";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "cosine" }
				ListElement	{ type: "function";	functionName: "tan";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "tangent" }

				ListElement	{ type: "function";	functionName: "asin";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "arc or inverse sine" }
				ListElement	{ type: "function";	functionName: "acos";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "arc or inverse cosine" }
				ListElement	{ type: "function";	functionName: "atan";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "arc or inverse tangent" }
				ListElement	{ type: "function";	functionName: "atan2";	functionParameters: "x,y";		functionParamTypes: "number,number";	toolTip: "arc or inverse tangent that returns angle of x,y within a full circle" }

				ListElement	{ type: "separator" }

				ListElement	{ type: "function";	functionName: "log";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "natural logarithm" }
				ListElement	{ type: "function";	functionName: "log2";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "base 2 logarithm" }
				ListElement	{ type: "function";	functionName: "log10";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "base 10 logarithm" }
				ListElement	{ type: "function";	functionName: "logb";	functionParameters: "y,base";	functionParamTypes: "number";			toolTip: "logarithm of y in 'base'" }
				ListElement	{ type: "function";	functionName: "exp";	functionParameters: "y";		functionParamTypes: "number";			toolTip: "exponential" }

				ListElement	{ type: "function";	functionName: "match";	functionParameters: "x,y";		functionParamTypes: "any,any";			toolTip: "returns the list X with NA for each element that is not in Y" } */


			}
			anchors.top: parent.top
			anchors.right: parent.right
			anchors.bottom: parent.bottom
			anchors.margins: 2
			anchors.bottomMargin: filterConstructor.extraSpaceUnderColumns + filterConstructor.blockDim

			width: 80
		}
	}

	function jsonChanged()
	{
		return jsonConverter.jaspsFilterConstructorJSON !== JSON.stringify(returnFilterJSON())
	}

	JSONtoFormulas
	{
		id: jsonConverter
		property string jaspsFilterConstructorJSON: filterConstructorJSONstring

		onJaspsFilterConstructorJSONChanged:
		{
			//console.log("onJaspsFilterConstructorJSONChanged ",jaspsFilterConstructorJSON)

			if(jaspsFilterConstructorJSON !== JSON.stringify(parent.returnFilterJSON()))
			{
				parent.initializeFromJSON(jaspsFilterConstructorJSON)
				applyFilter.onClickedFunction()
			}
		}
	}

	function returnFilterJSON()				{ return scriptColumn.convertToJSON() }
	function initializeFromJSON(jsonString)
	{
		trashCan.destroyAll();
		if(jsonString !== "")
			jsonConverter.convertJSONtoFormulas(JSON.parse(jsonString))


	}




}
