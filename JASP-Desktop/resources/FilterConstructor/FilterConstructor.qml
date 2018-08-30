import QtQuick.Controls 2.2
import QtQuick 2.9

Item {
	id: filterConstructor
	objectName:  "filterConstructor"

	property real blockDim: 20
	property real fontPixelSize: 14
	property var allKeys: ["number", "boolean", "string", "variable"]
	readonly property real desiredMinimumHeight: columnsRow.height + hints.height + applyFilter.height + (blockDim * 3)
	signal rCodeChanged(string rScript)
	property real extraSpaceUnderColumns: 0
	property bool somethingChanged: false

	property bool lastCheckPassed: true
	property bool showStartupMsg: true

	property alias functionModel: functieLijst.model

	onSomethingChangedChanged:
	{
		showStartupMsg = false

		if(somethingChanged)
			hints.filterText = ""
	}

	onVisibleChanged: if(visible && JSON.stringify(filterConstructor.returnFilterJSON()) != filterModel.constructedJSON)	initializeFromJSON(filterModel.constructedJSON)


	property string __debugName: "FilterConstructor"




	function checkAndApplyFilter()
	{
		focus = true
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
				hints.filterText += "Filter cleared<br>"
			else
				hints.filterText += "Filter applied<br>"

			filterModel.constructedJSON = JSON.stringify(filterConstructor.returnFilterJSON())
		}

		if(!allCorrect)
			hints.filterText += "Please enter all arguments - see fields marked in red.<br>"

		if(!allBoolean)
			hints.filterText += (!allCorrect ? "<br>" : "" ) + "Formula does not return a set of logical values, and therefore cannot be used in the filter.<br>"

		lastCheckPassed = allCorrect &&  allBoolean
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
			property string filterText: "Welcome to the drag and drop filter!<br>"
			id: hints
			text: filterText + (filterModel.filterErrorMsg !== "" ? "<br><i><font color=\"red\">"+filterModel.filterErrorMsg+"</font></i>" : "")

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			height: filterConstructor.fontPixelSize + contentHeight

			wrapMode: TextArea.WordWrap
			horizontalAlignment: TextArea.AlignHCenter

			textFormat: Text.StyledText
			font.pixelSize: filterConstructor.fontPixelSize
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
		//.replace(/\s/g,'')
		//console.log("last: ",jsonConverter.lastProperlyConstructedJSON.replace(/\s/g,''))
		//console.log("new:  ",JSON.stringify(returnFilterJSON()).replace(/\s/g,''))

		return jsonConverter.lastProperlyConstructedJSON !== JSON.stringify(returnFilterJSON())
	}

	JSONtoFormulas
	{
		id: jsonConverter
		objectName: "jsonConverter"
		property string jaspsFilterConstructorJSON:  filterModel.constructedJSON
		property string lastProperlyConstructedJSON: "{\"formulas\":[]}"

		onJaspsFilterConstructorJSONChanged:
		{
			if(jsonConverter.jaspsFilterConstructorJSON !== JSON.stringify(parent.returnFilterJSON()))
			{
				parent.initializeFromJSON()
				filterConstructor.checkAndApplyFilter()
			}

			parent.rememberCurrentConstructedJSON()
			filterModel.constructedR = scriptColumn.convertToR()

		}

		visible: false
	}

	function returnFilterJSON()				{ return scriptColumn.convertToJSON() }

	function initializeFromJSON()
	{
		if(filterModel.constructedJSON !== JSON.stringify(returnFilterJSON()))
		{
			trashCan.destroyAll(false);

			if(filterModel.constructedJSON !== "")
				jsonConverter.convertJSONtoFormulas(JSON.parse(filterModel.constructedJSON))
		}
	}

	function rememberCurrentConstructedJSON()
	{
		jsonConverter.lastProperlyConstructedJSON = JSON.stringify(returnFilterJSON())
	}
}
