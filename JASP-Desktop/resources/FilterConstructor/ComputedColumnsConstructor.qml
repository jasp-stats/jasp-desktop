import QtQuick.Controls 2.2
import QtQuick 2.9

Item {
	id: filterConstructor
	objectName:  "computedColumnsConstructor"
	property real blockDim: 20
	property real fontPixelSize: 14
	property var allKeys: ["number", "boolean", "string", "variable"]
	readonly property real desiredMinimumHeight: columnsRow.height + hints.height + (blockDim * 3)

	property real extraSpaceUnderColumns: 0
	property bool somethingChanged: false
	property bool isColumnConstructor: true

	property bool lastCheckPassed: true
	property bool showStartupMsg: true

	property alias functionModel: functieLijst.model

	onSomethingChangedChanged:
	{
		showStartupMsg = false

		if(somethingChanged)
			hints.filterText = ""
	}

	property string __debugName: "filterConstructorComputedColumns"

	property string rCode: ""
	property string jsonConstructed: ""


	function checkAndApplyFilter()
	{
		focus = true
		filterConstructor.somethingChanged = false
		var allCorrect = true
		var onlyOneFormula = scriptColumn.children.length === 1
		var noFormulas = scriptColumn.children.length === 0

		for (var i = 0; i < scriptColumn.children.length; ++i)
		{
			if(!scriptColumn.children[i].checkCompletenessFormulas())
				allCorrect = false

			noFormulas = false
		}

		hints.filterText = ""

		if(allCorrect )
		{
			if(noFormulas)
				hints.filterText += "Computed columns code clear(ed)<br>"
			else
				hints.filterText += "Computed columns code applied<br>"

			filterConstructor.rCode = scriptColumn.convertToR()
			filterConstructor.jsonConstructed = JSON.stringify(filterConstructor.returnFilterJSON())
		}

		if(!allCorrect)
			hints.filterText += "Please enter all arguments - see fields marked in red.<br>"

		if(!onlyOneFormula && !noFormulas)
			hints.filterText += (!allCorrect ? "<br>" : "" ) + "Only one formula per computed column allowed.<br>"

		lastCheckPassed = allCorrect && onlyOneFormula
		return lastCheckPassed
	}

	OperatorSelectorComputedColumns
	{
		id: columnsRow
		anchors.top: parent.top
		anchors.left: parent.left
		anchors.right: parent.right

		height: filterConstructor.blockDim * 1.75

		z: 3

		horizontalCenterX: computedHintsColumns.x + (computedHintsColumns.width * 0.5)

	}

	Rectangle {
		id: background

		color: "white"
		border.width: 1
		border.color: "lightGrey"

		anchors.fill: parent
		z: -3

		Image
		{
			id:							backgroundImage

			source:						"qrc:/icons/columnConstructorBackground.png"
			anchors.centerIn:			parent

			property real widthScale:	parent.width  / implicitWidth
			property real heightScale:	parent.height / implicitHeight
			property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

			width:						implicitWidth * ratio
			height:						implicitHeight * ratio
		}
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
		id: computedHintsColumns

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
			property string filterText: "Welcome to the drag and drop computed column constructor!<br>"
			id: hints
			text: filterText

			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			height: font.pixelSize + contentHeight

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
		//console.log("last: ",jsonConverterComputedColumns.lastProperlyConstructedJSON.replace(/\s/g,''))
		//console.log("new:  ",JSON.stringify(returnFilterJSON()).replace(/\s/g,''))

		return jsonConverterComputedColumns.lastProperlyConstructedJSON !== JSON.stringify(returnFilterJSON())
	}

	JSONtoFormulas
	{
		id: jsonConverterComputedColumns
		objectName: "jsonConverterComputedColumns"
		property string jaspsfilterConstructorJSON:  "{\"formulas\":[]}"
		property string lastProperlyConstructedJSON: "{\"formulas\":[]}"

		onJaspsfilterConstructorJSONChanged:
		{
			//console.log("onJaspsfilterConstructorJSONChanged ",jaspsfilterConstructorJSON)

			if(jsonConverterComputedColumns.jaspsfilterConstructorJSON !== JSON.stringify(parent.returnFilterJSON()))
			{
				parent.initializeFromJSON(jsonConverterComputedColumns.jaspsfilterConstructorJSON)
				filterConstructor.checkAndApplyFilter()
			}

			jsonConverterComputedColumns.lastProperlyConstructedJSON = JSON.stringify(returnFilterJSON())
		}



		visible: false
	}

	function returnFilterJSON()				{ return scriptColumn.convertToJSON() }
	function initializeFromJSON(jsonString)
	{
		trashCan.destroyAll();
		if(jsonString !== "")
			jsonConverterComputedColumns.convertJSONtoFormulas(JSON.parse(jsonString))

		filterConstructor.somethingChanged = false
	}
}
