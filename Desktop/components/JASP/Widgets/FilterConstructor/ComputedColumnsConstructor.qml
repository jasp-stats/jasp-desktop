import JASP.Controls 1.0
import QtQuick.Controls 2.2
import QtQuick 2.9


Item
{
								id:						filterConstructor
								objectName:				"computedColumnsConstructor"
				property string	__debugName:			"filterConstructorComputedColumns"
				property real	fontPixelSize:			baseFontSize * preferencesModel.uiScale
				property real	blockDim:				baseBlockDim * preferencesModel.uiScale
				property var	allKeys:				["number", "boolean", "string", "variable"]
	readonly	property real	desiredMinimumHeight:	columnsRow.height + hints.height + (blockDim * 3) + generatedRCodeBox.height
				property real	extraSpaceUnderColumns: 0
				property bool	somethingChanged:		false
				property bool	isColumnConstructor:	true
				property bool	showGeneratedRCode:		false
				property bool	lastCheckPassed:		true
				property bool	showStartupMsg:			true
				property alias	functionModel:			functieLijst.model
				property string	rCode:					""
				property string jsonConstructed:		""

	onSomethingChangedChanged:
	{
		showStartupMsg = false

		if(somethingChanged)
			hints.filterText = ""
	}

	function checkAndApplyFilter()
	{
		focus = true
		filterConstructor.somethingChanged = false

		var allCorrect		= true
		var onlyOneFormula	= scriptColumn.children.length === 1
		var noFormulas		= scriptColumn.children.length === 0

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
				hints.filterText += qsTr("Computed columns code clear(ed)")
			else
				hints.filterText += qsTr("Computed columns code applied")

			filterConstructor.rCode = scriptColumn.convertToR()

			filterConstructor.jsonConstructed = JSON.stringify(filterConstructor.returnFilterJSON())
		}

		if(!allCorrect)
			hints.filterText += qsTr("Please enter all arguments - see fields marked in red.")

		if(!onlyOneFormula && !noFormulas)
			hints.filterText += (!allCorrect ? "<br>" : "" ) + qsTr("Only one formula per computed column allowed.")

		lastCheckPassed = allCorrect && onlyOneFormula
		return lastCheckPassed
	}

	Rectangle {
		id: background

		color: jaspTheme.white
		border.width: 1
		border.color: "lightGrey"

		anchors.fill: parent
		z: -3

		Image
		{
			id:							backgroundImage

			source:						jaspTheme.iconPath + "/columnConstructorBackground.png"
			anchors.centerIn:			parent

			property real widthScale:	parent.width  / implicitWidth
			property real heightScale:	parent.height / implicitHeight
			property real ratio:		Math.min(Math.min(widthScale, heightScale), 1.0) * 0.5

			width:						implicitWidth * ratio
			height:						implicitHeight * ratio
		}
	}

	OperatorSelectorComputedColumns
	{
		id:				columnsRow
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	parent.right

		height:			filterConstructor.blockDim * 1.75

		z:				3

		horizontalCenterX: computedHintsColumns.x + (computedHintsColumns.width * 0.5)

	}

	Item
	{
		id: columnList

		//anchors.top: columnsRow.bottom
		anchors.top: columnsRow.bottom
		anchors.left: parent.left
		anchors.bottom: parent.bottom
		anchors.bottomMargin: filterConstructor.extraSpaceUnderColumns + filterConstructor.blockDim

		width: columns.width + columnsLeftScrollBar.width

		JASPScrollBar
		{
			id:				columnsLeftScrollBar
			flickable:		columns
			manualAnchor:	true

			anchors
			{
				top:		parent.top
				left:		parent.left
				bottom:		parent.bottom
			}
		}

		ElementView
		{
			id:				columns
			model:			columnsModel
			anchors.top:	parent.top
			anchors.left:	columnsLeftScrollBar.right
			anchors.bottom:	parent.bottom
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
								uit += ( i > 0 ? "& ": "") + children[i].returnR() + (i < children.length - 1 ? "\n" : "")

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
			property string filterText: qsTr("Welcome to the drag and drop computed column constructor!")

			id:						hints
			text:					filterText

			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			generatedRCodeBox.top

			height:					font.pixelSize + contentHeight

			wrapMode:				TextEdit.WordWrap
			horizontalAlignment:	TextArea.AlignHCenter

			textFormat:				Text.StyledText
			font.pixelSize:			filterConstructor.fontPixelSize
			color:					jaspTheme.textEnabled
		}

		Rectangle
		{
			id:						generatedRCodeBox

			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			parent.bottom
			anchors.bottomMargin:	2

			height:					visible ? generatedRcode.contentHeight + 8: 0
			visible:				filterConstructor.showGeneratedRCode

			color:					jaspTheme.white
			border.color:			jaspTheme.gray
			border.width:			1

			Text
			{
				id:							rLetter
				text:						"R:"
				color:						jaspTheme.textEnabled
				anchors.left:				parent.left
				anchors.verticalCenter:		parent.verticalCenter
				anchors.leftMargin:			4
				font.pixelSize:				filterConstructor.fontPixelSize
			}

			TextEdit
			{

				id:						generatedRcode
				text:					filterConstructor.rCode
				color:					jaspTheme.textEnabled

				wrapMode:				TextEdit.WordWrap
				horizontalAlignment:	TextEdit.AlignLeft
				verticalAlignment:		TextEdit.AlignVCenter

				textFormat:				Text.PlainText
				font.family:			"Courier"
				font.pixelSize:			filterConstructor.fontPixelSize
				selectByMouse:			true
				selectByKeyboard:		true
				readOnly:				true

				anchors
				{
					left:			rLetter.right
					right:			parent.right

					verticalCenter:	rLetter.verticalCenter
					leftMargin:		4
				}
			}
		}

	}

	Item
	{
		id: funcVarLists

		anchors.top: columnsRow.bottom
		anchors.right: parent.right
		anchors.bottom: parent.bottom
		anchors.rightMargin: 4 * preferencesModel.uiScale

		width: functieLijst.width + anchors.rightMargin + functionsRightScrollBar.width

		JASPScrollBar
		{
			id:				functionsRightScrollBar
			flickable:		functieLijst
			manualAnchor:	true

			anchors
			{
				top:			parent.top
				right:			parent.right
				bottom:			parent.bottom
				margins:		functieLijst.anchors.margins
				bottomMargin:	functieLijst.anchors.bottomMargin
			}
		}

		ElementView
		{
			id:					functieLijst
			anchors
			{
				top:			parent.top
				right:			functionsRightScrollBar.left
				bottom:			parent.bottom
				margins:		2 * preferencesModel.uiScale
				bottomMargin:	filterConstructor.extraSpaceUnderColumns + filterConstructor.blockDim
			}

			width:	80 * preferencesModel.uiScale //for init or something?
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
		{
			jsonConverterComputedColumns.convertJSONtoFormulas(JSON.parse(jsonString))
			checkAndApplyFilter()
		}

		filterConstructor.somethingChanged = false
	}
}
