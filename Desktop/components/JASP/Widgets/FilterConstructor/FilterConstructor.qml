import JASP.Controls 1.0
import QtQuick.Controls 2.2
import QtQuick 2.9


Item
{
								id:						filterConstructor
								objectName:				"filterConstructor"
				property string __debugName:			"FilterConstructor"
				property real	fontPixelSize:			baseFontSize * preferencesModel.uiScale
				property real	blockDim:				baseBlockDim * preferencesModel.uiScale
				property var	allKeys:				["number", "boolean", "string", "variable"]
	readonly	property real	desiredMinimumHeight:	operatorsRow.height + hints.height + applyFilter.height + (blockDim * 3)
				property real	extraSpaceUnderColumns:	0
				property bool	somethingChanged:		false
				property bool	isColumnConstructor:	false
				property bool	lastCheckPassed:		true
				property bool	showStartupMsg:			true
				property alias	functionModel:			functieLijst.model

	signal rCodeChanged(string rScript)

	onSomethingChangedChanged:
	{
		showStartupMsg = false

		if(somethingChanged)
			hints.filterText = ""
	}

	onVisibleChanged: if(visible && JSON.stringify(filterConstructor.returnFilterJSON()) != filterModel.constructedJSON)	initializeFromJSON(filterModel.constructedJSON)

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
				hints.filterText += qsTr("Filter cleared<br>")
			else
				hints.filterText += qsTr("Filter applied<br>")

			filterModel.constructedJSON = JSON.stringify(filterConstructor.returnFilterJSON())
		}

		if(!allCorrect)
			hints.filterText += qsTr("Please enter all arguments - see fields marked in red.<br>")

		if(!allBoolean)
			hints.filterText += (!allCorrect ? "<br>" : "" ) + qsTr("Formula does not return a set of logical values, and therefore cannot be used in the filter.<br>")

		lastCheckPassed = allCorrect &&  allBoolean
		return lastCheckPassed
	}

	Rectangle
	{
		id:				background
		color:			jaspTheme.white
		border.width:	1
		border.color:	jaspTheme.uiBackground
		anchors.fill:	parent
		z:				-3

		Image
		{
			id:							backgroundImage

			source:						jaspTheme.iconPath + "/filterConstructorBackground.png"
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
		id:				fadeCollector

		property int minWidthConstructor: 400 * preferencesModel.uiScale

		anchors
		{
			top:			parent.top
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			rightMargin:	Math.min(0, filterConstructor.width - fadeCollector.minWidthConstructor)
		}

		OperatorSelector
		{
			id:					operatorsRow
			height:				filterConstructor.blockDim * 1.75
			z:					3
			horizontalCenterX:	filterHintsColumns.x + (filterHintsColumns.width * 0.5)
			anchors
			{
				top:	parent.top
				left:	parent.left
				right:	parent.right
			}

		}

		Item
		{
			id:		columnList
			width:	columns.width + columnsLeftScrollBar.width

			anchors
			{
				top:			operatorsRow.bottom
				left:			parent.left
				bottom:			parent.bottom
				bottomMargin:	filterConstructor.extraSpaceUnderColumns + filterConstructor.blockDim
			}


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
			id:				filterHintsColumns
			z:				-1
			anchors
			{
				top:	operatorsRow.bottom
				left:	columnList.right
				right:	funcVarLists.left
				bottom: parent.bottom
			}

			Rectangle
			{
				id:				rectangularColumnContainer
				z:				parent.z + 1
				border.width:	1
				border.color:	jaspTheme.uiBorder
				color:			"transparent"

				anchors
				{
					top:	parent.top
					left:	parent.left
					right:	parent.right
					bottom:	hints.top
				}

				ScrollView
				{
					id:					scrollScriptColumn
					anchors.fill:		parent
					anchors.margins:	4
					clip:				true

					contentWidth:		scriptColumn.childrenRect.width
					contentHeight:		scriptColumn.childrenRect.height

					Item
					{

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

					height: Math.min(60 * preferencesModel.uiScale, scrollScriptColumn.height)
				}


			}

			Text
			{
				property string filterText: qsTr("Welcome to the drag and drop filter!<br>")

				id:						hints
				text:					filterText + (filterModel.filterErrorMsg !== "" ? "<br><i><font color=\"red\">"+filterModel.filterErrorMsg+"</font></i>" : "")

				color:					jaspTheme.textEnabled

				height:					filterConstructor.fontPixelSize + contentHeight

                wrapMode:				Text.WordWrap
                horizontalAlignment:	Text.AlignHCenter

				textFormat:				Text.StyledText
				font.pixelSize:			filterConstructor.fontPixelSize

				anchors
				{
					left:				parent.left
					right:				parent.right
					bottom:				parent.bottom
				}
			}

		}

		Item
		{
			id:					funcVarLists

			anchors
			{
				top:			operatorsRow.bottom
				right:			parent.right
				bottom:			parent.bottom
				rightMargin:	4 * preferencesModel.uiScale
			}

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
				id:						functieLijst
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
