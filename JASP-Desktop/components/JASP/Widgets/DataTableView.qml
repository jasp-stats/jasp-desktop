import QtQuick 2.9
import QtQuick.Controls 1.4 as Old
import QtQuick.Controls 2.2
import QtGraphicalEffects 1.0
import JASP.Theme 1.0

FocusScope
{
	id: __myRoot

	signal doubleClicked()

	Rectangle
	{
		color:			Theme.white
		anchors.fill:	parent
		z:				-1
		border.width:	1
		border.color:	Theme.uiBorder

		JASPDataView
		{
			focus:				__myRoot.focus

			id:					dataTableView
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.bottom:		dataStatusBar.top

			font:	Theme.font

			model:				dataSetModel
			onDoubleClicked:	__myRoot.doubleClicked()

			leftTopCornerItem:
				RectangularButton
				{
					id:				filterToggleButton
					width:			dataTableView.rowNumberWidth
					toolTip:		filterWindow.opened ? "Hide filter" : "Show filter"
					iconSource:		"qrc:/images/filter.png"
					onClicked:		filterWindow.toggle()
					border.width:	0
				}

			extraColumnItem:
				RectangularButton
				{
					id:				addColumnButton
					width:			height
					toolTip:		"Add computed column"
					iconSource:		"qrc:/icons/addition-sign.svg"
					onClicked:		createComputeDialog.open()
					border.width:	0
				}

			rowNumberDelegate:
				Rectangle
				{
					//gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
					//					GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}
					color:	Theme.uiBackground
					Text {
						text:				rowIndex + 1
						anchors.centerIn:	parent
						font:				dataTableView.font
					}

				}

			columnHeaderDelegate: Rectangle
			{
				id: headerRoot
							property real	iconTextPadding:	10
				readonly	property int	__iconDim:			baseBlockDim * preferencesModel.uiScale

				Image
				{
					id:						colIcon
					anchors.verticalCenter: parent.verticalCenter
					anchors.left:			parent.left
					anchors.margins:		4

					function myColumnType() {return dataSetModel.columnIcon(columnIndex)}

					source: dataSetModel.getColumnTypesWithCorrespondingIcon()[myColumnType()]
					width:	headerRoot.__iconDim
					height: headerRoot.__iconDim

					sourceSize {	width:	width * 2
									height:	height * 2 }


					function setColumnType(columnType)
					{
						dataSetModel.setColumnTypeFromQML(columnIndex, columnType)

						if(variablesWindow.chosenColumn === columnIndex && colIcon.myColumnType() === columnTypeScale)
							variablesWindow.chooseColumn(-1)
					}

					ColumnTypeModel {
						id: columnTypeModel
					}

					MouseArea
					{
						anchors.fill:		parent
						onClicked:
						{
							// if(columnIndex > -1)
							// 	popupLoader.open()
							var functionCall      = function (index)
							{
								// FIXME:
								var columnType = [columnTypeScale, columnTypeOrdinal, columnTypeNominal][index];

								if (columnType !== undefined)
									colIcon.setColumnType(columnType);

								customMenu.remove()
							}
							var props = {
								"model"			: columnTypeModel,
								"functionCall"	: functionCall
							};

							customMenu.showMenu(colIcon, props, colIcon.width / 2, colIcon.height);
						}

						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		"Click here to change column type"
						ToolTip.timeout:	3000
						ToolTip.delay:		500
						cursorShape:		Qt.PointingHandCursor
					}
				}

				Image
				{
					id:			colIsComputed

					width:		visible ? headerRoot.__iconDim : 0
					height:		headerRoot.__iconDim
					visible:	columnIsComputed

					anchors.left:			colIcon.right
					anchors.verticalCenter: parent.verticalCenter
					anchors.margins:		visible ? 1 : 0

					source:				"qrc:/icons/computed.png"
					sourceSize {	width:	headerRoot.__iconDim * 2
									height:	headerRoot.__iconDim * 2 }

					MouseArea
					{
						anchors.fill:	parent
						onClicked:		computeColumnWindow.open(dataSetModel.headerData(columnIndex, Qt.Horizontal))

						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		"Click here to change the columns formulas"
						ToolTip.timeout:	3000
						ToolTip.delay:		500

						cursorShape:		Qt.PointingHandCursor

					}
				}

				Text
				{
					id:				headerTextItem

					text:			headerText
					font:			dataTableView.font

					horizontalAlignment:		Text.AlignHCenter

					anchors.horizontalCenter:	headerRoot.horizontalCenter
					anchors.verticalCenter:		headerRoot.verticalCenter
				}


				color:	Theme.uiBackground
				//gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
				//					GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}





				AnimatedImage
				{
					id:			colIsInvalidated

					source:		"qrc:/icons/loading.gif"
					width:		visible ? headerRoot.__iconDim : 0
					height:		headerRoot.__iconDim
					playing:	visible
					visible:	columnIsInvalidated

					anchors.right:			colFilterOn.left
					anchors.verticalCenter:	parent.verticalCenter
					anchors.margins:		visible ? 1 : 0
				}

				Image
				{
					id:			colHasError

					width:		columnError.length > 0 ? headerRoot.__iconDim : 0
					height:		headerRoot.__iconDim
					visible:	columnError.length > 0 // && !columnIsInvalidated

					source:					"qrc:/icons/error.png"
					sourceSize {	width:	headerRoot.__iconDim * 2
									height:	headerRoot.__iconDim * 2 }

					anchors.right:			colIsInvalidated.left
					anchors.verticalCenter:	parent.verticalCenter
					anchors.margins:		visible ? 1 : 0

					MouseArea
					{
						anchors.fill:		parent
						onClicked:			computeColumnWindow.open(dataSetModel.headerData(columnIndex, Qt.Horizontal))

						hoverEnabled:		true
						ToolTip.visible:	containsMouse && columnError.length > 0
						ToolTip.text:		columnError
						ToolTip.timeout:	3000
						ToolTip.delay:		500
						cursorShape:		Qt.PointingHandCursor

					}

				}


				Image
				{
					id:						colFilterOn

					width:					columnIsFiltered ? headerRoot.__iconDim : 0
					height:					headerRoot.__iconDim

					source:					"qrc:/images/filter.png"
					sourceSize {	width:	headerRoot.__iconDim * 2
									height:	headerRoot.__iconDim * 2 }

					anchors.right:			parent.right
					anchors.margins:		columnIsFiltered ? 1 : 0
					anchors.verticalCenter:	parent.verticalCenter
				}



				MouseArea
				{
					anchors.left:	colIsComputed.right
					anchors.top:	parent.top
					anchors.bottom: parent.bottom
					anchors.right:	colHasError.left

					onClicked:
					{
						var chooseThisColumn = (columnIndex > -1 && dataSetModel.columnIcon(columnIndex)  !== columnTypeScale) ? columnIndex : -1
						variablesWindow.chooseColumn(chooseThisColumn)

						if(columnIndex >= 0 && dataSetModel.columnUsedInEasyFilter(columnIndex))
						{
							filterWindow.showEasyFilter = true
							filterWindow.open()
						}

					}

					hoverEnabled:		true
					ToolTip.visible:	containsMouse && dataSetModel.columnIcon(columnIndex)  !== columnTypeScale
					ToolTip.text:		"Click here to change labels" + (columnIsFiltered ? " or inspect filter" : "" )
					ToolTip.timeout:	3000
					ToolTip.delay:		500
					cursorShape:		containsMouse && dataSetModel.columnIcon(columnIndex)  !== columnTypeScale ? Qt.PointingHandCursor : Qt.ArrowCursor
				}
			}
		}

		Rectangle
		{
			id:				dataStatusBar
			objectName:		"dataStatusBar"
			anchors.left:	parent.left
			anchors.right:	parent.right
			anchors.bottom: parent.bottom

			color:			"#EEEEEE"
			border.color:	"lightGrey"
			border.width:	1

			height:			datafiltertatusText.text.length > 0 ? datafiltertatusText.contentHeight + 16 : 0

			Text
			{
				id:						datafiltertatusText
				text:					filterModel.statusBarText
				font:					dataTableView.font
				anchors.left:			parent.left
				anchors.verticalCenter:	parent.verticalCenter
				anchors.leftMargin:		8
			}
		}
	}
}
