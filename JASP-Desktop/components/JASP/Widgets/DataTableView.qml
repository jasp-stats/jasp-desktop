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

		JASPDataView
		{
			focus:				__myRoot.focus

			id:					dataTableView
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.bottom:		dataStatusBar.top

			font.pixelSize:		baseFontSize * ppiScale

			//headersGradient: myHeadersGradient
			model:				dataSetModel

			onDoubleClicked:	__myRoot.doubleClicked()

			leftTopCornerItem:
				Rectangle
				{
					id: filterToggleButton
					gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
										GradientStop { position: 1.0;	color: "#DDDDDD" }	GradientStop { position: 0.77;	color: "#DDDDDD" }	}

					ToolTip.text:		filterWindow.opened ? "Hide filter" : "Show filter"
					ToolTip.timeout:	4500
					ToolTip.delay:		500
					ToolTip.visible:	filterToggleButtonMouseArea.containsMouse

					Image
					{
						source:				"qrc:/images/filter.png"
						sourceSize.width:	width * 2
						sourceSize.height:	height * 2
						width:				height

						anchors.top:				parent.top
						anchors.bottom:				parent.bottom
						anchors.horizontalCenter:	parent.horizontalCenter
						anchors.margins:			4


					}

					MouseArea
					{
						id:				filterToggleButtonMouseArea
						anchors.fill:	parent
						onClicked:		filterWindow.toggle()
						hoverEnabled:	true
						cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
					}
				}

			CreateComputeColumnDialog { id: createComputeDialog	}

			extraColumnItem:
				Rectangle
				{
					id:		addColumnButton
					width:	40

					gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
										GradientStop { position: 1.0;	color: "#DDDDDD" }	GradientStop { position: 0.77;	color: "#DDDDDD" }	}

					ToolTip.text:		"Add computed column"
					ToolTip.timeout:	4500
					ToolTip.delay:		500
					ToolTip.visible:	addColumnButtonMouseArea.containsMouse

					Image
					{
						source:				"qrc:/icons/addition.png"
						sourceSize.width:	width
						sourceSize.height:	height
						width:				height

						anchors.top:				parent.top
						anchors.bottom:				parent.bottom
						anchors.horizontalCenter:	parent.horizontalCenter
						anchors.margins:	4

					}

					MouseArea
					{
						id:				addColumnButtonMouseArea
						anchors.fill:	parent
						onClicked:		createComputeDialog.open()
						hoverEnabled:	true
						cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
					}


				}

			rowNumberDelegate:
				Rectangle
				{
					gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
										GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}

					Text {
						text:				rowIndex
						anchors.centerIn:	parent
						font:				dataTableView.font
					}

				}

			columnHeaderDelegate: Rectangle
			{
				id: headerRoot
							property real	iconTextPadding:	10
				readonly	property int	__iconDim:			baseBlockDim * ppiScale

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
						//colIcon.myColumnType =
						dataSetModel.setColumnTypeFromQML(columnIndex, columnType)

						if(variablesWindow.chosenColumn === columnIndex && colIcon.myColumnType() === columnTypeScale)
							variablesWindow.chooseColumn(-1)


					}

					MouseArea
					{
						anchors.fill:		parent
						onClicked:			if(columnIndex > -1) popupIcons.open()

						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		"Click here to change columntype"
						ToolTip.timeout:	3000
						ToolTip.delay:		500
						cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

					}


					Popup {
						id: popupIcons; modal: true; focus: true;
						padding: 8
						spacing: 4
						y: colIcon.y + colIcon.height
						x: colIcon.x - (headerRoot.__iconDim * 0.5)

						closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape


						Column
						{
							//width: parent.width
							spacing: popupIcons.padding / 2

							Repeater{
								id: iconRepeater
								model: columnIsComputed ? [columnTypeScale, columnTypeOrdinal, columnTypeNominal, columnTypeNominalText] :
														  [columnTypeScale, columnTypeOrdinal, columnTypeNominal] //these are set in the rootcontext in mainwindow!

								Rectangle
								{
									id:		columnTypeChangeIcon

									width:	headerRoot.__iconDim + (baseFontSize * 7 * ppiScale)
									height: headerRoot.__iconDim * 1.5
									radius: 15

									color:	popupIconMouseArea.useThisColor

									Item
									{
										id:						popupLabelIcon
										width:					(popupIconImage.width + popupText.width)
										height:					headerRoot.__iconDim
										anchors.left:			parent.left
										anchors.leftMargin:		10
										anchors.verticalCenter: parent.verticalCenter

										Image
										{
											id: popupIconImage

											source:					dataSetModel.getColumnTypesWithCorrespondingIcon()[iconRepeater.model[index]]
											width:					headerRoot.__iconDim
											height:					headerRoot.__iconDim
											sourceSize {	width:	width * 2
															height:	height * 2 }

											anchors.verticalCenter: parent.verticalCenter
										}

										Text
										{
											id:		popupText
											text:	iconRepeater.model[index] === columnTypeScale ?
														"Scale" : iconRepeater.model[index] === columnTypeOrdinal ?
															"Ordinal" : iconRepeater.model[index] === columnTypeNominal ?
																"Nominal" : "Text"

											anchors.left:		popupIconImage.right
											anchors.leftMargin: 10

											font:				dataTableView.font
										}
									}

									MouseArea
									{
										id:				popupIconMouseArea
										anchors.fill:	parent

										hoverEnabled:	true
										cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

										property color useThisColor: containsMouse ? "lightGray" : "transparent"

										onClicked:
										{
											var columnType = iconRepeater.model[index]
											popupIcons.close()
											colIcon.setColumnType(columnType)
										}
									}
								}
							}
						}
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

						cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

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



				gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
									GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}





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
						cursorShape:		containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

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
