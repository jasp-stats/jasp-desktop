import QtQuick 2.9
import QtQuick.Controls 1.4 as Old
import QtQuick.Controls 2.3
import QtGraphicalEffects 1.0

FocusScope
{
    id: __myRoot

	signal doubleClicked()



	Rectangle
	{
		color: "white"
		anchors.fill: parent
		z: -1

		JASPDataView
		{
			focus: __myRoot.focus

			id: dataTableView
			anchors.top: parent.top
			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: dataStatusBar.top

			//headersGradient: myHeadersGradient
			model: dataSetModel

			onDoubleClicked: __myRoot.doubleClicked()


			leftTopCornerItem:
				Rectangle
				{
					id: filterToggleButton
					gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
										GradientStop { position: 1.0;	color: "#DDDDDD" }	GradientStop { position: 0.77;	color: "#DDDDDD" }	}

					ToolTip.text: filterWindow.opened ? "Hide filter" : "Show filter"
					ToolTip.timeout: 4500
					ToolTip.delay: 500
					ToolTip.visible: filterToggleButtonMouseArea.containsMouse

					Image
					{
						source: "../images/filter.png"
						anchors.top: parent.top
						anchors.bottom: parent.bottom
						anchors.horizontalCenter: parent.horizontalCenter

						anchors.margins: 4

						sourceSize.width: width
						sourceSize.height: height
						width: height

					}

					MouseArea
					{
						id: filterToggleButtonMouseArea
						anchors.fill: parent
						onClicked: filterWindow.toggle()
						hoverEnabled: true
					}
				}

			rowNumberDelegate:
				Rectangle
				{
					gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
										GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}
					Text { text: rowIndex; anchors.centerIn: parent }
				}

			columnHeaderDelegate: Rectangle
			{
				id: headerRoot
				property real iconTextPadding: 10
				readonly property int __iconDim: 16

				Text
				{
					id: headerTextItem

					text: headerText

					horizontalAlignment: Text.AlignHCenter

					anchors.horizontalCenter: headerRoot.horizontalCenter
					anchors.verticalCenter: headerRoot.verticalCenter
				}



				gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
									GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}

				Image
				{
					id: colIcon
					anchors.verticalCenter: parent.verticalCenter
					anchors.left: parent.left
					anchors.margins: 4

					function myColumnType() {return dataSetModel.columnIcon(columnIndex)}

					source: dataSetModel.getColumnTypesWithCorrespondingIcon()[myColumnType()]
					width:	headerRoot.__iconDim
					height: headerRoot.__iconDim


					function setColumnType(columnType)
					{
						//colIcon.myColumnType =
						dataSetModel.setColumnTypeFromQML(columnIndex, columnType)

						if(variablesWindow.chosenColumn === columnIndex && colIcon.myColumnType() === columnTypeScale)
							variablesWindow.chooseColumn(-1)


					}

					MouseArea
					{
						anchors.fill: parent
						onClicked: if(columnIndex > -1) popupIcons.open()

						hoverEnabled: true
						ToolTip.visible: containsMouse
						ToolTip.text: "Click here to change columntype"
						ToolTip.timeout: 3000
						ToolTip.delay: 500

					}


					Popup {
						id: popupIcons; modal: true; focus: true;
						padding: 8
						spacing: 4
						y: colIcon.y + colIcon.height
						x: colIcon.x - (headerRoot.__iconDim * 0.5)

						closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

						TextMetrics { id: nominalTextMeasure; text: "Nominal"}

						Column
						{
							//width: parent.width
							spacing: popupIcons.padding / 2

							Repeater{
								id: iconRepeater
								model: [columnTypeScale, columnTypeOrdinal, columnTypeNominal]

								Rectangle
								{
									id: columnTypeChangeIcon

									width: 90
									height: headerRoot.__iconDim * 1.5
									radius: 15

									color: popupIconMouseArea.useThisColor

									Item
									{
										width: (popupIconImage.width + popupText.width)
										height: headerRoot.__iconDim
										anchors.verticalCenter: parent.verticalCenter
										anchors.left: parent.left
										anchors.leftMargin: 10

										Image
										{
											id: popupIconImage

											anchors.verticalCenter: parent.verticalCenter

											source: dataSetModel.getColumnTypesWithCorrespondingIcon()[iconRepeater.model[index]]
											width:	headerRoot.__iconDim
											height: headerRoot.__iconDim
											sourceSize.width:	width
											sourceSize.height:	height
										}

										Text
										{
											id: popupText
											text: iconRepeater.model[index] === columnTypeScale ? "Scale" : ( iconRepeater.model[index] === columnTypeOrdinal ? "Ordinal" : "Nominal")

											anchors.left: popupIconImage.right
											anchors.leftMargin: 10
										}
									}

									MouseArea
									{
										id: popupIconMouseArea
										anchors.fill: parent

										hoverEnabled: true
										cursorShape: containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor

										property color useThisColor: containsMouse ? "lightGray" : "transparent"

										onClicked:
										{
											var columnType = iconRepeater.model[index]
											popupIcons.close()
											colIcon.setColumnType(columnType)
											filterWindow.sendFilter()

										}
									}
								}
							}
						}
					}
				}



				Image
				{
					id: colFilterOn

					anchors.right: parent.right
					anchors.verticalCenter: parent.verticalCenter

					readonly property bool thisColumnIsFiltered: dataSetModel.columnsFilteredCount, dataSetModel.columnHasFilter(columnIndex) || dataSetModel.columnUsedInEasyFilter(columnIndex) //extra field before comma is a hack to trigger reload on property binding

					anchors.margins: thisColumnIsFiltered ? 1 : 0

					source: "../images/filter.png"
					width: thisColumnIsFiltered ? headerRoot.__iconDim : 0
					height:  headerRoot.__iconDim

				}

				MouseArea
				{
					anchors.left: colIcon.right
					anchors.top: parent.top
					anchors.bottom: parent.bottom
					anchors.right: parent.right
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

					hoverEnabled: true
					ToolTip.visible: containsMouse && dataSetModel.columnIcon(columnIndex)  !== columnTypeScale
					ToolTip.text: "Click here to change labels"
					ToolTip.timeout: 3000
					ToolTip.delay: 500
				}
			}
		}

		Rectangle
		{
			id: dataStatusBar
			objectName: "dataStatusBar"
			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom

			color: "#EEEEEE"
			border.color: "lightGrey"
			border.width: 1

			height: datafiltertatusText.text.length > 0 ? datafiltertatusText.contentHeight + 16 : 0

			function setText(newText) { datafiltertatusText.text = newText }

			Text
			{
				id: datafiltertatusText
				text: ""
				anchors.left: parent.left
				anchors.verticalCenter: parent.verticalCenter
				anchors.leftMargin: 8
			}
		}

	}
}
