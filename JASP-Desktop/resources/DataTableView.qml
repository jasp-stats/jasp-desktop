import QtQuick 2.9
import QtQuick.Controls 1.4 as Old
import QtQuick.Controls 2.3
import QtGraphicalEffects 1.0

FocusScope
{
    id: __myRoot

	Rectangle
	{
		color: "white"
		anchors.fill: parent
		z: -1

		JASPDataView
		{
			focus: __myRoot.focus

			id: dataTableView
			anchors.fill: parent
			//headersGradient: myHeadersGradient
			model: dataSetModel


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
					width: headerRoot.__iconDim
					height:  headerRoot.__iconDim


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
					}


					Popup {
						id: popupIcons; modal: true; focus: true;
						padding: 2 * 1
						y: colIcon.y + colIcon.height
						x: colIcon.x - (headerRoot.__iconDim * 0.5)

						closePolicy: Popup.CloseOnPressOutside | Popup.CloseOnEscape

						TextMetrics { id: nominalTextMeasure; text: "Nominal"}

						Column
						{
							width: parent.width
							spacing: popupIcons.padding / 2

							Repeater{
								id: iconRepeater
								model: [columnTypeScale, columnTypeOrdinal, columnTypeNominal]

								Old.Button
								{
									id: columnTypeChangeIcon
									iconSource: dataSetModel.getColumnTypesWithCorrespondingIcon()[iconRepeater.model[index]]

									width: headerRoot.__iconDim * 2.5 + nominalTextMeasure.width
									height: headerRoot.__iconDim * 1.2

									text: iconRepeater.model[index] === columnTypeScale ? "Scale" : ( iconRepeater.model[index] === columnTypeOrdinal ? "Ordinal" : "Nominal")


									onClicked: columnTypeChosen()

									function columnTypeChosen()
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



				Image
				{
					id: colFilterOn

					anchors.right: parent.right
					anchors.verticalCenter: parent.verticalCenter

					readonly property bool thisColumnIsFiltered: dataSetModel.columnsFilteredCount, dataSetModel.columnHasFilter(columnIndex) //extra field before comma is a hack to trigger reload on property binding

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

					}
				}
			}
		}
	}
}
