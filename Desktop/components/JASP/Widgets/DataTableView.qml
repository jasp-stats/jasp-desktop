import QtQuick				2.9
//import QtQuick.Controls		1.4 as Old
import QtQuick.Controls		2.2


FocusScope
{
	id: __myRoot

	signal doubleClicked()

	Rectangle
	{
		color:			jaspTheme.white
		anchors.fill:	parent
		z:				-1
		border.width:	1
		border.color:	jaspTheme.uiBorder

		JASPDataView
		{
			focus:				__myRoot.focus

			id:					dataTableView
			anchors.top:		parent.top
			anchors.left:		parent.left
			anchors.right:		parent.right
			anchors.bottom:		dataStatusBar.top

			itemHorizontalPadding:	8 * jaspTheme.uiScale
			itemVerticalPadding:	8 * jaspTheme.uiScale

			model:				dataSetModel
			onDoubleClicked:	__myRoot.doubleClicked()

			itemDelegate:
				Text
				{
					text:				itemText
					color:				itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
					font:				jaspTheme.font
					verticalAlignment:	Text.AlignVCenter
				}

			leftTopCornerItem:
				RectangularButton
				{
					id:				filterToggleButton
					width:			dataTableView.rowNumberWidth
					toolTip:		filterWindow.opened ? qsTr("Hide filter") : qsTr("Show filter")
					iconSource:		jaspTheme.iconPath + "filter.png"
					onClicked:		filterWindow.toggle()
					border.width:	0
				}

			extraColumnItem:
				RectangularButton
				{
					id:				addColumnButton
					width:			height
					toolTip:		qsTr("Add computed column")
					iconSource:		jaspTheme.iconPath + "/addition-sign.svg"
					onClicked:		createComputeDialog.open()
					border.width:	0
				}

			rowNumberDelegate:
				Rectangle
				{
					//gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
					//					GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}
					color:	jaspTheme.uiBackground
					Text {
						text:				rowIndex + 1
						font:				jaspTheme.font
						anchors.centerIn:	parent
						color:				jaspTheme.textEnabled
					}

				}

			columnHeaderDelegate: Rectangle
			{
				id:		headerRoot
				color:	jaspTheme.uiBackground

							property real	iconTextPadding:	10
				readonly	property int	__iconDim:			baseBlockDim * preferencesModel.uiScale

				Image
				{
					id:						colIcon
					anchors.verticalCenter: parent.verticalCenter
					anchors.left:			parent.left
					anchors.margins:		4

					function myColumnType() {return dataSetModel.columnIcon(columnIndex)}

					source: jaspTheme.iconPath + dataSetModel.getColumnTypesWithCorrespondingIcon()[myColumnType()]
					width:	headerRoot.__iconDim
					height: headerRoot.__iconDim

					sourceSize {	width:	width * 2
									height:	height * 2 }


					function setColumnType(columnType)
					{
						dataSetModel.setColumnTypeFromQML(columnIndex, columnType)

						if(labelModel.chosenColumn === columnIndex && colIcon.myColumnType() === columnTypeScale)
							labelModel.visible = false;
					}


					MouseArea
					{
						anchors.fill:		parent
						onClicked:
						{
							var functionCall      = function (index)
							{
								// FIXME:
								var columnType = [columnTypeScale, columnTypeOrdinal, columnTypeNominal][index];

								if (columnType !== undefined)
									colIcon.setColumnType(columnType);

								customMenu.hide()
							}

							var props = {
								"model":		columnTypesModel,
								"functionCall": functionCall
							};

							customMenu.scrollOri.x	= dataTableView.contentX;
							customMenu.scrollOri.y	= 0;

							customMenu.toggle(dataTableView, props, headerRoot.x - contentX, headerRoot.y + headerRoot.height - dataTableView.contentY);

							customMenu.menuScroll.x	= Qt.binding(function() { return -1 * (dataTableView.contentX - customMenu.scrollOri.x); });
							customMenu.menuScroll.y	= 0;
							customMenu.menuMinIsMin	= true
							customMenu.menuMaxPos.x	= dataTableView.width + dataTableView.x
						}

						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		qsTr("Click here to change column type")
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

					source:				jaspTheme.iconPath + "/computed.png"
					sourceSize {	width:	headerRoot.__iconDim * 2
									height:	headerRoot.__iconDim * 2 }

					MouseArea
					{
						anchors.fill:	parent
						onClicked:		computeColumnWindow.open(dataSetModel.headerData(columnIndex, Qt.Horizontal))

						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		qsTr("Click here to change the columns formulas")
						ToolTip.timeout:	3000
						ToolTip.delay:		500

						cursorShape:		Qt.PointingHandCursor

					}
				}

				Text
				{
					id:				headerTextItem

					text:			headerText
					font:			jaspTheme.font
					color:			jaspTheme.textEnabled

					horizontalAlignment:		Text.AlignHCenter

					anchors.horizontalCenter:	headerRoot.horizontalCenter
					anchors.verticalCenter:		headerRoot.verticalCenter
				}

				LoadingIndicator
				{
					id:			colIsInvalidated

					width:		headerRoot.__iconDim
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

					source:					jaspTheme.iconPath + "/error.png"
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

					source:					jaspTheme.iconPath + "filter.png"
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
						if(columnIndex >= 0)
						{

							if(dataSetModel.columnIcon(columnIndex)  !== columnTypeScale)
							{
								var changedIndex		= labelModel.chosenColumn	!== columnIndex
								labelModel.chosenColumn	= columnIndex;
								labelModel.visible		= changedIndex ? true : !labelModel.visible;
							}

							if(dataSetModel.columnUsedInEasyFilter(columnIndex))
							{
								filterWindow.showEasyFilter = true
								filterWindow.open()
							}
						}

					hoverEnabled:		true
					ToolTip.visible:	containsMouse && dataSetModel.columnIcon(columnIndex)  !== columnTypeScale
					ToolTip.text:		qsTr("Click here to change labels") + (columnIsFiltered ? qsTr(" or inspect filter") : "" )
					ToolTip.timeout:	3000
					ToolTip.delay:		500
					cursorShape:		dataSetModel.columnIcon(columnIndex)  !== columnTypeScale || dataSetModel.columnUsedInEasyFilter(columnIndex) ? Qt.PointingHandCursor : Qt.ArrowCursor
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

			color:			jaspTheme.grayMuchLighter
			border.color:	jaspTheme.grayLighter
			border.width:	1

			height:			datafiltertatusText.text.length > 0 ? datafiltertatusText.contentHeight + (16 * preferencesModel.uiScale) : 0

			Text
			{
				id:						datafiltertatusText
				text:					filterModel.statusBarText
				font:					jaspTheme.font
				color:					jaspTheme.textEnabled
				anchors.left:			parent.left
				anchors.verticalCenter:	parent.verticalCenter
				anchors.leftMargin:		8 * preferencesModel.uiScale
			}
		}
	}
}
