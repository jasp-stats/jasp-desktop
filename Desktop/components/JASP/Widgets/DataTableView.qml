import QtQuick				2.15
import QtQuick.Controls		2.15
import QtQml.Models			2.15


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
			focus:					__myRoot.focus

			id:						dataTableView
			anchors.top:			parent.top
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			dataStatusBar.top

			itemHorizontalPadding:	8 * jaspTheme.uiScale
			itemVerticalPadding:	8 * jaspTheme.uiScale

			model:					dataSetModel
			cacheItems:				!ribbonModel.dataMode

			doubleClickWorkaround:	!ribbonModel.dataMode
			//flickableInteractive:	!ribbonModel.dataMode
			onDoubleClicked:		__myRoot.doubleClicked()

			function showCopyPasteMenu(fromItem, globalPos, indexClicked)
			{
				console.log("showCopyPasteMenu!")

				view.contextMenuClickedAtIndex(indexClicked);

				var ctrlCmd = MACOS ? qsTr("Cmd") : qsTr("Ctrl");

				var copyPasteMenuModel =
				[
							qsTr("Select All"),

							"---",

							qsTr("Cut             (%1+X)").arg(ctrlCmd),
							qsTr("Copy            (%1+C)").arg(ctrlCmd),
							qsTr("Paste           (%1+V)").arg(ctrlCmd),

							"---", //Works but is ugly:  + qsTr("Including header:"),

							qsTr("Header cut      (%1+Shift+X)").arg(ctrlCmd),
							qsTr("Header copy     (%1+Shift+C)").arg(ctrlCmd),
							qsTr("Header paste    (%1+Shift+V)").arg(ctrlCmd),

							"---",

							qsTr("Select column"),
							qsTr("Insert column before"),
							qsTr("Insert column after"),
							qsTr("Delete column"),

							"---",

							qsTr("Select row"),
							qsTr("Insert row before"),
							qsTr("Insert row after"),
							qsTr("Delete row"),

				]

				var copyPasteMenuFunctions =
				[
						function() { dataTableView.view.selectAll() },

						function(){},

						function() { dataTableView.view.cut(  false)},
						function() { dataTableView.view.copy( false)},
						function() { dataTableView.view.paste(false)},

						function (){},

						function() { dataTableView.view.cut(  true )},
						function() { dataTableView.view.copy( true )},
						function() { dataTableView.view.paste(true )},

						function (){},

						function() { dataTableView.view.columnSelect(		indexClicked.column) },
						function() { dataTableView.view.columnInsertBefore(	indexClicked.column) },
						function() { dataTableView.view.columnInsertAfter(	indexClicked.column) },
						function() { dataTableView.view.columnDelete(		indexClicked.column) },

						function (){},

						function() { dataTableView.view.rowSelect(			indexClicked.row) },
						function() { dataTableView.view.rowInsertBefore(	indexClicked.row) },
						function() { dataTableView.view.rowInsertAfter(		indexClicked.row) },
						function() { dataTableView.view.rowDelete(			indexClicked.row) }

				]

				var props = {
					"model":		copyPasteMenuModel,
					"functionCall": function (index)
					{
						var chosenElement = copyPasteMenuModel[index];

					//	console.log("Option " + chosenElement + " chosen, running function.");

						copyPasteMenuFunctions[index]();

						customMenu.hide()
					}
				};

				//customMenu.scrollOri.x	= __JASPDataViewRoot.contentX;
				//customMenu.scrollOri.y	= 0;

				var fromItemPos = fromItem.mapFromGlobal(globalPos.x, globalPos.y)

				customMenu.show(fromItem, props, fromItemPos.x, fromItemPos.y);

				//customMenu.menuScroll.x	= Qt.binding(function() { return -1 * (__JASPDataViewRoot.contentX - customMenu.scrollOri.x); });
				//customMenu.menuScroll.y	= 0;
				//customMenu.menuMinIsMin	= true
				//customMenu.menuMaxPos.x	= __JASPDataViewRoot.width + __JASPDataViewRoot.x
			}

			editDelegate:
				TextInput
				{
					id:						editItem
					text:					itemText
					color:					itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
					font:					jaspTheme.font
					verticalAlignment:		Text.AlignVCenter
					onEditingFinished:		finishEdit();
					z:						10


					Component.onCompleted:	focusTimer.start();
					Timer
					{
						id:					focusTimer
						interval:			10
						repeat:				false
						onTriggered:
						{
							editItem.forceActiveFocus()
							dataTableView.moveItemIntoView(editItem);
						}
					}

					property bool alreadyFinished:	false

					Connections
					{
						target:					ribbonModel
						onFinishCurrentEdit:	finishEdit();
					}

					function finishEdit()
					{
						if(!alreadyFinished)
							dataTableView.view.editFinished(index, text);
						alreadyFinished = true;
					}

					Keys.onPressed:
					{
						var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
						var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );
						var arrowPressed	= false;
						var arrowIndex;

						switch(event.key)
						{
						case Qt.Key_C:
							if(controlPressed)
							{
								theView.copy(shiftPressed);
								event.accepted = true;
							}
							break;

						case Qt.Key_X:
							if(controlPressed)
							{
								theView.cut(shiftPressed);
								event.accepted = true;
							}
							break;

						case Qt.Key_V:
							if(controlPressed)
							{
								theView.paste(shiftPressed);
								event.accepted = true;
							}
							break;

						case Qt.Key_A:
							if(controlPressed)
							{
								theView.selectAll();
								event.accepted = true;
							}
							break;

						case Qt.Key_Home:	mainWindowRoot.changeFocusToFileMenu(); break;

						case Qt.Key_Up:		if(rowIndex		> 0)								{ arrowPressed = true; arrowIndex   = dataSetModel.index(rowIndex - 1,	columnIndex);		} break;
						case Qt.Key_Down:	if(rowIndex		< dataSetModel.rowCount()    - 1)	{ arrowPressed = true; arrowIndex   = dataSetModel.index(rowIndex + 1,	columnIndex);		} break;
						case Qt.Key_Left:	if(columnIndex	> 0)								{ arrowPressed = true; arrowIndex   = dataSetModel.index(rowIndex,		columnIndex - 1);	} break;
						case Qt.Key_Right:	if(columnIndex	< dataSetModel.columnCount() - 1)	{ arrowPressed = true; arrowIndex   = dataSetModel.index(rowIndex,		columnIndex + 1);	} break;
						}

						if(arrowPressed)
						{
							finishEdit();

							if(!shiftPressed)
								dataTableView.view.selectionStart	= arrowIndex;
							else
							{
								dataTableView.view.selectionEnd  = arrowIndex;
								dataTableView.view.edit(arrowIndex);
							}

							event.accepted = true;
						}

					}

					Rectangle
					{
						id:					highlighter
						color:				jaspTheme.itemHighlight
						z:					-1
						visible:			ribbonModel.dataMode
						anchors
						{
							fill:			 parent
							topMargin:		-dataTableView.itemVerticalPadding
							leftMargin:		-dataTableView.itemHorizontalPadding
							rightMargin:	-dataTableView.itemHorizontalPadding
							bottomMargin:	-dataTableView.itemVerticalPadding
						}

						MouseArea
						{
							z:					1234
							anchors.fill:		parent
							acceptedButtons:	Qt.RightButton

							onPressed:
								if(mouse.buttons & Qt.RightButton)
								{
									finishEdit()
									dataTableView.showCopyPasteMenu(editItem, mapToGlobal(mouse.x, mouse.y), dataSetModel.index(rowIndex, columnIndex));
								}
						}
					}
				}

			itemDelegate:
				Text
				{
					text:				itemText
					color:				itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
					font:				jaspTheme.font
					verticalAlignment:	Text.AlignVCenter
					
					MouseArea
					{
						z:					1234
						hoverEnabled:		true
						anchors.fill:		itemHighlight
						acceptedButtons:	Qt.LeftButton | Qt.RightButton
						
						onPressed:			
							if(ribbonModel.dataMode)
							{
								var shiftPressed = Boolean(mouse.modifiers & Qt.ShiftModifier);

								if(Boolean(mouse.buttons & Qt.RightButton))
								{
									forceActiveFocus();
									dataTableView.showCopyPasteMenu(itemHighlight, mapToGlobal(mouse.x, mouse.y), dataSetModel.index(rowIndex, columnIndex));
								}
								else
								{
									if(!shiftPressed)	dataTableView.view.selectionStart   = dataTableView.view.model.index(rowIndex, columnIndex);
									else				dataTableView.view.selectionEnd		= dataTableView.view.model.index(rowIndex, columnIndex);

								
									forceActiveFocus();
								}
							}
											
						onPositionChanged:	if(ribbonModel.dataMode && Boolean(mouse.modifiers & Qt.ShiftModifier))
											{
												var idx = dataTableView.view.model.index(rowIndex, columnIndex)
												dataTableView.view.pollSelectScroll(idx)
												dataTableView.view.selectionEnd = idx
											}

					}
					
					Rectangle
					{
						id:				itemHighlight
						visible:		ribbonModel.dataMode && (dataTableView.selection.hasSelection, dataTableView.selection.isSelected(dataTableView.view.model.index(rowIndex, columnIndex)))
						
						color:			jaspTheme.itemHighlight
						opacity:		1.0
						z:				-1

						anchors
						{
							fill:			 parent
							topMargin:		-dataTableView.itemVerticalPadding
							leftMargin:		-dataTableView.itemHorizontalPadding
							rightMargin:	-dataTableView.itemHorizontalPadding
							bottomMargin:	-dataTableView.itemVerticalPadding
						}
					}
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

					source: jaspTheme.iconPath + dataSetModel.getColumnTypesWithCorrespondingIcon()[columnType]
					width:	headerRoot.__iconDim
					height: headerRoot.__iconDim

					sourceSize {	width:	width * 2
									height:	height * 2 }


					function setColumnType(newColumnType)
					{
						dataSetModel.setColumnTypeFromQML(columnIndex, newColumnType)

						if(labelModel.chosenColumn === columnIndex && columnType === columnTypeScale)
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


					width:		columnIsInvalidated ? headerRoot.__iconDim : 0
					visible:	columnIsInvalidated

					anchors.right:			colFilterOn.left
					anchors.verticalCenter:	parent.verticalCenter
					anchors.margins:		visible ? 1 : 0
				}

				Image
				{
					id:						colFilterOn

					width:					columnIsFiltered ? headerRoot.__iconDim : 0
					height:					headerRoot.__iconDim

					source:					jaspTheme.iconPath + "filter.png"
					sourceSize {	width:	headerRoot.__iconDim * 2
									height:	headerRoot.__iconDim * 2 }

					anchors.right:			colHasError.left
					anchors.margins:		columnIsFiltered ? 1 : 0
					anchors.verticalCenter:	parent.verticalCenter
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

					anchors.right:			parent.right
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

				MouseArea
				{
					anchors.left:	colIsComputed.right
					anchors.top:	parent.top
					anchors.bottom: parent.bottom
					anchors.right:	colHasError.left

					onClicked:
						if(columnIndex >= 0)
						{

							if(columnType  !== columnTypeScale)
							{
								var changedIndex		= labelModel.chosenColumn	!== columnIndex
								labelModel.chosenColumn	= columnIndex;
								labelModel.visible		= changedIndex ? true : !labelModel.visible;
							}
							else
								dataSetModel.renameColumnDialog(columnIndex);

							if(dataSetModel.columnUsedInEasyFilter(columnIndex))
							{
								filterWindow.showEasyFilter = true
								filterWindow.open()
							}
						}

					hoverEnabled:		true
					ToolTip.visible:	containsMouse && (columnType !== columnTypeScale)
					ToolTip.text:		qsTr("Click here to change labels") + (columnIsFiltered ? qsTr(" or inspect filter") : "" )
					ToolTip.timeout:	3000
					ToolTip.delay:		500
					cursorShape:		(columnType !== columnTypeScale) || dataSetModel.columnUsedInEasyFilter(columnIndex) ? Qt.PointingHandCursor : Qt.ArrowCursor
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
