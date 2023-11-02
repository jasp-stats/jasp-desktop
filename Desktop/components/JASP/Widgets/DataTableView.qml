import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models


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

		Rectangle
		{
			id:					changeModeNotifier
			anchors.centerIn:	parent
			height:				changeModeNotifierText.implicitHeight * 1.5
			width:				changeModeNotifierText.implicitWidth + 2 * jaspTheme.generalMenuMargin
			radius:				20
			color:				jaspTheme.grayDarker
			opacity:			changeModeNotifierTimer.running ? 0.8 : 0
			visible:			opacity > 0

			Behavior on opacity	{ enabled: preferencesModel.animationsOn; PropertyAnimation { duration: 100 } }

			Connections
			{
				target:				ribbonModel
				function onDataModeChanged()
				{
					if (ribbonModel.dataMode)
						changeModeNotifierTimer.restart()
				}
			}

			Timer
			{
				id:				changeModeNotifierTimer
				running:		false
				repeat:			false
				interval:		900
			}

			Text
			{
				id:					changeModeNotifierText
				color:				jaspTheme.white
				font.family: 		jaspTheme.fontLabel.family
				font.bold:			jaspTheme.fontLabel.bold
				font.pixelSize:		26
				anchors.centerIn:	parent
				text:				qsTr("Change to data editing mode")
				z:					2
			}
		}

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
			expandDataSet:			ribbonModel.dataMode

			doubleClickWorkaround:	!ribbonModel.dataMode
			//flickableInteractive:	!ribbonModel.dataMode
			onDoubleClicked:		__myRoot.doubleClicked()

			function showPopupMenu(fromItem, globalPos, rowIndex, columnIndex)
			{
				var ctrlCmd = MACOS ? qsTr("Cmd") : qsTr("Ctrl");
				var point   = Qt.point(columnIndex, rowIndex);
				var isVirtual	= dataTableView.view.isVirtual(point);
				var isColHeader = dataTableView.view.isColumnHeader(point);
				var isRowHeader = dataTableView.view.isRowHeader(point);
				var isCell		= dataTableView.view.isCell(point);

				var menuModel =
				[
					{ text: qsTr("Select All"),	shortcut: qsTr("%1+A").arg(ctrlCmd),											func: function() { dataTableView.view.selectAll() },	icon: "menu-select-all",	enabled: !isVirtual				},

					{ text: "---" },

					{ text: qsTr("Cut"),			shortcut: qsTr("%1+X").arg(ctrlCmd),										func: function() { dataTableView.view.cut(point) },		icon: "menu-data-cut",		enabled: !isVirtual				},
					{ text: qsTr("Copy"),			shortcut: qsTr("%1+C").arg(ctrlCmd),										func: function() { dataTableView.view.copy(point) },	icon: "menu-data-copy",		enabled: !isVirtual				},
					{ text: qsTr("Paste"),			shortcut: qsTr("%1+V").arg(ctrlCmd),										func: function() { dataTableView.view.paste(point)},	icon: "menu-data-paste"										},
					{ text: qsTr("Clear cells"),	shortcut: qsTr("Del"),														func: function() { dataTableView.view.cellsClear(); },	icon: "menu-cells-clear",	enabled: !isVirtual				},
					{ text: qsTr("Undo: %1").arg(dataTableView.view.undoText()),	shortcut: qsTr("%1+Z").arg(ctrlCmd),		func: function() { dataTableView.view.undo() },			icon: "menu-undo",			enabled: dataTableView.view.undoText() !== ""	},
					{ text: qsTr("Redo: %1").arg(dataTableView.view.redoText()),	shortcut: qsTr("%1+Shift+Z").arg(ctrlCmd),	func: function() { dataTableView.view.redo() },			icon: "menu-redo",			enabled: dataTableView.view.redoText() !== ""	},
				]

				if(!isVirtual && (isCell || isColHeader))
				{
					menuModel.push({ text: "---" });
					if (isCell)
						menuModel.push({ text: qsTr("Select column"),								func: function() { dataTableView.view.columnSelect(			columnIndex) },	icon: "menu-column-select"			})
					menuModel.push(
						{ text: qsTr("Insert column before"),										func: function() { dataTableView.view.columnInsertBefore(	columnIndex) },	icon: "menu-column-insert-before"	},
						{ text: qsTr("Insert column after"),										func: function() { dataTableView.view.columnInsertAfter(	columnIndex) },	icon: "menu-column-insert-after"	},
						{ text: qsTr("Delete column"),												func: function() { dataTableView.view.columnsDelete() },					icon: "menu-column-remove"			})

				 }

				if(!isVirtual && (isCell || isRowHeader))
				{
					menuModel.push({ text: "---" })
					if (isCell)
						menuModel.push({ text: qsTr("Select row"),									func: function() { dataTableView.view.rowSelect(			rowIndex) },	icon: "menu-row-select"				})
					menuModel.push(
						{ text: qsTr("Insert row above"),											func: function() { dataTableView.view.rowInsertBefore(		rowIndex) },	icon: "menu-row-insert-before"		},
						{ text: qsTr("Insert row below"),											func: function() { dataTableView.view.rowInsertAfter(		rowIndex) },	icon: "menu-row-insert-after"		},
						{ text: qsTr("Delete row"),													func: function() { dataTableView.view.rowsDelete();	},						icon: "menu-row-remove"				})
				}

				var menuText = []
				var menuShortcuts = []
				var menuFunctions = []
				var menuIcons = []
				var menuEnabled = []

				for (var i = 0; i < menuModel.length; i++)
				{
					var menu = menuModel[i]
					menuText.push(menu["text"])
					if (menu.hasOwnProperty("func"))
						menuFunctions.push(menu["func"])
					else
						menuFunctions.push(function() {})
					if (menu.hasOwnProperty("icon"))
						menuIcons.push(jaspTheme.iconPath + menu["icon"] + ".svg")
					else
						menuIcons.push("")
					if (menu.hasOwnProperty("shortcut"))
						menuShortcuts.push(menu["shortcut"])
					else
						menuShortcuts.push("")
					if (menu.hasOwnProperty("enabled"))
						menuEnabled.push(menu["enabled"])
					else
						menuEnabled.push(true)
				}

				var props = {
					"hasIcons": true,
					"model":		menuText,
					"functionCall": function (index)
					{
						menuFunctions[index]();

						customMenu.hide()
					},
					"icons": menuIcons,
					"shortcut": menuShortcuts,
					"enabled": menuEnabled
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

			Keys.onPressed: (event) =>
			{
				var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
				var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );

				switch(event.key)
				{
				case Qt.Key_Delete:
					event.accepted = true;
					dataTableView.view.cellsClear();
					break;

				case Qt.Key_A:
					if(controlPressed)
					{
						event.accepted = true;
						dataTableView.view.selectAll();
					}
					break;

				case Qt.Key_Home:
						event.accepted = true;
						mainWindowRoot.changeFocusToFileMenu();
						break;

				default:
					event.accepted = false;
					break;
				}
			}


			editDelegate:
				TextInput
				{
					id:						editItem
					text:					itemText
					color:					itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
					font:					jaspTheme.font
					verticalAlignment:		Text.AlignVCenter
					onEditingFinished:		dataTableView.view.commitEdit(rowIndex, columnIndex, text);
					z:						10
					readOnly:				!itemEditable

					onTextChanged:			isEditing = keyPressed // The text is changed when the edit item is made visible, so we have to wait that a key is pressed before setting the isEditing to true
					onVisibleChanged:		{ isEditing = false; keyPressed = false }
					property bool isEditing: false
					property bool keyPressed: false

					Component.onCompleted:	{ focusTimer.start(); }
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

					Keys.onPressed: (event) =>
					{
						keyPressed = true

						var rowI			= rowIndex
						var colI			= columnIndex
						var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier);
						var shiftPressed	= Boolean(event.modifiers & Qt.ShiftModifier  );
						var arrowPressed	= false;
						var arrowIndex;

						switch(event.key)
						{
						case Qt.Key_C:
							if(controlPressed)
							{
								theView.copy(Qt.point(colI, rowI));
								event.accepted = true;
							}
							break;

						case Qt.Key_X:
							if(controlPressed)
							{
								theView.cut(Qt.point(colI, rowI));
								event.accepted = true;
							}
							break;

						case Qt.Key_V:
							if(controlPressed)
							{
								theView.paste(Qt.point(colI, rowI));
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

						case Qt.Key_Z:
							if(controlPressed)
							{
								if (shiftPressed)
									theView.redo();
								else
									theView.undo();
							}
							break;

						case Qt.Key_Y:
							if(controlPressed && !shiftPressed)
								theView.redo();
								break;

						case Qt.Key_Home:	mainWindowRoot.changeFocusToFileMenu(); break;

						case Qt.Key_Up:		if(rowI > 0)										{ arrowPressed = true; arrowIndex   = Qt.point(colI, rowI - 1);		} break;
						case Qt.Key_Down:	if(rowI	< dataTableView.view.rowCount()    - 1)		{ arrowPressed = true; arrowIndex   = Qt.point(colI, rowI + 1);		} break;
						case Qt.Key_Left:	if(colI	> 0 && (editItem.cursorPosition <= 0 || !itemEditable))		{ arrowPressed = true; arrowIndex   = Qt.point(colI - 1, rowI);		} break;
						case Qt.Key_Right:	if(colI	< dataTableView.view.columnCount() - 1 &&
											   editItem.cursorPosition >= editItem.text.length)	{ arrowPressed = true; arrowIndex = Qt.point(colI + 1, rowI);		} break;
						case Qt.Key_Backtab: if(colI > 0)										{ arrowPressed = true; arrowIndex = Qt.point(colI - 1, rowI);	shiftPressed = false; } break;
						case Qt.Key_Tab:	 if(colI < dataTableView.view.columnCount() - 1)	{ arrowPressed = true; arrowIndex = Qt.point(colI + 1, rowI);		} break;

						case Qt.Key_Return:
						case Qt.Key_Enter:	if(rowI	< dataTableView.view.rowCount()    - 1)		{ arrowPressed = true; arrowIndex   = Qt.point(colI, rowI + 1);		} break;


						}

						if(arrowPressed)
						{
							if(!shiftPressed)
								dataTableView.view.selectionStart	= arrowIndex;
							else
								dataTableView.view.selectionEnd  = arrowIndex;

							dataTableView.view.edit(arrowIndex.y, arrowIndex.x);

							event.accepted = true;
						}

						if(!itemEditable)
							event.accepted = true; //textinput steals the focus otherwise and we cant move with arrows after pressing anything

					}


					Rectangle
					{
						id:					highlighter
						color:				editItem.isEditing ? "transparent" : jaspTheme.itemHighlight
						z:					-1
						visible:			ribbonModel.dataMode
						border.width:		2
						border.color:		jaspTheme.itemHighlight
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

							onPressed: (mouse) =>
							{
								if(mouse.buttons & Qt.RightButton)
									dataTableView.showPopupMenu(editItem, mapToGlobal(mouse.x, mouse.y), rowIndex, columnIndex);
							}
						}
					}
				}

			itemDelegate:
				Text
				{
					text:				itemText
					textFormat:			Text.RichText
					color:				itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
					font:				jaspTheme.font
					verticalAlignment:	Text.AlignVCenter

					MouseArea
					{
						z:					1234
						hoverEnabled:		true
						anchors.fill:		itemHighlight
						acceptedButtons:	Qt.LeftButton | Qt.RightButton

						onPressed:	(mouse) =>
						{
							if(ribbonModel.dataMode)
							{
								var shiftPressed = Boolean(mouse.modifiers & Qt.ShiftModifier)
								var rightPressed = Boolean(mouse.buttons & Qt.RightButton)
								var isSelected = dataTableView.view.isSelected(rowIndex, columnIndex)

								if(!shiftPressed)
								{
									if (!rightPressed || !isSelected)
										dataTableView.view.selectionStart = Qt.point(columnIndex, rowIndex)
								}
								else
									dataTableView.view.selectionEnd = Qt.point(columnIndex, rowIndex);

								if(rightPressed)
								{
									dataTableView.view.clearEdit()
									dataTableView.showPopupMenu(itemHighlight, mapToGlobal(mouse.x, mouse.y), rowIndex, columnIndex);
								}
								else if(!shiftPressed)
									dataTableView.view.edit(rowIndex, columnIndex)

							}
							else if (columnModel.visible)
							{
								columnModel.chosenColumn = columnIndex
							}
						}

						onPositionChanged:	(mouse) =>
						{
							if(ribbonModel.dataMode && Boolean(mouse.modifiers & Qt.ShiftModifier))
							{
								dataTableView.view.pollSelectScroll(rowIndex, columnIndex)
								dataTableView.view.selectionEnd = Qt.point(columnIndex, rowIndex)
							}
						}

					}

					Rectangle
					{
						id:				itemHighlight
						visible:		ribbonModel.dataMode && (dataTableView.selection && dataTableView.selection.hasSelection && dataTableView.view.isSelected(rowIndex, columnIndex))

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
				JaspControls.RectangularButton
				{
					id:				filterToggleButton
					width:			dataTableView.rowNumberWidth
					toolTip:		filterWindow.opened ? qsTr("Hide filter") : qsTr("Show filter")
					iconSource:		jaspTheme.iconPath + "filter.png"
					onClicked:		filterWindow.toggle()
					border.width:	0
				}

			extraColumnItem:
				JaspControls.RectangularButton
				{
					id:				addColumnButton
					width:			visible ? height : 0
					toolTip:		qsTr("Add computed column")
					iconSource:		jaspTheme.iconPath + "/addition-sign.svg"
					onClicked:		createComputeDialog.open()
					border.width:	0
					visible:		!dataTableView.expandDataSet
				}

			rowNumberDelegate:
				Rectangle
				{
					//gradient: Gradient{	GradientStop { position: 0.0;	color: "#EEEEEE" }	GradientStop { position: 0.75;	color: "#EEEEEE" }
					//					GradientStop { position: 0.77;	color: "#DDDDDD" }	GradientStop { position: 1.0;	color: "#DDDDDD" }	}
					color:	ribbonModel.dataMode && (dataTableView.view.selectionStart.y === rowIndex) ? jaspTheme.itemSelectedNoFocusColor : jaspTheme.uiBackground
					Text {
						text:				rowNumber
						font:				jaspTheme.font
						anchors.centerIn:	parent
						color:				virtual ? jaspTheme.textDisabled : jaspTheme.textEnabled
					}

					MouseArea
					{
						anchors.fill:		parent
						enabled:			ribbonModel.dataMode
						hoverEnabled:		true
						ToolTip.visible:	containsMouse
						ToolTip.text:		qsTr("Click here to select the row, hold shift for selecting multiple.")
						ToolTip.timeout:	3000
						ToolTip.delay:		500
						cursorShape:		Qt.PointingHandCursor
						acceptedButtons:	Qt.LeftButton | Qt.RightButton
						onClicked: 			(mouseEvent)=>
											{
												if(mouseEvent.button === Qt.LeftButton || mouseEvent.button === Qt.RightButton)
													dataTableView.view.rowSelect(rowIndex, mouseEvent.modifiers & Qt.ShiftModifier, mouseEvent.button === Qt.RightButton);
												if(mouseEvent.button === Qt.RightButton)
													dataTableView.showPopupMenu(parent, mapToGlobal(mouseEvent.x, mouseEvent.y), rowIndex, -1);
											}
					}

				}

			columnHeaderDelegate: Rectangle
			{
				id:		headerRoot
				color:	(!ribbonModel.dataMode && columnModel.chosenColumn === columnIndex && columnModel.visible) || (ribbonModel.dataMode && (dataTableView.view.selectionStart.x === columnIndex)) ? jaspTheme.itemSelectedNoFocusColor : jaspTheme.uiBackground

							property real	iconTextPadding:	10
				readonly	property int	__iconDim:			baseBlockDim * preferencesModel.uiScale

				Image
				{
					id:						colIcon
					anchors.verticalCenter: parent.verticalCenter
					anchors.left:			parent.left
					anchors.margins:		4


					source: String(dataSetModel.getColumnTypesWithIcons()[columnType]) === "" ? "" : jaspTheme.iconPath + dataSetModel.getColumnTypesWithIcons()[columnType]
					width:	headerRoot.__iconDim
					height: headerRoot.__iconDim

					sourceSize {	width:	width * 2
									height:	height * 2 }


					function setColumnType(newColumnType)
					{
						dataTableView.view.setColumnType(columnIndex, newColumnType)
					}


					MouseArea
					{
						enabled:			!virtual && computedColumnType != computedColumnTypeAnalysis
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
                        cursorShape:		enabled ? Qt.PointingHandCursor : Qt.ArrowCursor
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

				}

				Text
				{
					id:				headerTextItem

					text:			headerText
					font:			jaspTheme.font
					color:			jaspTheme.textEnabled
					textFormat:		Text.RichText

					horizontalAlignment:		Text.AlignHCenter

					anchors.horizontalCenter:	headerRoot.horizontalCenter
					anchors.verticalCenter:		headerRoot.verticalCenter
				}

				LoadingIndicator
				{
					id:			colIsInvalidated


					width:		columnIsInvalidated ? headerRoot.__iconDim : 0
					visible:	columnIsInvalidated && columnIsComputed

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

				}

				MouseArea
				{
					anchors.left:		colIcon.right
					anchors.top:		parent.top
					anchors.bottom:		parent.bottom
					anchors.right:		parent.right
					acceptedButtons:	Qt.LeftButton | Qt.RightButton
					onDoubleClicked:	(mouseEvent)=>
					{
						if(mouseEvent.button !== Qt.LeftButton)
							return;

						if (columnModel.chosenColumn === columnIndex && columnModel.visible)
							columnModel.visible = false;
						else
						{
							columnModel.chosenColumn	= columnIndex;
							columnModel.visible			= true;

							if(dataSetModel.columnUsedInEasyFilter(columnIndex))
							{
								filterWindow.showEasyFilter = true
								filterWindow.open()
							}
                            
							//A button in VariablesWindow will do this? in any case, it is kind of annoying to have the analysis always pop up instead of variableswindow...
							//if(computedColumnType == computedColumnTypeAnalysis || computedColumnType == computedColumnTypeAnalysisNotComputed)
							//    computedColumnsInterface.showAnalysisFormForColumn(headerText) //headerText should be columnName
                        }

					}

					onClicked:	(mouseEvent)=>
					{
						if(columnIndex >= 0)
						{
							if(mouseEvent.button === Qt.LeftButton || mouseEvent.button === Qt.RightButton)
								dataTableView.view.columnSelect(columnIndex, mouseEvent.modifiers & Qt.ShiftModifier, mouseEvent.button === Qt.RightButton)

							if(ribbonModel.dataMode && mouseEvent.button === Qt.RightButton)
								dataTableView.showPopupMenu(parent, mapToGlobal(mouseEvent.x, mouseEvent.y), -1, columnIndex)
						}
					}

					hoverEnabled:		true
					ToolTip.visible:	containsMouse
					ToolTip.text:		virtual ? qsTr("Add column")
											:   ("<b>" + columnTitle + "</b>"
                                                + (computedColumnType == computedColumnTypeAnalysis ? "<br>" + qsTr("Computed by an analysis") + "<br>": "")
												+ (columnDescription === "" ? "" : "<br><i>" + columnDescription + "</i>")
												+ "<br><br>"
												+ (!columnModel.visible	? qsTr("Doubleclick here to change variable settings")
																		: (columnModel.chosenColumn === columnIndex ? qsTr("Doubleclick here to close variable window")
																													: qsTr("Click here to change selected variable")
																		  )
												  )
											  )
					ToolTip.timeout:	3000
					ToolTip.delay:		500
					cursorShape:		Qt.PointingHandCursor
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

			height:			dataFilterStatusText.text.length > 0 ? dataFilterStatusText.contentHeight + (16 * preferencesModel.uiScale) : 0

			Text
			{
				id:						dataFilterStatusText
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
