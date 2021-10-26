import QtQuick				2.15
import QtQuick.Controls		1.4 as Old
import QtQuick.Controls		2.15
import QtQml.Models			2.15
import QtQuick.Layouts 		1.15
import QtGraphicalEffects	1.0


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
				RoundedButton
				{
					id:				filterToggleButton
					width:			dataTableView.rowNumberWidth
					toolTip:		filterWindow.opened ? qsTr("Hide filter") : qsTr("Show filter")
					iconSource:		jaspTheme.iconPath + "filter.png"
					onClicked:		filterWindow.toggle()
					border.width:	0
				}

			extraColumnItem:
				RoundedButton
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

			/// So, this is the layout, it's like before but with a bit more structure. I have tried to
			///	make sure that the header title always appears in the center, and I think I got it, but
			/// I feel sometimes it is slightly off.
			///
			/// RowLayout:
			/// 
			/// +-------------------------------------------------------------------------------------+
			/// | +---------------------------------+ +-------------------------------+ +-----------+ |
			/// | | +-------+  +-------+  +-------+ | | +-------------------++-------+| | +-------+ | |
			/// | | |       |  |       |  |       | | | |                   ||       || | |       | | |
			/// | | |  Typ. |  |  Com. |  |  Fil. | | | |  Header Title     ||  Err. || | |  Ren. | | |
			/// | | |       |  |       |  |       | | | |                   ||       || | |       | | |
			/// | | +-------+  +-------+  +-------+ | | +-------------------++-------+| | +-------+ | |
			/// | +---------------------------------+ +-------------------------------+ +-----------+ |
			/// +-------------------------------------------------------------------------------------+
			columnHeaderDelegate: Rectangle
			{
				id:		headerRoot
				color:	jaspTheme.uiBackground

				implicitWidth: 	columnHeaderLayout.implicitWidth
				implicitHeight: columnHeaderLayout.implicitHeight

				RowLayout {
					id: columnHeaderLayout
					
					spacing: 				6
					anchors.fill: 			parent
					Layout.fillWidth: 	 	true

					anchors.leftMargin:  	jaspTheme.generalAnchorMargin
					anchors.rightMargin: 	jaspTheme.generalAnchorMargin

					RowLayout {
						id: iconsLayout

						spacing: 2

						Layout.alignment:			Qt.AlignLeft || Qt.AlignVCenter

						Image
						{
							id:							colIcon
							Layout.preferredHeight: 	0.6 * headerRoot.height
							Layout.preferredWidth: 		0.6 * headerRoot.height
							Layout.alignment:			Qt.AlignHCenter
							fillMode: 					Image.PreserveAspectFit

							function myColumnType() {return dataSetModel.columnIcon(columnIndex)}

							source: jaspTheme.iconPath + dataSetModel.getColumnTypesWithCorrespondingIcon()[myColumnType()]

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
										// FIXME: Talk to me, what's wrong with you!
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
							id:							colIsComputed
							Layout.preferredHeight: 	0.6 * headerRoot.height
							Layout.preferredWidth: 		0.6 * headerRoot.height
							Layout.alignment:			Qt.AlignHCenter
							fillMode: 					Image.PreserveAspectFit

							visible:					columnIsComputed

							source:				jaspTheme.iconPath + "/computed.png"

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

						Image
						{
							id:							colFilterOn
							Layout.preferredHeight: 	0.6 * headerRoot.height
							Layout.preferredWidth: 		0.6 * headerRoot.height
							Layout.alignment:			Qt.AlignHCenter
							fillMode: 					Image.PreserveAspectFit

							visible:					columnIsFiltered

							source:						jaspTheme.iconPath + "filter.png"

							MouseArea
							{
								anchors.fill: parent

								hoverEnabled:		true
								ToolTip.visible:	containsMouse
								ToolTip.text:		qsTr("Click here to change the columns filter")
								ToolTip.timeout:	3000
								ToolTip.delay:		500

								cursorShape:		Qt.PointingHandCursor

								onClicked: 
								{
									if(dataSetModel.columnUsedInEasyFilter(columnIndex))
									{
										filterWindow.showEasyFilter = true
										filterWindow.open()
									}
								}
							}
						}
					
					}

					RowLayout {
						id: 						titleLayout

						Layout.alignment:			Qt.AlignHCenter || Qt.AlignVCenter

						Text
						{
							id:				headerTextItem

							text:			headerText
							font:			jaspTheme.font
							color:			jaspTheme.textEnabled

							MouseArea
							{
								id: changeTypeMouseArea
								anchors.fill: parent

								onClicked:  
								{
									if(dataSetModel.columnIcon(columnIndex)  !== columnTypeScale)
									{
										var changedIndex		= labelModel.chosenColumn	!== columnIndex
										labelModel.chosenColumn	= columnIndex;
										labelModel.visible		= changedIndex ? true : !labelModel.visible;
									}
								}

								hoverEnabled:		true
								ToolTip.visible:	containsMouse
								ToolTip.text:		qsTr("Click here to change labels") + (columnIsFiltered ? qsTr(" or inspect filter") : "" )
								ToolTip.timeout:	3000
								ToolTip.delay:		500
								cursorShape:		dataSetModel.columnIcon(columnIndex)  !== columnTypeScale || dataSetModel.columnUsedInEasyFilter(columnIndex) ? Qt.PointingHandCursor : Qt.ArrowCursor
							}
						}

						Image
						{
							id:							colHasError
							Layout.preferredHeight: 	0.6 * headerRoot.height
							Layout.preferredWidth: 		0.6 * headerRoot.height
							Layout.alignment:			Qt.AlignLeft
							fillMode: 					Image.PreserveAspectFit

							visible:					columnError.length > 0 // && !columnIsInvalidated

							source:						jaspTheme.iconPath + "/error.png"

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

						LoadingIndicator
						{
							id:			colIsInvalidated

							width:		headerRoot.__iconDim
							visible:	columnIsInvalidated
						}

					}

					RowLayout {

						id:							renameLayout
						Layout.alignment:			Qt.AlignRight || Qt.AlignVCenter
						Layout.preferredHeight: 	0.6 * headerRoot.height
						Layout.preferredWidth: 		0.6 * headerRoot.height

						Rectangle {
							id: 						renameButtonContainer
							Layout.preferredHeight: 	0.6 * headerRoot.height
							Layout.preferredWidth: 		0.6 * headerRoot.height

							color: 						"transparent"

							Image
							{
								id:							colMenuImage
								anchors.fill: 				parent
								fillMode: 					Image.PreserveAspectFit
								opacity: 					0.0

								source:						jaspTheme.iconPath + "/edit-pencil.png"

								MouseArea
								{
									id: renameMenuMouseArea
									anchors.fill: parent

									onClicked:  dataSetModel.renameColumnDialog(columnIndex);

									// Unfortunately, I couldn't get the `MouseArea::containsMouse` to work; so, I had to do this.
									onEntered: {
										colMenuImage.opacity = 1.0;
									}

									onExited: {
										colMenuImage.opacity = 0.0;
									}

									// TODO: I sill need to implement this part. The idea is that this functionality will move into 
									// a menu containing a few things, e.g., rename, change label, delete, ...
									//
									// if(dataSetModel.columnIcon(columnIndex)  !== columnTypeScale)
									// {
									// 	var changedIndex		= labelModel.chosenColumn	!== columnIndex
									// 	labelModel.chosenColumn	= columnIndex;
									// 	labelModel.visible		= changedIndex ? true : !labelModel.visible;
									// }
									// else
									// ToolTip.text:		qsTr("Click here to change labels") + (columnIsFiltered ? qsTr(" or inspect filter") : "" )

									hoverEnabled:		true
									ToolTip.visible:	containsMouse
									ToolTip.text:		qsTr("Click here to rename the column")
									ToolTip.timeout:	3000
									ToolTip.delay:		500
									cursorShape:		dataSetModel.columnIcon(columnIndex)  !== columnTypeScale || dataSetModel.columnUsedInEasyFilter(columnIndex) ? Qt.PointingHandCursor : Qt.ArrowCursor
								}
							}
						}

					}

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
