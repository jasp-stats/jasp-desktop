import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models
import QtQuick.Layouts

FocusScope
{
	id: __myRoot

	signal doubleClicked()
	
	property alias isMainDataViewer:		dataTableView.isMainDataViewer
	property real calculatedMinimumHeight:	200 * jaspTheme.uiScale + (!computeDataSetPanel.visible ? 0 : computeDataSetPanel.height + jaspTheme.generalAnchorMargin)

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
			anchors.top:			dataStatusBar.bottom
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			computeDataSetPanel.visible ? computeDataSetPanel.top : dataSelectionBar.top

			itemHorizontalPadding:	8 * jaspTheme.uiScale
			itemVerticalPadding:	8 * jaspTheme.uiScale

			model:					dataSetModel
			cacheItems:				true //!ribbonModel.dataMode
			maxColWidth:			250 * jaspTheme.uiScale
			expandDataSet:			ribbonModel.dataMode
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
					{ text: qsTr("Select All"),	shortcut: qsTr("%1+A").arg(ctrlCmd),											func: function() { dataTableView.view.selectAll() },			icon: "menu-select-all",	enabled: !isVirtual				},

					{ text: "---" },

					{ text: qsTr("Cut"),			shortcut: qsTr("%1+X").arg(ctrlCmd),										func: function() { dataTableView.view.cut(point) },				icon: "menu-data-cut",		enabled: !isVirtual				},
					{ text: qsTr("Copy"),			shortcut: qsTr("%1+C").arg(ctrlCmd),										func: function() { dataTableView.view.copy(point) },			icon: "menu-data-copy",		enabled: !isVirtual				},
					{ text: qsTr("Paste"),			shortcut: qsTr("%1+V").arg(ctrlCmd),										func: function() { dataTableView.view.paste(point)},			icon: "menu-data-paste"										},
					{ text: qsTr("Clear cells"),	shortcut: qsTr("Del"),														func: function() { dataTableView.view.cellsClear(); },			icon: "menu-cells-clear",	enabled: !isVirtual				},
					{ text: qsTr("Undo: %1").arg(dataTableView.view.undoText()),	shortcut: qsTr("%1+Z").arg(ctrlCmd),		func: function() { dataTableView.view.undo() },					icon: "menu-undo",			enabled: dataTableView.view.undoText() !== ""	},
					{ text: qsTr("Redo: %1").arg(dataTableView.view.redoText()),	shortcut: qsTr("%1+Shift+Z").arg(ctrlCmd),	func: function() { dataTableView.view.redo() },					icon: "menu-redo",			enabled: dataTableView.view.redoText() !== ""	},
				]
				
				
				if(!isVirtual && (isCell || isRowHeader))
				{
					menuModel.push({ text: "---" })
					if (isCell)
						menuModel.push({ text: qsTr("Select row"),									func: function() { dataTableView.view.rowSelect(			rowIndex) },					icon: "menu-row-select"				})
					menuModel.push(
						{ text: qsTr("Insert row above"),											func: function() { dataTableView.view.rowInsertBefore(		rowIndex) },					icon: "menu-row-insert-before"		},
						{ text: qsTr("Insert row below"),											func: function() { dataTableView.view.rowInsertAfter(		rowIndex) },					icon: "menu-row-insert-after"		},
						{ text: qsTr("Delete row"),													func: function() { dataTableView.view.rowsDelete(			rowIndex);},					icon: "menu-row-remove"				})
				}

				if(!isVirtual && (isCell || isColHeader))
				{
					menuModel.push({ text: "---" });
					if (isCell)
						menuModel.push({ text: qsTr("Select column"),								func: function() { dataTableView.view.columnSelect(			columnIndex) },					icon: "menu-column-select"			})
					menuModel.push(
						{ text: qsTr("Delete column"),												func: function() { dataTableView.view.columnsDelete(		columnIndex) },					icon: "menu-column-remove"			},
						{ text: qsTr("Insert column before"),										func: function() { dataTableView.view.columnInsertBefore(	columnIndex)				},	icon: "menu-column-insert-before"	},
						{ text: qsTr("Insert column after"),										func: function() { dataTableView.view.columnInsertAfter(	columnIndex)				},	icon: "menu-column-insert-after"	},
						{ text:	"---" },
						{ text: qsTr("Insert constructor column before"),							func: function() { dataTableView.view.columnInsertBefore(	columnIndex, true, false)	},	icon: "menu-column-insert-before"	},
						{ text: qsTr("Insert constructor column after"),							func: function() { dataTableView.view.columnInsertAfter(	columnIndex, true, false)	},	icon: "menu-column-insert-after"	},
						{ text: qsTr("Insert R column before"),										func: function() { dataTableView.view.columnInsertBefore(	columnIndex, true, true)	},	icon: "menu-column-insert-before"	},
						{ text: qsTr("Insert R column after"),										func: function() { dataTableView.view.columnInsertAfter(	columnIndex, true, true)	},	icon: "menu-column-insert-after"	},
						{ text:	"---" },
						{ text: qsTr("Reverse values"),												func: function() { dataTableView.view.columnReverseValues(	columnIndex)				},	icon: "menu-column-reverse-values"	},
						{ text: qsTr("Order labels by values"),										func: function() { dataTableView.view.columnautoSortByValues(	columnIndex)				},	icon: "menu-column-order-by-values"	}
						)

				 }

				var menuText		= []
				var menuShortcuts	= []
				var menuFunctions	= []
				var menuIcons		= []
				var menuEnabled		= []

				for (var i = 0; i < menuModel.length; i++)
				{
					var menu = menuModel[i]
					
					menuText		.push(!menu.hasOwnProperty("text")		? "???"			: menu["text"]								)
					menuFunctions	.push(!menu.hasOwnProperty("func")		? function() {} : menu["func"]								)
					menuIcons		.push(!menu.hasOwnProperty("icon")		? ""			: jaspTheme.iconPath + menu["icon"] + ".svg")
					menuShortcuts	.push(!menu.hasOwnProperty("shortcut")	? ""			: menu["shortcut"]							)
					menuEnabled		.push(!menu.hasOwnProperty("enabled")	? true			: menu["enabled"]							)
				}

				var props = {
					"hasIcons":		true,
					"model":		menuText,
					"icons":		menuIcons,
					"shortcut":		menuShortcuts,
					"enabled":		menuEnabled,
					"functionCall": function (index)
					{
						menuFunctions[index]();
						customMenu.hideMenus()
					}
				};

				var fromItemPos = fromItem.mapFromGlobal(globalPos.x, globalPos.y)

				customMenu.show(fromItem, props, fromItemPos.x, fromItemPos.y);
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
									
				case Qt.Key_C:
					if(controlPressed && dataTableView.view.selectionMin.x >= 0 && dataTableView.view.selectionMin.y >= 0)
					{
						dataTableView.view.copy();
						event.accepted = true;
					}
					break;
		
				case Qt.Key_X:
					if(controlPressed && dataTableView.view.selectionMin.x >= 0 && dataTableView.view.selectionMin.y >= 0)
					{
						dataTableView.view.cut();
						event.accepted = true;
					}
					break;
		
				case Qt.Key_V:
					if(controlPressed && dataTableView.view.selectionMin.x >= 0 && dataTableView.view.selectionMin.y >= 0)
					{
						dataTableView.view.paste();
						event.accepted = true;
					}
					break;

				case Qt.Key_Home:
						event.accepted = true;
						mainWindowRoot.changeFocusToFileMenu();
						break;
									
				
				case Qt.Key_PageUp:
					event.accepted = true;
					if(shiftPressed)	contentX = Math.max(0, contentX - width)
					else				contentY = Math.max(0, contentY - height)
					break;
								
				case Qt.Key_PageDown:
					event.accepted = true; 
					if(shiftPressed)	contentX = Math.min(contentWidth  - width,  contentX + width)
					else				contentY = Math.min(contentHeight - height, contentY + height)
					break;
				
				case Qt.Key_Down:
					event.accepted = true;
					budgeDown();
					break;
				
				case Qt.Key_Up:
					event.accepted = true;
					budgeUp();
					break;
				
				case Qt.Key_Left:
					event.accepted = true;
					budgeLeft();
					break;
				
				case Qt.Key_Right:
					event.accepted = true;
					budgeRight();
					break;
									
				default:
					event.accepted = false;
					break;
				}
			}


			editDelegate:			DataTableViewEdit {}
			itemDelegate:			DataTableViewItem {}
			rowNumberDelegate:		DataTableViewRowHeader {}
			columnHeaderDelegate:	DataTableViewColumnHeader {}

			leftTopCornerItem:
				Item
				{
					width:			dataTableView.rowNumberWidth
					
					JaspControls.RectangularButton
					{
						id:				filterToggleButton
						
						toolTip:		filterModel.filterVisible ? qsTr("Hide filter") : qsTr("Show filter")
						iconSource:		jaspTheme.iconPath + "filter.png"
						onClicked:		filterModel.filterVisible = !filterModel.filterVisible
						border.width:	1
						
						anchors
						{
							fill:		parent
							margins:	-1
						}
					}
				}

			extraColumnItem:
				JaspControls.RectangularButton
				{
					id:				addColumnButton
					x:				2.5
					y:				-1
					width:			visible ? height + 2 : 0
					toolTip:		qsTr("Add computed column")
					iconSource:		jaspTheme.iconPath + "/addition-sign.svg"
					onClicked:		createComputeDialog.open()
					border.width:	1
					visible:		!dataTableView.expandDataSet
				}


		}

		
		Rectangle
		{
			id:				dataStatusBar			
			color:			jaspTheme.grayMuchLighter
			border.color:	jaspTheme.grayLighter
			border.width:	1
			
			anchors.left:	parent.left
			anchors.right:	parent.right
			anchors.top:	parent.top
			
			height:			rowLayoutStatusBar.height + jaspTheme.generalAnchorMargin * 2
			
		
			Flickable
			{
				
				
				contentWidth:		rowLayoutStatusBar.width
				contentHeight:		rowLayoutStatusBar.height
	
				anchors.fill:		parent
				anchors.margins:	jaspTheme.generalAnchorMargin
				
				
				RowLayout
				{
					id:				rowLayoutStatusBar
					height:			24 * jaspTheme.uiScale
					
					//JaspControls.DropDown
					Text
					{
						id:					filterDropDown
						text:				qsTr("Showing: ") + filterModel.currentFilterTitle
						font:				jaspTheme.font
						color:				jaspTheme.textEnabled						
					}
					
					Repeater
					{
						model: dataSetModel.currentTypeIcons
						
						JaspControls.RoundedButton
						{
							
							iconSource:			jaspTheme.currentIconPath() + modelData
							
							implicitWidth:		rowLayoutStatusBar.height
							implicitHeight:		rowLayoutStatusBar.height
							
							onClicked:			dataSetModel.toggleColType(index, false)
							onDoubleClicked:	dataSetModel.toggleColType(index, true)
						}
					}
					
					TextField
					{
						id:						columnFilterInput
						text:					""
						placeholderText:		qsTr("Columnfilter")
						font:					jaspTheme.font
						color:					jaspTheme.textEnabled
						onTextChanged:			dataSetModel.columnFilter = text
						height:					rowLayoutStatusBar.height
						padding:				0
						
						background:				Rectangle
						{
							color:				jaspTheme.controlBackgroundColor
							border.color:		jaspTheme.buttonBorderColor
							border.width:		1
							radius:				jaspTheme.borderRadius
							implicitWidth:		200 * jaspTheme.uiScale
							implicitHeight:		rowLayoutStatusBar.height	
						}
					}
					
	
					//Text
					//{
					//	id:						dataFilterStatusText
					//	text:					filterModel.filter.statusBarText
					//	font:					jaspTheme.font
					//	color:					jaspTheme.textEnabled
					//}
					
				}
			}
		}
		
		Rectangle
		{
			id:				dataSelectionBar			
			color:			jaspTheme.uiBackground
			border.color:	jaspTheme.grayLighter
			border.width:	1
			
			anchors.left:	parent.left
			anchors.right:	parent.right
			anchors.bottom:	parent.bottom
			
			height:			dataTabButtons.height + jaspTheme.generalAnchorMargin * 2
			//z:				-1
			
			Rectangle
			{
				anchors
				{
					top:			parent.top
					left:			parent.left
					right:			parent.right
					bottom:			dataTabButtonsFlickable.top
					topMargin:		border.width * -1
					bottomMargin:	-1
				}
				
				//z:				1
				
				color:			jaspTheme.white
				border.color:	jaspTheme.uiBorder
				border.width:	1
				
			}
			
		
			Flickable
			{
				id:					dataTabButtonsFlickable
				clip:				true
				contentWidth:		dataTabButtons.width
				contentHeight:		dataTabButtons.height
					
	
				anchors
				{
					top:		parent.top
					left:		parent.left
					right:		parent.right
					bottom:		parent.bottom
					topMargin:	jaspTheme.generalAnchorMargin
				}
				
				RowLayout
				{
					id:		dataTabButtons
					
					Repeater
					{
						model:			dataSetPackage.workspace
						
						DataSetTabButton
						{
							required property string	name
							required property string	title
							required property string	description
							required property bool		columnIsComputed
							
							dataSetName:		name
							description:		description
							text:				title
							isComputed:			columnIsComputed
							//buttonActive:		dataSetPackage.workspace.shownDataSet && dataSetPackage.workspace.shownDataSet.name === name
							//showTextField:		buttonActive
							onClicked:			dataSetPackage.workspace.setShownDataSet(name)
						}
					
					}
					
					JaspControls.MenuButton
					{
						id:						addDataSetButtonEasy
						iconSource:				jaspTheme.iconPath + "/round_addition.png"
						onClicked:				mainWindow.addNewDataSet()
						radius:					height
					}
				}
			}
		}
		
		ComputeDataSetPanel
		{
			id:						computeDataSetPanel
			anchors.left:			parent.left
			anchors.right:			parent.right
			anchors.bottom:			dataSelectionBar.top
			visible:				dataSetPackage.workspace.shownDataSet && dataSetPackage.workspace.shownDataSet.codeType === computedColumnTypeRCode
		}
	}
}
