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
