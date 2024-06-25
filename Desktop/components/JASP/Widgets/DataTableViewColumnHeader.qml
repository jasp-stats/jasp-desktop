import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

Rectangle
{
	id:			headerRoot
	color:		(!ribbonModel.dataMode || dataTableView.view.selectionMin.x == -1
					? columnModel.chosenColumn === columnIndex && columnModel.visible 
					: (dataTableView.view.selectionMin.x <= columnIndex && dataTableView.view.selectionMax.x >= columnIndex)
				) 
				? jaspTheme.itemSelectedNoFocusColor 
				: jaspTheme.buttonColor

	readonly	property int	__iconDim:			baseBlockDim * preferencesModel.uiScale

	function getColumnTypeIcon(type)
	{
		return String(dataSetModel.getColumnTypesWithIcons()[type]) === "" ? "" : jaspTheme.iconPath + dataSetModel.getColumnTypesWithIcons()[type]
	}


	Keys.onPressed: (event) =>
	{
		var controlPressed	= Boolean(event.modifiers & Qt.ControlModifier)

		if (controlPressed)
		{
			switch(event.key)
			{
			case Qt.Key_C:
				theView.copy(Qt.point(columnIndex, -1));
				event.accepted = true;
				break;
			case Qt.Key_X:
				theView.cut(Qt.point(columnIndex, -1));
				event.accepted = true;
				break;
			case Qt.Key_V:
				theView.paste(Qt.point(columnIndex, -1));
				event.accepted = true;
				break;
			}
		}
	}

	Image
	{
		id:						colIcon
		anchors.verticalCenter: parent.verticalCenter
		anchors.left:			parent.left
		anchors.margins:		4 * jaspTheme.uiScale


		source:					getColumnTypeIcon(columnType)
		width:					source == "" ? 0 : headerRoot.__iconDim
		height:					headerRoot.__iconDim

		sourceSize {	width:	width * 2
						height:	height * 2 }


		function setColumnType(newColumnType)
		{
			dataTableView.view.setColumnType(columnIndex, newColumnType)
		}


		MouseArea
		{
			enabled:			!virtual && computedColumnType !== computedColumnTypeAnalysis
			anchors.fill:		parent
			onClicked:
			{
				var functionCall      = function (index)
				{
					colIcon.setColumnType(columnTypesModel.getType(index));
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


	Text
	{
		id:							headerTextItem

		text:						headerText
		font:						jaspTheme.font
		color:						jaspTheme.textEnabled
		textFormat:					Text.Normal
		elide:						Text.ElideRight
		horizontalAlignment:		Text.AlignLeft

		anchors.left:				colIcon.right
		anchors.leftMargin:			colIcon.anchors.margins
		anchors.verticalCenter:		headerRoot.verticalCenter
		anchors.right:				colIsComputed.left
	}
	
	DataTableViewColumnHeaderComputedColumnInfo
	{
		id:						colIsComputed

		anchors.right:			colFilterOn.left
		anchors.verticalCenter: parent.verticalCenter
		anchors.margins:		columnIsComputed ? 2 * jaspTheme.uiScale : 0
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
		anchors.margins:		columnIsFiltered ? 2 * jaspTheme.uiScale : 0
		anchors.verticalCenter:	parent.verticalCenter
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
				headerRoot.forceActiveFocus()

				if(mouseEvent.button === Qt.LeftButton)
				{
					dataTableView.view.select(-1, columnIndex, mouseEvent.modifiers & Qt.ShiftModifier, mouseEvent.modifiers & Qt.ControlModifier)
					columnModel.chosenColumn = columnIndex;
						
				}

				if(mouseEvent.button === Qt.RightButton)
				{
					ribbonModel.dataMode = true
					dataTableView.showPopupMenu(parent, mapToGlobal(mouseEvent.x, mouseEvent.y), -1, columnIndex)
				}
			}
		}
		
		onPositionChanged:	(mouseEvent) =>
		{
			if(ribbonModel.dataMode && Boolean(mouseEvent.modifiers & Qt.ShiftModifier))
				dataTableView.view.selectHover(-1, columnIndex)

		}

		hoverEnabled:		true
		ToolTip.visible:	containsMouse
		ToolTip.text:		virtual ? qsTr("Add column")
								:   ("<b>" + columnTitle + "</b>"
									+ (computedColumnType === computedColumnTypeAnalysis ? "<br>" + qsTr("Computed by an analysis") + "<br>": "")
									+ (columnDescription === "" ? "" : "<br><i>" + columnDescription + "</i>")
									+ "<br><br>"
									+ (!columnModel.visible	? qsTr("Doubleclick here to change variable settings")
															: (columnModel.chosenColumn === columnIndex ? qsTr("Doubleclick here to close variable window")
																										: qsTr("Click here to change selected variable")
															  )
									  )
									+ (columnError == "" ? "" : "<br><br>" + qsTr("Computed column has error:<br>") + "<code'>" + columnError.replace("\n","<br>") + "</code>")
								  )
		ToolTip.timeout:	30000
		ToolTip.delay:		500
		cursorShape:		Qt.PointingHandCursor
	}
}
