import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models


Rectangle
{
	color:	ribbonModel.dataMode && (dataTableView.view.selectionMin.y <= rowIndex && dataTableView.view.selectionMax.y >= rowIndex) 
			? jaspTheme.itemSelectedNoFocusColor 
			: jaspTheme.buttonColor
	Text
	{
		text:				rowNumber
		font:				jaspTheme.font
		anchors.centerIn:	parent
		color:				virtual ? jaspTheme.textDisabled : jaspTheme.textEnabled
	}

	MouseArea
	{
		anchors.fill:		parent
		//enabled:			ribbonModel.dataMode
		hoverEnabled:		true
		ToolTip.visible:	containsMouse
		ToolTip.text:		qsTr("Click here to select the row, hold shift for selecting multiple.")
		ToolTip.timeout:	3000
		ToolTip.delay:		500
		cursorShape:		Qt.PointingHandCursor
		acceptedButtons:	Qt.LeftButton | Qt.RightButton
		onClicked: 			(mouseEvent)=>
							{
								if(mouseEvent.button === Qt.LeftButton)
									dataTableView.view.select(rowIndex, -1, mouseEvent.modifiers & Qt.ShiftModifier, mouseEvent.modifiers & Qt.ControlModifier);
								
								if(mouseEvent.button === Qt.RightButton)
									dataTableView.showPopupMenu(parent, mapToGlobal(mouseEvent.x, mouseEvent.y), rowIndex, -1);
							}
		
		onPositionChanged:	(mouseEvent) =>
		{
			if(ribbonModel.dataMode && Boolean(mouseEvent.modifiers & Qt.ShiftModifier))
				dataTableView.view.selectHover(rowIndex, -1)
		}
	}
}
