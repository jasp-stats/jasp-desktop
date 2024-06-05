import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

Item
{
	id:			itemDelegateItem
	
	Item
	{
		anchors.fill:	parent
		
		Text
		{
			id:						itemDelegateText
			text:					itemText
			textFormat:				Text.PlainText
			color:					itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
			font:					jaspTheme.font
			verticalAlignment:		Text.AlignVCenter
			elide:					Text.ElideRight
			anchors
			{
				top:				parent.top
				left:				parent.left
				right:				itemDelegateLabel.visible ? itemDelegateLabel.left : parent.right
				bottom:				parent.bottom
			}
		}
		
		TextMetrics
		{
			id:		itemDelegateLabelMetrics
			font:	itemDelegateLabel.font
			text:	itemShadowText === undefined ? "" : itemShadowText
		}
		
		Text
		{
			id:						itemDelegateLabel
			text:					itemShadowText ? itemShadowText : ""
			textFormat:				Text.PlainText
			color:					jaspTheme.textDisabled
			font:					jaspTheme.font
			verticalAlignment:		Text.AlignVCenter
			horizontalAlignment:	Text.AlignRight
			elide:					Text.ElideLeft
			visible:				itemShadowText !== undefined && itemText !== undefined && itemText !== itemShadowText
			anchors
			{
				top:				parent.top
				left:				itemDelegateLabelMetrics.width < itemDelegateItem.width/2 ? undefined : parent.horizontalCenter
				right:				parent.right
				bottom:				parent.bottom
			}
		}
	}

	JASPMouseAreaToolTipped
	{
		z:					1234
		hoverEnabled:		true
		anchors.fill:		itemHighlight
		acceptedButtons:	Qt.LeftButton | Qt.RightButton
		
		toolTipText:		itemShadowText !== undefined && itemText !== undefined && itemText !== itemShadowText ? "%1 %2".arg(itemText).arg(itemShadowText) : itemText
		toolTipTimeOut:		10000

		onPressed:	(mouse) =>
		{
			if(ribbonModel.dataMode)
			{
				var shiftPressed	= Boolean(mouse.modifiers & Qt.ShiftModifier)
				var controlPressed	= Boolean(mouse.modifiers & Qt.ControlModifier)
				var rightPressed	= Boolean(mouse.buttons & Qt.RightButton)
				var isSelected		= dataTableView.view.isSelected(rowIndex, columnIndex)
					
				if(!rightPressed)
					dataTableView.view.select(rowIndex, columnIndex, shiftPressed, controlPressed);
				else
				{
					dataTableView.view.clearEdit()
					dataTableView.showPopupMenu(itemHighlight, mapToGlobal(mouse.x, mouse.y), rowIndex, columnIndex);
				}
			}
			
			if (columnModel.visible)
				columnModel.chosenColumn = columnIndex
		}

		onPositionChanged:	(mouse) =>
		{
			if(ribbonModel.dataMode && Boolean(mouse.modifiers & Qt.ShiftModifier))
				dataTableView.view.selectHover(rowIndex, columnIndex)
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
