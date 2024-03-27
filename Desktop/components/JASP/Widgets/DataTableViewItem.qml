import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

Item
{
	id:			itemDelegateItem
	
	
	Text
	{
		id:					itemDelegateText
		text:				itemText
		textFormat:			Text.RichText
		color:				itemActive ? jaspTheme.textEnabled : jaspTheme.textDisabled
		font:				jaspTheme.font
		verticalAlignment:	Text.AlignVCenter
		width:				Math.min(contentWidth, itemDelegateItem.width)
		anchors
		{
			top:			parent.top
			left:			parent.left
			bottom:			parent.bottom
		}
	}
	
	Text
	{
		id:						itemDelegateLabel
		text:					itemLabel ? itemLabel : ""
		textFormat:				Text.RichText
		color:					jaspTheme.textDisabled
		font:					jaspTheme.font
		verticalAlignment:		Text.AlignVCenter
		horizontalAlignment:	Text.AlignRight
		elide:					Text.ElideRight
		visible:				itemLabel !== undefined && itemText !== undefined && itemText !== itemLabel
		width:					Math.max(0, parent.width - itemDelegateText.width)
		anchors
		{
			top:				parent.top
			right:				parent.right
			bottom:				parent.bottom
		}
	}

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
					if (!rightPressed && !isSelected)
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
