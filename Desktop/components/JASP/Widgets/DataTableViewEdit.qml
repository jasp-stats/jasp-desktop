import QtQuick
import QtQuick.Controls
import JASP.Controls		as JaspControls
import QtQml.Models

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
				if(editItem.selectedText == editItem.text)
					theView.selectAll();
				else
					editItem.selectAll()
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
