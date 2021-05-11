import QtQuick			2.15
import QtQuick.Controls 2.15

Rectangle
{
	property alias	text:		textInput.text
	property alias	textInput:	textInput
	property var	nextEl:		null
	
	signal onTextChanged();
	
	id:					root
	height:				textInput.implicitHeight + jaspTheme.generalAnchorMargin * 2
	width:				textInput.implicitWidth  + jaspTheme.generalAnchorMargin * 2

	color:				enabled ? jaspTheme.controlBackgroundColor : jaspTheme.controlDisabledBackgroundColor
	border.color:		jaspTheme.buttonBorderColor
	border.width:		1
	onFocusChanged:		if(focus) textInput.focus = true;

	TextInput
	{
		id:					textInput
		text:				"..."
		clip:				true
		font:				jaspTheme.font
		onTextChanged:		root.onTextChanged()
		color:				enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		KeyNavigation.tab:	root.nextEl
		KeyNavigation.down:	root.nextEl
		selectByMouse:		true
		selectedTextColor:	jaspTheme.white
		selectionColor:		jaspTheme.itemSelectedColor


		anchors
		{
			left:			parent.left
			right:			parent.right
			verticalCenter:	parent.verticalCenter
			margins:		jaspTheme.generalAnchorMargin
		}
	}
}
