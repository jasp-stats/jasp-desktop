import QtQuick
import QtQuick.Controls
import JASP.Controls	as JaspControls
import JASP
import QtQuick.Layouts

Item
{
	id:					root
	implicitHeight:		theButton.implicitHeight
	implicitWidth:		theButton.width
	height:				implicitHeight
	width:				implicitWidth
	
	property alias	text:			theButton.text
	property alias	iconSource:		theButton.iconSource
	property bool	buttonActive:	false
	property bool	showTextField:	false
	property alias	theButton:		theButton
	property bool	doSeparator:	false
	property bool	hideButtoness:	false
	property color	hideButtonCol:	jaspTheme.textDisabled
	
	signal clicked();
	signal doubleClicked();
	
	Loader
	{
		id:					textFieldLoader
		sourceComponent:	doSeparator ? addFilterButton : showTextField ? textField : undefined
		anchors.centerIn:	theButton
		z:					10
		
	}
	
	JaspControls.RoundedButton
	{
		id:			theButton
		height:		implicitHeight + offset
		width:		doSeparator ? height : showTextField ? Math.max(implicitWidth, textFieldLoader.width + jaspTheme.generalAnchorMargin) : implicitWidth
	//	visible:	!doSeparator
		
		border.width:	hideButtoness ? 0 : 1
		
		property real offset: 4 * jaspTheme.uiScale
		
		onClicked:			parent.clicked()
		onDoubleClicked:	parent.doubleClicked()
		color:				buttonActive ? jaspTheme.white : jaspTheme.buttonColor
		enabled:			!hideButtoness && !buttonActive && !showTextField
		textColor:			hideButtoness ? hideButtonCol : !showTextField ? jaspTheme.textEnabled : theButton.color;
		
		Rectangle
		{
			id:				disabledTabButtonLineBottom
			color:			parent.border.color
			height:			1
			visible:		!buttonActive
			anchors
			{
				left:			parent.left
				right:			parent.right
				top:			parent.top
				topMargin:		parent.implicitHeight
			}
		}
	}
	
	Component
	{
		id:	textField
		
		RowLayout
		{
			
			TextInput
			{
				id:		textInput
				font:	jaspTheme.font
				color:	jaspTheme.textEnabled
				text:	theButton.text
				
				onEditingFinished:		filterModel.renameCurrentFilter(text)

			}
			
			JaspControls.MenuButton
			{
				id:					deleteButton
				height:				textInput.height
				width:				height
				radius:				height
				iconSource:			jaspTheme.iconPath + "close-button.png"
				onClicked:			filterModel.deleteCurrentFilter()
				toolTip:			qsTr("Delete filter")
			}
		}
	}
	
	Component
	{
		id:	addFilterButton
		
			
		JaspControls.MenuButton
		{
			id:					addButton
			width:				height
			radius:				height
			iconSource:			jaspTheme.iconPath + "/round_addition.png"
			onClicked:			filterModel.addFilter(labelText)
			toolTip:			qsTr("Add filter")
		}
	}
}
