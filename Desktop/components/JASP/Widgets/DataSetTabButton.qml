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
	property string	dataSetName:	""
	property string	description:	""
	property bool	buttonActive:	dataSetPackage && dataSetPackage.workspace && dataSetPackage.workspace.shownDataSet && dataSetName === dataSetPackage.workspace.shownDataSet.name
	property bool	showTextField:	buttonActive
	property alias	theButton:		theButton
	property bool   isComputed:		false;

	signal clicked();
	signal doubleClicked();
	
	Loader
	{
		id:					textFieldLoader
		sourceComponent:	showTextField ? textField : undefined
		anchors.centerIn:	theButton
		z:					10
		
	}
	
	JaspControls.RoundedButton
	{
		id:			theButton
		height:		implicitHeight + offset
		width:		showTextField ? Math.max(implicitWidth, textFieldLoader.width + jaspTheme.generalAnchorMargin) : implicitWidth
		
		property real offset: 4 * jaspTheme.uiScale
		
		y:		-1 * offset + 1
		
		onClicked:			parent.clicked()
		onDoubleClicked:	parent.doubleClicked()
		color:				buttonActive ? jaspTheme.white : jaspTheme.buttonColor
		enabled:			!buttonActive && !showTextField
		textColor:			!showTextField ? jaspTheme.textEnabled : theButton.color;
		
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
				bottom:			parent.bottom
				bottomMargin:	parent.implicitHeight
			}
		}
		
		toolTip:		description
	}
	
	Component
	{
		id:	textField
		
		RowLayout
		{
			JaspControls.CheckBox
			{
				id:					computedToggle
				checked:			root.isComputed
				onCheckedChanged:	dataSetPackage.workspace.setDataSetComputed(root.dataSetName, checked)

				ToolTip.text:		qsTr("Computed dataset")
				ToolTip.visible:	hovered
			}
			
			TextInput
			{
				id:		textInput
				font:	jaspTheme.font
				color:	jaspTheme.textEnabled
				text:	theButton.text
				
				onEditingFinished:	dataSetPackage.workspace.shownDataSet.title = text
			}
			
			JaspControls.MenuButton
			{
				id:					deleteButton
				height:				textInput.height
				width:				height
				radius:				height
				iconSource:			jaspTheme.iconPath + "close-button.png"
				onClicked:
				{
					if (messages.showYesNoQML(
						qsTr("Delete dataset"),
						qsTr("Are you sure you want to delete the dataset '%1'? This cannot be undone.").arg(root.dataSetName),
						qsTr("Delete"),
						qsTr("Cancel")))
						dataSetPackage.workspace.deleteShownDataSet()
				}
				toolTip:			qsTr("Delete dataset")
			}
			
			
		}
	}
}
