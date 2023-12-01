import QtQuick
import QtQuick.Window
import QtQuick.Controls
//import QtWebEngine
import JASP.Widgets
import JASP.Controls as JC


Window
{
	id:				aboutWindow

	width:			1200 * preferencesModel.uiScale
	height:			600 * preferencesModel.uiScale

	minimumWidth:	Math.min(500, 500 * preferencesModel.uiScale)
	minimumHeight:	Math.min(500, 500 * preferencesModel.uiScale)

	default property alias	content:			contentInfo.children
			property string labelcolor:			"#F99800"
			property string closebuttoncolor:	jaspTheme.blue

	visible:				false
	title:					qsTr("About JASP")

	color:			jaspTheme.white

	signal closeModel();

	Shortcut { onActivated: closeModel();	sequences: ["Ctrl+Q", "Ctrl+W", Qt.Key_Close]; }

	Connections
	{
		target:			mainWindow
		function onCloseWindows() { closeModel(); }
	}

	Image
	{
		id:				topWave
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	parent.right
		height:			parent.height/4

		source:			jaspTheme.iconPath + "jasp-wave-down-blue-120.svg"
		z:				10
	}

	Item
	{
		id:					jaspLogo
		z:					15
		width:				parent.height / 3
		height:				parent.height / 5.5
		anchors.centerIn:	topWave
		
		Image
		{
			anchors.fill:		parent
			fillMode:			Image.PreserveAspectFit
			source:				jaspTheme.iconPath + "jasp-logo.svg"
			sourceSize.width:	width  * 2
			sourceSize.height:	height * 2
			mipmap:				true
		}
	}

	Rectangle
	{
		id:							aboutInfoBox
		z:							5

		anchors
		{
			top:			topWave.bottom
			left:			parent.left
			right:			parent.right
			leftMargin:		15 * jaspTheme.uiScale
			rightMargin:	x
            bottom:         bottomWave.top
		}
        
        color:				jaspTheme.white
        
		Flickable
        {
			id:						contentFlicker
			anchors
			{
				top:				parent.top
				left:				parent.left
				right:				scrollingOnTheRight.left
				bottom:				parent.bottom
			}
            
			contentWidth:			width
			contentHeight:			contentInfo.childrenRect.height

            Column
            {
                id:				contentInfo
                spacing:		5 * preferencesModel.uiScale
                width:          contentFlicker.width
    
                //This is where the subitems go!
            }
		}
		
		JC.JASPScrollBar
		{
			id:				scrollingOnTheRight
			flickable:		contentFlicker
			manualAnchor:	true
			
			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
			}
		}
		

	}

	Rectangle
	{
		id:					closeButton
		z:					12
		x:					aboutWindow.width / 2 - parent.x -width / 2
		anchors.centerIn:	bottomWave
		color:				closebuttoncolor
		radius:				5 * preferencesModel.uiScale
		height:				25 * preferencesModel.uiScale
		width:				75 * preferencesModel.uiScale

		JC.Text
		{
			anchors.centerIn:		parent
			text:					qsTr("Close")
			color:					jaspTheme.white
			horizontalAlignment:	Text.AlignHCenter
			verticalAlignment:		Text.AlignVCenter
		}

		MouseArea
		{
			onClicked:				(event) => { closeModel() }
			anchors.fill:			parent
			cursorShape:			Qt.PointingHandCursor
			focus:					true
			Keys.onEnterPressed:	(event) => { closeModel() }
			Keys.onReturnPressed:	(event) => { closeModel() }
			Keys.onEscapePressed:	(event) => { closeModel() }
		}
	}

	Image
	{
		id:					bottomWave

		anchors.bottom:		parent.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		height:				parent.height/4
		source:				jaspTheme.iconPath + "jasp-wave-up-green-120.svg"
		z:					10
	}
}

