import QtQuick
import QtQuick.Window
import QtQuick.Controls
//import QtWebEngine
import JASP.Widgets
import JASP.Controls as JC


Window
{
	id:				aboutWindow

	width:			1000 * preferencesModel.uiScale
	height:			550 * preferencesModel.uiScale

	minimumWidth:	850
	minimumHeight:	550

	default property alias	content:			contentInfo.children
			property string labelcolor:			"#F99800"
			property string closebuttoncolor:	"#50B0E3"

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

		source:		jaspTheme.iconPath + "jasp-wave-down-blue-120.svg"
	}

	Item
	{
		id:				jaspLogo

		width:			parent.height / 3
		height:			parent.height / 5.5
		anchors
		{
			left:		parent.left
			leftMargin: 50 * preferencesModel.uiScale
			top:		aboutInfoBox.top
			topMargin:	25 * preferencesModel.uiScale
		}

		Image
		{
			anchors.fill:		parent
			fillMode:			Image.PreserveAspectFit
			source:				jaspTheme.iconPath + "jasp-logo-black.svg"
			sourceSize.width:	width  * 2
			sourceSize.height:	height * 2
			mipmap:				true
		}
	}

	Rectangle
	{
		id:							aboutInfoBox

		anchors
		{
			top:			topWave.bottom
			left:			jaspLogo.right
			right:			parent.right
			leftMargin:		25
			rightMargin:	15
		}
		height:			contentInfo.height
		color:			jaspTheme.white

		Column
		{
			id:				contentInfo
			anchors.left:	parent.left
			anchors.top:	parent.top
			anchors.right:	parent.right
			spacing:		5 * preferencesModel.uiScale

			//This is where the subitems go!
		}
	}

	Rectangle
	{
		id:					closeButton
		x:					aboutWindow.width / 2 - parent.x -width / 2
		anchors.top:		aboutInfoBox.bottom
		anchors.topMargin:	10 * preferencesModel.uiScale
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
		z:					-1

		anchors.bottom:		parent.bottom
		anchors.left:		parent.left
		anchors.right:		parent.right
		height:				parent.height/4
		source:				jaspTheme.iconPath + "jasp-wave-up-green-120.svg"
	}
}

