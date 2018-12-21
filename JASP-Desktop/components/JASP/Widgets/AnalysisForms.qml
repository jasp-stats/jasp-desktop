import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Widgets 1.0
import JASP.Theme 1.0

Rectangle
{
	z:				0
	color:			Theme.uiBackground
	border.color:	Theme.uiBorder
	border.width:	1

	property int extraSpaceRight: openCloseButton.width

	Item
	{
		id:				dropShadow
		x:				1 - width
		z:				-1
		height:			parent.height
		width:			Theme.shadowRadius


		Rectangle
		{
			anchors.centerIn: parent
			rotation:	90
			gradient:	Gradient {
				GradientStop { position: 0.0; color: Theme.shadow }
				GradientStop { position: 1.0; color: "transparent" } }
			height:		dropShadow.width
			width:		dropShadow.height
		}
	}

	Rectangle
	{
		id:				openCloseButton
		width:			20 * ppiScale
		color:			Theme.uiBackground
		border.color:	Theme.uiBorder
		border.width:	1
		anchors
		{
			top:	parent.top
			left:	parent.left
			bottom:	parent.bottom
		}

		Image
		{

			readonly property string iconsFolder:	"qrc:/images/"
			readonly property string expandedIcon:	"arrow-right.png"
			readonly property string contractedIcon: "arrow-left.png"

			source:				iconsFolder + (mainWindow.analysesVisible ? expandedIcon : contractedIcon)
			width:				parent.width - 4
			height:				width
			sourceSize.width:	width * 2;
			sourceSize.height:	height * 2;

			anchors.centerIn:	parent
		}

		MouseArea
		{
			anchors.fill:	parent
			hoverEnabled:	true
			cursorShape:	containsMouse ? Qt.PointingHandCursor : Qt.ArrowCursor
			onClicked:		mainWindow.analysesVisible = !mainWindow.analysesVisible
			z:				3
		}
	}

	ListView
	{
		id:								analysesListView

		model:							analysesModel
		highlightFollowsCurrentItem:	true
		z:								2
		visible:						mainWindow.analysesVisible

		anchors
		{
			top:		parent.top
			left:		openCloseButton.right
			right:		parent.right
			bottom:		parent.bottom
			margins:	parent.border.width
		}



		delegate: Loader
		{
			source:			formPath
			width:			analysesListView.width

			property bool	currentSelected:	ListView.isCurrentItem
			property Item	listView:			ListView.view
			property int	myIndex:			index
			property string	analysisTitle:		title
		}
	}
}
