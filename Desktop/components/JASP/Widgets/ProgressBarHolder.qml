import QtQuick			2.11
import QtQuick.Controls	2.4


Item
{

	Rectangle
	{
		color:			jaspTheme.darkeningColour
		opacity:		parent.visible ? 0.5 : 0.0
		anchors.fill:	parent
		Behavior on opacity	{ enabled: preferencesModel.animationsOn; PropertyAnimation { duration: 100 } }
	}

	MouseArea
	{
		anchors.fill:	parent
		hoverEnabled:	true
	}

	Rectangle
	{
		id:					progressBarHolder
		color:				jaspTheme.uiBackground
		border.color:		jaspTheme.uiBorder
		border.width:		1
		height:				120 * preferencesModel.uiScale
		radius:				height
		width:				parent.width * 0.5
		anchors.centerIn:	parent

		Text
		{
			id:						loadingText
			horizontalAlignment:	Text.AlignHCenter
			text:					mainWindow.progressBarStatus
			font:					jaspTheme.fontLabel
			color:					jaspTheme.textEnabled
			elide:					Text.ElideMiddle

			anchors
			{
				top:			parent.top
				left:			parent.left
				right:			parent.right
				topMargin:		16 * preferencesModel.uiScale
				leftMargin:		height / 2
				rightMargin:	height / 2
			}
		}

		ProgressBar
		{
			id:				loadingBar
			from:			0
			to:				100
			value:			mainWindow.progressBarProgress

			anchors
			{
				verticalCenter:	parent.verticalCenter
				left:			progressBarHolder.left
				right:			progressBarHolder.right
				margins:		30 * preferencesModel.uiScale
			}

			background:
				Rectangle
				{
					//y:				3 * preferencesModel.uiScale
					implicitWidth:	200 * preferencesModel.uiScale
					height:			18 * preferencesModel.uiScale
					radius:			height
					color:			jaspTheme.grayLighter
				}


			contentItem: Item
			{
				implicitWidth:	200 * preferencesModel.uiScale
				implicitHeight: 14  * preferencesModel.uiScale

				Rectangle
				{
					y:		2 * preferencesModel.uiScale
					x:		2 * preferencesModel.uiScale
					width:	loadingBar.visualPosition * (parent.width - (x * 2))
					height: parent.height
					radius: height
					color:	jaspTheme.blue
				}
			}
		}
	}
}
