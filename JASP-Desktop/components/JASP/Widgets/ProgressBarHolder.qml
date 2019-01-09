import QtQuick			2.11
import QtQuick.Controls	2.4
import JASP.Theme		1.0

Item
{
	visible:				false
	anchors
	{
		top:	rootDataset.top
		left:	rootDataset.left
		right:	rootDataset.right
	}

	height:			!visible ? 0 : loadingBar.height + loadingBar.y

    Text
    {
		id:						loadingText
		horizontalAlignment:	Text.AlignHCenter
		text:					mainWindow.progressBarStatus
		font:					Theme.fontLabel

		anchors
		{
			top:		parent.top
			left:		parent.left
			right:		parent.right
			topMargin:	30
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
			top:		loadingText.bottom
			left:		progressBarHolder.left
			right:		progressBarHolder.right
			margins:	30
		}

		background:
			Rectangle
			{
				y:				3
				implicitWidth:	200
				height:			6
				radius:			height
				color:			Theme.grayLighter
			}


		contentItem: Item
		{
			implicitWidth:	200
			implicitHeight: 12

			Rectangle
			{
				width:	loadingBar.visualPosition * parent.width
				height: parent.height
				radius: height
				color:	Theme.blue
			}
		}
    }
}
