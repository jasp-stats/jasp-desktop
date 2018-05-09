import QtQuick 2.10
import QtQuick.Window 2.10
import QtQuick.Controls 2.2
import JASP 1.0

FocusScope
{
	id: __JASPDataViewRoot
	property alias model: theView.model

	property alias itemDelegate: theView.itemDelegate
	property alias rowNumberDelegate: theView.rowNumberDelegate
	property alias columnHeaderDelegate: theView.columnHeaderDelegate
	property alias leftTopCornerItem: theView.leftTopCornerItem

	property alias itemHorizontalPadding: theView.itemHorizontalPadding
	property alias itemVerticalPadding: theView.itemVerticalPadding

	//property alias font: theView.font //not properly implemented

	signal doubleClicked()

	Flickable
	{
		id: myFlickable

		anchors.fill: parent

		contentWidth: theView.width
		contentHeight: theView.height

		flickDeceleration: 4000

		clip: true

		DataSetView
		{
			z: -1
			id: theView
			model: null

			viewportX: myFlickable.visibleArea.xPosition * width
			viewportY: myFlickable.visibleArea.yPosition * height
			viewportW: myFlickable.visibleArea.widthRatio * width
			viewportH: myFlickable.visibleArea.heightRatio * height

			MouseArea
			{
				z: -10
				property real lastTimeClicked: -1
				property real doubleClickTime: 500

				anchors.fill: parent

				onReleased:
				{
					var curTime = new Date().getTime()

					if(lastTimeClicked === -1)
					{
						lastTimeClicked = curTime
					}
					else if(curTime - lastTimeClicked < doubleClickTime)
					{
						lastTimeClicked = -1
						__JASPDataViewRoot.doubleClicked()
					}
					else
						lastTimeClicked = -1
				}
			}
		}


		ScrollBar.vertical: ScrollBar { id: vertiScroller; z:2}
		ScrollBar.horizontal: ScrollBar { z:2}
	}




}
