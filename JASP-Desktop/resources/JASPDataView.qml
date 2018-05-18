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

		//flickDeceleration: 4000

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

				hoverEnabled: true

				cursorShape: containsMouse ? Qt.OpenHandCursor : Qt.ArrowCursor

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

				onWheel:
				{
					//console.log("wheel.angleDelta ",wheel.angleDelta)
					if(wheel.angleDelta.y == 120)
						vertiScroller.decrease()
					else if(wheel.angleDelta.y == -120)
							vertiScroller.increase()
					else
						wheel.accepted = false
				}
			}
		}


		ScrollBar.vertical: ScrollBar
		{
			id: vertiScroller;
			z: 0

			stepSize: 0.025
			//onSizeChanged: if(size < 0.02)	size = 0.02 // a bit crude and doesnt rebind later but it works, which means we can see it at the very least...

			/*
			contentItem: Item
			{
				implicitWidth: 8
				implicitHeight: Math.max((parent.height * parent.size), implicitWidth)
			}


			Rectangle
			{
				opacity: 0.5
				y: parent.position * parent.height
				width: parent.width
				height: Math.max((parent.height * parent.size), width)
				color: "#14a1e3"
				radius: width
			}*/


		}

		ScrollBar.horizontal: ScrollBar
		{
			z: 0
			stepSize: 0.025
			//onSizeChanged: if(size < 0.02)	size = 0.02
		}
	}




}
