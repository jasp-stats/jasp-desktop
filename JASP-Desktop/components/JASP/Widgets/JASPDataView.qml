import "qrc:/components/JASP/Controls/"
import QtQuick 2.9
import QtQuick.Window 2.3
import QtQuick.Controls 2.2
import JASP 1.0

FocusScope
{
	id: __JASPDataViewRoot
	property alias model: theView.model

	property alias itemDelegate:			theView.itemDelegate
	property alias rowNumberDelegate:		theView.rowNumberDelegate
	property alias columnHeaderDelegate:	theView.columnHeaderDelegate
	property alias leftTopCornerItem:		theView.leftTopCornerItem
	property alias extraColumnItem:			theView.extraColumnItem

	property alias itemHorizontalPadding:	theView.itemHorizontalPadding
	property alias itemVerticalPadding:		theView.itemVerticalPadding
	property alias font:					theView.font

	JASPMouseAreaToolTipped
	{
		z: -10


		anchors.fill: parent
		anchors.leftMargin: theView.rowNumberWidth
		anchors.topMargin: theView.headerHeight

		toolTipText: "double-click to edit data"

		acceptedButtons: Qt.NoButton

	}

	signal doubleClicked()

	Flickable
	{
		id: myFlickable

		//anchors.fill: parent
		anchors.top:	parent.top
		anchors.left:	parent.left
		anchors.right:	vertiScroller.left
		anchors.bottom: horiScroller.top

		contentWidth:	theView.width
		contentHeight:	theView.height

		//flickDeceleration: 4000

		clip: true

		DataSetView
		{
			z: -1
			id: theView
			model: null

			viewportX:	myFlickable.visibleArea.xPosition * width
			viewportY:	myFlickable.visibleArea.yPosition * height
			viewportW:	myFlickable.visibleArea.widthRatio * width
			viewportH:	myFlickable.visibleArea.heightRatio * height

			MouseArea
			{
				id: datasetMouseArea
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

				onWheel:
				{
					//console.log("wheel.angleDelta ",wheel.angleDelta)


					if(wheel.angleDelta.y == 120)
					{
						if(wheel.modifiers & Qt.ShiftModifier)
							horiScroller.scrollUp()
						else
							vertiScroller.scrollUp()
					}
					else if(wheel.angleDelta.y == -120)
					{
						if(wheel.modifiers & Qt.ShiftModifier)
							horiScroller.scrollDown()
						else
							vertiScroller.scrollDown()
					}
					else
						wheel.accepted = false
				}
			}

		}
	}


	JASPScrollBar
	{
		id:				vertiScroller;
		flickable:		myFlickable

		//extraMarginRightOrBottom:	horiScroller.height
		//extraMarginLeftOrTop:		theView.headerHeight

		anchors.top:	parent.top
		anchors.right:	parent.right
		anchors.bottom: parent.bottom

		anchors.bottomMargin: horiScroller.height
	}

	JASPScrollBar
	{
		id:				horiScroller;
		flickable:		myFlickable
		vertical:		false

		//extraMarginRightOrBottom:	vertiScroller.width
		//extraMarginLeftOrTop:		theView.rowNumberWidth

		anchors.left:	parent.left
		anchors.right:	parent.right
		anchors.bottom: parent.bottom

		anchors.rightMargin: vertiScroller.width
	}



}
