import QtQuick 2.10
import QtQuick.Window 2.10
import QtQuick.Controls 2.2
import JASP 1.0

FocusScope
{
	property alias model: theView.model

	property alias itemDelegate: theView.itemDelegate
	property alias rowNumberDelegate: theView.rowNumberDelegate
	property alias columnHeaderDelegate: theView.columnHeaderDelegate
	property alias leftTopCornerItem: theView.leftTopCornerItem

	property alias itemHorizontalPadding: theView.itemHorizontalPadding
	property alias itemVerticalPadding: theView.itemVerticalPadding

	//property alias font: theView.font //not properly implemented


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

		}


		ScrollBar.vertical: ScrollBar { id: vertiScroller; z:2}
		ScrollBar.horizontal: ScrollBar { z:2}

	}

/*
	MouseArea
	{
		anchors.fill: parent

		acceptedButtons: Qt.NoButton

		onWheel:
		{
			console.log("wheeel!")

			myFlickable.flickDeceleration = -10000

			wheel.accepted = false
		}
	}*/

}
