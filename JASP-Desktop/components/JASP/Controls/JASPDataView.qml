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
	property alias leftTopCornerDelegate: theView.leftTopCornerDelegate

	property alias itemHorizontalPadding: theView.itemHorizontalPadding
	property alias itemVerticalPadding: theView.itemVerticalPadding

	//property alias font: theView.font //not properly implemented

	Flickable
	{
		id: myFlickable

		anchors.fill: parent

		contentWidth: theView.width
		contentHeight: theView.height

		clip: true

		DataSetView
		{
			z: -1
			Keys.onEscapePressed: theWindow.close()
			id: theView
			model: null

			viewportX: myFlickable.visibleArea.xPosition * width
			viewportY: myFlickable.visibleArea.yPosition * height
			viewportW: myFlickable.visibleArea.widthRatio * width
			viewportH: myFlickable.visibleArea.heightRatio * height

		}


		ScrollBar.vertical: ScrollBar { z:2}
		ScrollBar.horizontal: ScrollBar { z:2}

	}
}
