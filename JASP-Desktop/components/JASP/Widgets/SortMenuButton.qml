import QtQuick 2.0
import JASP.Theme 1.0

Rectangle
{
	height:		20 * preferencesModel.uiScale
	width:		height
	z:			10
	radius:		height
	color:		mouseAreaSort.containsMouse ? hoveredColor: defaultColor

	property string defaultColor: Theme.white
	property string hoveredColor: Theme.whiteBroken
	property var sortMenuModel: null

	Image
	{
		source: "qrc:/icons/sort-az.png"
		anchors.fill: parent
		anchors.margins: 3
	}

	MouseArea
	{
		id: mouseAreaSort
		anchors.fill: parent
		hoverEnabled: true
		onClicked:
		{
			var functionCall = function (index)
			{
				sortMenuModel.clickSortItem(index)
				customMenu.hide()
			}

			var props = {
				"model": sortMenuModel,
				"functionCall"	: functionCall
			};

			customMenu.showMenu(parent, props, 0, parent.height);
		}
	}
}
