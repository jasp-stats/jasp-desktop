import QtQuick 2.0
import JASP.Theme 1.0

Rectangle
{
	height:		18 * preferencesModel.uiScale
	width:		height
	z:			10
	radius:		height
	color:		mouseAreaSort.containsMouse ? hoveredColor: defaultColor

	property string defaultColor:	Theme.white
	property string hoveredColor:	Theme.whiteBroken
	property var	sortMenuModel:	null
	property int	scrollXPosition: 0
	property int	scrollYPosition: 0
	property point	scrollPosition:	Qt.point(scrollXPosition, scrollYPosition)

	Image
	{
		source: "qrc:/icons/sort-az.png"
		anchors.fill: parent
		anchors.margins: 3 * preferencesModel.uiScale
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
			customMenu.scrollOri		= scrollPosition;
			customMenu.menuScroll.y		= Qt.binding(function() { return -1 * (scrollPosition.y - customMenu.scrollOri.y); });
		}
	}
}
