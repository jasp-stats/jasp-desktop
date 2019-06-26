import QtQuick 2.0
import JASP.Theme 1.0

MenuButton
{
	id:					sortButton
	_scaledDim:         24 * preferencesModel.uiScale
	width:				height
	toolTip:			"Sort the items"
	radius:				height
	iconSource:			"qrc:/icons/sort-az.png"
	z:                  10
	defaultColor:       Theme.buttonColor

	property var	sortMenuModel:	null
	property int	scrollXPosition: 0
	property int	scrollYPosition: 0
	property point	scrollPosition:	Qt.point(scrollXPosition, scrollYPosition)

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

		customMenu.toggle(sortButton, props, 0, sortButton.height);
		customMenu.scrollOri		= scrollPosition;
		customMenu.menuScroll.y		= Qt.binding(function() { return -1 * (scrollPosition.y - customMenu.scrollOri.y); });
	}

}
