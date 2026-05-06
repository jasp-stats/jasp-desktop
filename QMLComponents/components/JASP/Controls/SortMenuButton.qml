import QtQuick


/*!
    \qmltype SortMenuButton
    \inqmlmodule JASP.Controls 1.0
    \brief A circular sort button that opens a sort-order popup.

    Extends MenuButton with a sort icon. When clicked, opens a popup menu
    driven by sortMenuModel to let users re-order list items.

    \note This is primarily an internal UI component used by list controls.

    \section1 Properties

    \list
    \li \b sortMenuModel (var) - Model providing sort options. Default: null.
    \endlist

    \section1 Example

    \qml
    SortMenuButton {
        sortMenuModel: myList.sortMenuModel
    }
    \endqml
*/
MenuButton
{
	id:					sortButton
	_scaledDim:         24 * preferencesModel.uiScale
	width:				height
	toolTip:			qsTr("Sort the items")
	radius:				height
	iconSource:			jaspTheme.iconPath + "/sort-az.png"
	z:                  10
	defaultColor:       jaspTheme.buttonColor
	opacity:			enabled ? 1 : 0.5

	property var	sortMenuModel:	null
	property int	scrollXPosition: 0
	property int	scrollYPosition: 0
	property point	scrollPosition:	Qt.point(scrollXPosition, scrollYPosition)

	onClicked:
	{
		var functionCall = function (index)
		{
			sortMenuModel.clickSortItem(index)
			customMenu.hideMenus()
		}

		var props = {
			"model": sortMenuModel,
			"functionCall"	: functionCall
		};

		customMenu.toggle(sortButton, props);
		customMenu.scrollOri		= scrollPosition;
		customMenu.menuScroll.y		= Qt.binding(function() { return -1 * (scrollPosition.y - customMenu.scrollOri.y); });
	}

}
