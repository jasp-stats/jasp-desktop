import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

RectangularButton
{
	color:			_pressed ? Theme.buttonColorPressed :	_showHovered ? Theme.gray						: "transparent"
	//border.color:											_showHovered ? Theme.buttonBorderColorHovered	: Theme.buttonBorderColor
	border.width:	0
	//radius:			height
}
