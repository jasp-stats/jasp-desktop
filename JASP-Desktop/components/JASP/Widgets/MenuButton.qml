import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

RectangularButton
{
	property bool clickonhover: false
	color:			_pressed ? Theme.buttonColorPressed :	_showHovered ? Theme.gray						: "transparent"
	border.width:	0
	centerText:		false
	onHoveredChanged: { if (clickonhover && hovered) clicked(); }
}
