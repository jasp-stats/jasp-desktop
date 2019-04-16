import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

RectangularButton
{
	property bool clickOnHover: false
	property bool clickWhenFocussed: true;
	property bool isIcon: true
	property bool hasSubMenu: false

	color:
	{
		if (isIcon)
		{
			if (_pressed || _showHovered) return Theme.buttonColorPressed
			else return "transparent"
		}
		else
		{
			if (_pressed) return Theme.buttonMenuColorPressed;
			else if (selected)
			{
				if (activeFocus) return Theme.buttonMenuColorFocus;
				else return Theme.buttonMenuColorSelected;
			}
			else if (_showHovered) return Theme.buttonMenuColorHovered;
			else return "transparent";
		}
	}

	border.width:	0
	centerText:		false

	Timer
	{
        id: delayOnhoverTimer
		interval: Theme.hoverTime
        running: false
        repeat: false
        onTriggered: {
			if (hovered) forceActiveFocus();
        }
    }

	Keys.onSpacePressed: { clicked();  event.accepted = true;}
	Keys.onEnterPressed: { clicked();  event.accepted = true;}
	Keys.onReturnPressed: { clicked();  event.accepted = true;}

	onHoveredChanged:
	{
		if (clickOnHover)
		{
			if (hovered)
				delayOnhoverTimer.start()
			else
				delayOnhoverTimer.stop()
		}
    }

	onActiveFocusChanged:
	{
		if (clickOnHover)
		{
			if (activeFocus && clickWhenFocussed)
				clicked()
		}
	}

	Image
	{
		anchors.verticalCenter: parent.verticalCenter
		anchors.right: parent.right
		anchors.rightMargin: Theme.generalAnchorMargin
		height: Theme.subMenuIconHeight
		width: height
		source: "qrc:/icons/large-arrow-right.png"
		visible: hasSubMenu
		opacity: enabled ? ((hovered || activeFocus) ? 1 : 0.5) : 0.3
	}

}
