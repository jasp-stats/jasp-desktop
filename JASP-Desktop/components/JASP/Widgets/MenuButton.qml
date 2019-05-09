import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

RectangularButton
{
	property bool hasSubMenu:			false
	property bool showHovered:			hasSubMenu ? delayOnhoverTimer.running : hovered

	id: root
	font:			Theme.fontRibbon
	color:			(_pressed || activeFocus) ? Theme.buttonColorPressed : (showHovered || selected) ? Theme.buttonColorHovered : "transparent"
	border.width:	0
	centerText:		false

	signal hoverClicked();

	Timer
	{
		id:					delayOnhoverTimer
		interval:			Theme.hoverTime
		running:			false
		repeat:				false
		onTriggered:		if (hovered && root.hasSubMenu) root.hoverClicked();
    }

	onClicked:				delayOnhoverTimer.stop();
	onHoveredChanged:		if (hasSubMenu)
							{
								if (hovered)	delayOnhoverTimer.start()
								else			delayOnhoverTimer.stop()
							}

	Image
	{
		anchors.verticalCenter:	parent.verticalCenter
		anchors.right:			parent.right
		anchors.rightMargin:	Theme.generalAnchorMargin
		height:					Theme.subMenuIconHeight
		width:					height
		source:					root.hasSubMenu ? "qrc:/icons/large-arrow-right.png" : ""
		visible:				hasSubMenu
		opacity:				enabled ? ((hovered || activeFocus) ? 1 : 0.5) : 0.3
		smooth:					true
		mipmap:					true
		sourceSize.width:		width * 2
		sourceSize.height:		height * 2
	}

}
