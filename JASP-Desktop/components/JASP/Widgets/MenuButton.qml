import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

RectangularButton
{
	id: root
	property bool clickOnHover:			false
	property bool clickWhenFocussed:	true;
	property bool hasSubMenu:			false

	font: Theme.fontRibbon

	color: (_pressed || selected) ? Theme.buttonColorPressed : root.hovered ? Theme.buttonColorHovered : "transparent"
	border.width:	0
	centerText:		false

	Timer
	{
		id:				delayOnhoverTimer
		interval:		Theme.hoverTime
		running:		false
		repeat:			false
		onTriggered:	if (hovered && root.hasSubMenu) forceActiveFocus();
    }

	Keys.onSpacePressed:	{ clicked();  event.accepted = true;}
	Keys.onEnterPressed:	{ clicked();  event.accepted = true;}
	Keys.onReturnPressed:	{ clicked();  event.accepted = true;}

	onHoveredChanged:	if (clickOnHover)
						{
							if (hovered)	delayOnhoverTimer.start()
							else			delayOnhoverTimer.stop()
						}

	onActiveFocusChanged:	if (clickOnHover && activeFocus && clickWhenFocussed)	clicked()

	Image
	{
		anchors.verticalCenter:	parent.verticalCenter
		anchors.right:			parent.right
		anchors.rightMargin:	Theme.generalAnchorMargin
		height:					Theme.subMenuIconHeight
		width:					height
		source:					"qrc:/icons/large-arrow-right.png"
		visible:				hasSubMenu
		opacity:				enabled ? ((hovered || activeFocus) ? 1 : 0.5) : 0.3
		smooth:					true
		mipmap:					true
		sourceSize.width:		width * 2
		sourceSize.height:		height * 2
	}

}
