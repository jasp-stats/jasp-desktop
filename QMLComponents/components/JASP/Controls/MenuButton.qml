import QtQuick
import QtQuick.Layouts
import JASP.Controls

/*!
    \qmltype MenuButton
    \inqmlmodule JASP.Controls 1.0
    \brief A button with optional submenu arrow and hover-to-open behaviour.

    Extends RoundedButton with hover-delay logic and a submenu indicator arrow.
    Used internally for ribbon menus and toolbar items.

    \note This is primarily an internal UI component. Module developers typically
    use Button or HelpButton instead.

    \section1 Properties

    \list
    \li \b hasSubMenu (bool) - Show a submenu arrow and enable hover-to-open. Default: false.
    \li \b defaultColor (color) - Background color when idle. Default: "transparent".
    \endlist

    \section1 Signals

    \list
    \li \b hoverClicked() - Emitted when hover-delay triggers on a submenu button.
    \endlist

    \section1 Example

    \qml
    MenuButton {
        text: qsTr("Options")
        hasSubMenu: true
    }
    \endqml
*/
RoundedButton
{
	property bool	hasSubMenu:			false
	property bool	showHovered:		hasSubMenu ? delayOnhoverTimer.running : hovered
	property color	defaultColor:       "transparent"
	property real	arrowExtraWidth:	arrow.width + jaspTheme.generalAnchorMargin

	id:					root
	font:				jaspTheme.fontRibbon
	color:				(_pressed || activeFocus) ? jaspTheme.buttonColorPressed : (showHovered || selected) ? jaspTheme.buttonColorHovered : defaultColor
	border.width:		0
	centerText:			false
	activeFocusOnTab:	true
	
	Accessible.role:			Accessible.Button
	Accessible.name:			text
	Accessible.description:		toolTip != "" ? toolTip : qsTr("A menubutton %1").arg(text)
	Accessible.onPressAction:	clicked()
	

	signal hoverClicked();
	onHoverClicked:			forceActiveFocus();

	Timer
	{
		id:					delayOnhoverTimer
		interval:			jaspTheme.hoverTime
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
		id:						arrow
		anchors.verticalCenter:	parent.verticalCenter
		anchors.right:			parent.right	
		height:					jaspTheme.subMenuIconHeight
		width:					height
		source:					root.hasSubMenu ? jaspTheme.iconPath + "/large-arrow-right.png" : ""
		visible:				hasSubMenu
		opacity:				enabled ? ((hovered || activeFocus) ? 1 : 0.5) : 0.3
		smooth:					true
		mipmap:					true
		sourceSize.width:		width * 2
		sourceSize.height:		height * 2
	}

}
