import QtQuick 2.9
import QtQuick.Controls 2.2
import JASP.Theme 1.0

Rectangle
{
	id: filterButtonRoot

	color:			(!filterButtonRoot.disabled && filterButtonRoot.hovered) || filterButtonRoot.selected ? Theme.white : Theme.grayLighter
	border.color:	(!filterButtonRoot.disabled && filterButtonRoot.hovered) || filterButtonRoot.selected ? Theme.black : Theme.gray
	border.width: 1

	property string text:				""
	property string toolTip:			"This is a button"
	property bool	disabled:			false
	property bool	selected:			false
	property string iconSource:			""
	property real	buttonPadding:		buttonIcon.visible ? 4 : 16
	property alias	hovered:			buttonMouseArea.containsMouse
	property bool	showIconAndText:	false

	implicitWidth:	showIconAndText ? buttonText.width + buttonPadding + 32 + buttonPadding : buttonIcon.visible ? 32 : buttonText.width + buttonPadding
	implicitHeight: 32


	ToolTip.delay:		500
	ToolTip.timeout:	3500
	ToolTip.visible:	hovered
	ToolTip.text:		toolTip

	signal clicked()

	MouseArea
	{
		id: buttonMouseArea
		anchors.fill: parent
		acceptedButtons: Qt.LeftButton
		hoverEnabled: true
		cursorShape: containsMouse && !parent.disabled ? Qt.PointingHandCursor : Qt.ArrowCursor

		onClicked: if(!filterButtonRoot.disabled) filterButtonRoot.clicked()
	}

	Image
	{
		id: buttonIcon
		x:	filterButtonRoot.showIconAndText ? filterButtonRoot.buttonPadding : (parent.width / 2) - (width / 2)
		y:	(parent.height / 2) - (height / 2)

		width:	Math.min(filterButtonRoot.width - (2 * buttonPadding), height)
		height: filterButtonRoot.height - (2 * buttonPadding)

		sourceSize.width: Math.max(48, width)
		sourceSize.height: Math.max(48, height)

		visible: filterButtonRoot.iconSource != "" || filterButtonRoot.showIconAndText
		source: filterButtonRoot.iconSource
	}

	Text
	{
		id: buttonText
		x:	filterButtonRoot.showIconAndText ? buttonIcon.x + buttonIcon.width + filterButtonRoot.buttonPadding : (parent.width / 2) - (width / 2)
		y:	(parent.height / 2) - (height / 2)

		text: filterButtonRoot.text
		visible: filterButtonRoot.iconSource == "" || filterButtonRoot.showIconAndText

		font.pixelSize: Math.max(filterButtonRoot.height * 0.4, Math.min(12, filterButtonRoot.height - 2))

		height: contentHeight
		width: contentWidth
	}
}
