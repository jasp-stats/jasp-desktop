import QtQuick			2.11
import JASP.Widgets		1.0
import JASP.Theme		1.0
import QtQuick.Controls	2.4

//Only works vertically right now...

Rectangle
{
	id:				openCloseButton

	signal arrowClicked
	property bool pointingLeft: true
	property bool showArrow:	false

	property string toolTipDrag:	""
	property string toolTipArrow:	""


	width:			Theme.splitHandleWidth
	color:			styleData.hovered ? Theme.grayLighter : Theme.uiBackground
	//border.color:	Theme.uiBorder
	//border.width:	1

	anchors
	{
		top:		parent.top
		right:		parent.right
		bottom:		parent.bottom
	}

	ToolTip
	{
		text:			openCloseButton.toolTipDrag
		timeout:		Theme.toolTipTimeout
		delay:			Theme.toolTipDelay
		font:			Theme.font
		background:		Rectangle { color:	Theme.tooltipBackgroundColor }
		visible:		openCloseButton.toolTipDrag !== "" && styleData.hovered && (!openCloseButton.showArrow || !arrowMouse.containsMouse)
		y:				parent.height / 2
		x:				parent.width / 2
	}

/*	readonly property bool styleData.index		Specifies the index of the splitter handle. The handle between the first and the second item will get index 0, the next handle index 1 etc.
	readonly property bool styleData.hovered	The handle is being hovered.
	readonly property bool styleData.pressed	The handle is being pressed.
	readonly property bool styleData.resizing	The handle is being dragged. */


	Item
	{
		id:			centerElement
		height:		160
		anchors
		{
			verticalCenter: parent.verticalCenter
			left:			parent.left
			right:			parent.right
		}

		Loader //No arrow? then three dots in the center instead
		{
			sourceComponent:	openCloseButton.showArrow ? undefined : threeDotsComp
			anchors.centerIn:	parent
		}

		Rectangle
		{

			color:			arrowMouse.containsMouse ? Theme.grayLighter : Theme.uiBackground
			visible:		openCloseButton.showArrow
			anchors.fill:	parent
			MouseArea
			{
				id:				arrowMouse
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		openCloseButton.arrowClicked()
				z:				3
			}

			Image
			{

				readonly property string iconsFolder:		"qrc:/images/"
				readonly property string leftIcon:			"arrow-left.png"
				readonly property string rightIcon:			"arrow-right.png"

				source:				iconsFolder + (openCloseButton.pointingLeft ? leftIcon : rightIcon)
				width:				parent.width - (4 * preferencesModel.uiScale)
				height:				width
				sourceSize.width:	width * 2;
				sourceSize.height:	height * 2;

				anchors.centerIn:	parent

				ToolTip.text:			openCloseButton.toolTipArrow
				ToolTip.timeout:		Theme.toolTipTimeout
				ToolTip.delay:			Theme.toolTipDelay
				ToolTip.toolTip.font:	Theme.font
				ToolTip.visible:		openCloseButton.toolTipArrow !== "" && arrowMouse.containsMouse

			}
		}
	}

	Item
	{
		anchors
		{
			top:	parent.top
			bottom:	centerElement.top
			left:	parent.left
			right:	parent.right
		}

		Loader
		{
			visible:			height < parent.height
			sourceComponent:	threeDotsComp
			anchors.centerIn:	parent
		}

	}

	Item
	{
		anchors
		{
			top:	centerElement.bottom
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}

		Loader
		{
			visible:			height < parent.height
			sourceComponent:	threeDotsComp
			anchors.centerIn:	parent
		}

	}

	Component
	{
		id:		threeDotsComp

		Item
		{
							id:		threeDots
							height:	width * 4
							width:	Theme.splitHandleWidth * 0.3
			property color	kleur:	Theme.grayDarker

			Rectangle
			{
				color:		threeDots.kleur
				height:		width
				radius:		width

				anchors
				{
					top:	parent.top
					left:	parent.left
					right:	parent.right
				}
			}

			Rectangle
			{
				color:				threeDots.kleur
				height:				width
				radius:				width
				anchors
				{
					verticalCenter:	parent.verticalCenter
					left:			parent.left
					right:			parent.right
				}
			}

			Rectangle
			{
				color:		threeDots.kleur
				height:		width
				radius:		width

				anchors
				{
					left:	parent.left
					right:	parent.right
					bottom:	parent.bottom
				}
			}
		}
	}
}
