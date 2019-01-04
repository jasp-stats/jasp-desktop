import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

Item
{
	id:			modulesMenu

	width:		slidePart.width
	height:		600
	z:			1
	visible:	slidePart.x < slidePart.width

	property bool opened: false //should be from some model

	onOpenedChanged: if(!opened) ribbonModel.highlightedModuleIndex = -1

	Rectangle
	{
		id:				slidePart
		x:				modulesMenu.opened ? 0 : width
		width:			340
		height:			modulesMenu.height
		color:			Theme.uiBackground
		border.width:	1
		border.color:	Theme.uiBorder

		Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }

		Column
		{
			id:			modules
			spacing:	4
			width:		parent.width - Theme.generalAnchorMargin
			anchors
			{
				top:				parent.top
				topMargin:			5
				horizontalCenter:	parent.horizontalCenter
			}

			Repeater
			{

				model: ribbonModel

				MenuButton
				{
					id:					moduleButton
					text:				displayText

					width:				parent.width-6
					height:				40
					anchors.leftMargin: 3
					anchors.left:		parent.left
					onClicked:			ribbonModel.toggleModuleEnabled(index)
					//enabled:			modulesMenuModel.buttonsenabled[index];

					onHoveredChanged:	if(hovered) ribbonModel.highlightedModuleIndex = index

					textColor:			ribbonEnabled ? Theme.black : Theme.gray
				}
			}
		}


		focus: true
		//Keys.onSpacePressed: locationMenu.visible = !locationMenu.visible

		Item
		{
			id:			dropShadow
			y:			0
			x:			-width
			height:		parent.height
			width:		Theme.shadowRadius
			//visible:	modulesMenu.visible

			Rectangle
			{
				anchors.centerIn: parent
				rotation:	90
				gradient:	Gradient {
					GradientStop { position: 0.0; color: Theme.shadow }
					GradientStop { position: 1.0; color: "transparent" } }
				height:		dropShadow.width
				width:		dropShadow.height
			}
		}
	}
}
