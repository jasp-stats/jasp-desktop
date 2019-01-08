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
				bottom:				parent.bottom
			}

			property int buttonMargin:	3
			property int buttonWidth:	width - (buttonMargin * 2)
			property int buttonHeight:	40

			MenuButton
			{
				id:					addModuleButton
				text:				"Add Dynamic Module"
				width:				modules.buttonWidth
				height:				modules.buttonHeight
				anchors.leftMargin: modules.buttonMargin
				anchors.left:		parent.left
				onClicked: 			moduleInstallerDialog.open()
				iconSource:			"qrc:/icons/addition-sign.svg"
				showIconAndText:	true
				toolTip:			"Install a dynamic module"
			}

			ToolSeparator
			{
				orientation:				Qt.Horizontal
				width:						modules.buttonWidth
				anchors.horizontalCenter:	parent.horizontalCenter
			}

			Repeater
			{

				model: ribbonModel

				Item
				{
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					anchors.left:		parent.left

					MenuButton
					{
						id:					moduleButton
						text:				displayText
						textColor:			ribbonEnabled ? Theme.black : hovered ? Theme.white : Theme.gray
						toolTip:			(ribbonEnabled ? "Disable" : "Enable") + " module " + displayText

						onClicked:			ribbonModel.toggleModuleEnabled(index)
						onHoveredChanged:	if(hovered && enabled) ribbonModel.highlightedModuleIndex = index

						anchors
						{
							top:	parent.top
							left:	parent.left //minusButton.right //isDynamic ? minusButton.right : parent.left
							right:	parent.right
							bottom:	parent.bottom
						}
					}

					MenuButton
					{
						z:				1
						id:				minusButton
						visible:		isDynamic
						iconSource:		"qrc:/icons/subtraction-sign.svg"
						width:			height
						onClicked:		dynamicModules.uninstallJASPModule(moduleName)
						toolTip:		"Uninstall module " + displayText
						anchors
						{
							top:	parent.top
							left:	parent.left
							bottom:	parent.bottom
						}
					}
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
