import QtQuick			2.11
import QtQuick.Controls	2.2
import QtQuick.Layouts	1.3
import JASP.Theme		1.0
import JASP.Widgets		1.0
import JASP.Controls	1.0

FocusScope
{
	id:			modulesMenu

	width:		slidePart.width
	height:		600
	z:			1
	visible:	slidePart.x < slidePart.width

	property bool opened: false //should be from some model

	onOpenedChanged: if(!opened) ribbonModel.highlightedModuleIndex = -1; else forceActiveFocus();

	Keys.onEscapePressed: if(opened)
	{
		opened = false;
		focus = false;
	}

	Rectangle
	{
		id:				slidePart
		x:				modulesMenu.opened ? 0 : width
		width:			340 * preferencesModel.uiScale
		height:			modulesMenu.height
		color:			Theme.fileMenuColorBackground
		border.width:	1
		border.color:	Theme.uiBorder

		Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


		MouseArea
		{
			id:				gottaCatchEmAll //Clicks that is
			anchors.fill:	parent
			z:				-6
		}

		Column
		{
			id:			modules
			spacing:	4  * preferencesModel.uiScale
			width:		parent.width - Theme.generalAnchorMargin
			anchors
			{
				top:				parent.top
				topMargin:			5
				horizontalCenter:	parent.horizontalCenter
				bottom:				parent.bottom
			}

			property int buttonMargin:	3  * preferencesModel.uiScale
			property int buttonWidth:	width - (buttonMargin * 2)
			property int buttonHeight:	40  * preferencesModel.uiScale
			
			MenuButton
			{
				id:					addModuleButton
				text:				"Install Module"
				width:				modules.buttonWidth
				height:				modules.buttonHeight
				anchors.leftMargin: modules.buttonMargin
				anchors.left:		parent.left
				onClicked: 			moduleInstallerDialog.open()
				iconSource:			"qrc:/icons/install_icon.png"  // icon from https://icons8.com/icon/set/install/cotton
				showIconAndText:	true
				iconLeft:			false
				toolTip:			"Install a module"
				visible:			preferencesModel.developerMode
			}

			ToolSeparator
			{
				orientation:				Qt.Horizontal
				width:						modules.buttonWidth
				anchors.horizontalCenter:	parent.horizontalCenter
				visible:					preferencesModel.developerMode
			}
			
			MenuButton
			{
				id:					addDeveloperModuleButton
				text:				folderSelected ? "Install Developer Module" : "Select a Developer Module"
				width:				modules.buttonWidth
				height:				modules.buttonHeight
				anchors.leftMargin: modules.buttonMargin
				anchors.left:		parent.left
				onClicked: 			folderSelected ? dynamicModules.installJASPDeveloperModule() : preferencesModel.browseDeveloperFolder()
				toolTip:			folderSelected ? "Install selected developer module" : "Select a developer module under Left menu->Preference->Advanced"
				visible:			preferencesModel.developerMode
				enabled:			dynamicModules.developersModuleInstallButtonEnabled

				readonly property bool folderSelected: preferencesModel.developerFolder != ""
			}
			
			ToolSeparator
			{
				orientation:				Qt.Horizontal
				width:						modules.buttonWidth
				anchors.horizontalCenter:	parent.horizontalCenter
				visible:					preferencesModel.developerMode
			}			

			Repeater
			{

				model: ribbonModel

				Rectangle
				{
					visible:			!isCommon
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					anchors.left:		parent.left
					color:				!isDynamic || dynamicModule.status !== "error" ? "transparent" : Theme.red

					CheckBox
					{
						id:					moduleButton
						label:				displayText
						checked:			ribbonEnabled
						onCheckedChanged:	ribbonModel.setModuleEnabled(index, checked)
						enabled:			!isDynamic || !(dynamicModule.loading || dynamicModule.installing)
						font:				Theme.fontRibbon

						toolTip:			!isDynamic ? ""
												: dynamicModule.installing ? "Installing:\n" + dynamicModule.installLog
													: dynamicModule.loading ? "Loading:\n" + dynamicModule.loadLog
														: dynamicModule.status === "readyForUse" ? "Loaded and ready for use!"
															: dynamicModule.status === "error" ? "Error occurred!"
																: "Not ready for use?"

						//textColor:			ribbonEnabled ? Theme.black : hovered ? Theme.white : Theme.gray
						//toolTip:			(ribbonEnabled ? "Disable" : "Enable") + " module " + displayText
						//onClicked:			ribbonModel.toggleModuleEnabled(index)
						//onHoveredChanged:	if(hovered && enabled) ribbonModel.highlightedModuleIndex = index

						anchors
						{
							left			: parent.left //minusButton.right //isDynamic ? minusButton.right : parent.left
							right			: minusButton.left
							verticalCenter	: parent.verticalCenter
						}
					}

					MenuButton
					{
						z:				1
						id:				minusButton
						visible:		isDynamic
						iconSource:		hovered ? "qrc:/icons/delete_icon.png" : "qrc:/icons/delete_icon_gray.png"  // icon from https://icons8.com/icon/set/delete/material
						width:			visible ? height : 0
						onClicked:		dynamicModules.uninstallJASPModule(moduleName)
						toolTip:		"Uninstall module " + displayText
						anchors
						{
							right			: parent.right
							verticalCenter	: parent.verticalCenter
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
