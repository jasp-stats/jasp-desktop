import QtQuick			2.11
import QtQuick.Controls	2.2
import QtQuick.Layouts	1.3

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
	property int currentIndex: preferencesModel.developerMode ? -3 : -1  // -2, -3 denote install module and developer mode buttons

	onOpenedChanged: if(!opened) ribbonModel.highlightedModuleIndex = -1; else forceActiveFocus();

	Keys.onEscapePressed: closeAndFocusRibbon();
	Keys.onRightPressed:  closeAndFocusRibbon();

	function closeAndFocusRibbon()
	{
		opened       = false;
		ribbon.focus = true;
	}

	Keys.onPressed: (event)=>
	{
		if (event.key === Qt.Key_Down)
		{
			if (preferencesModel.developerMode)
			{
				let nextIndex  = currentIndex + 1;
				if (nextIndex === repeater.count)
					nextIndex  = -2;
				currentIndex   = nextIndex;
			}
			else
				currentIndex   = mod(currentIndex + 1, repeater.count);
		}
		else if (event.key === Qt.Key_Up)
		{
			if (preferencesModel.developerMode)
			{
				let nextIndex   = currentIndex - 1;
				if (nextIndex  === -3)
					nextIndex   = repeater.count -1;
				currentIndex    = nextIndex;
			}
			else
				currentIndex = mod(currentIndex - 1, repeater.count);
		}
	}

	Rectangle
	{
		id:				slidePart
		x:				modulesMenu.opened ? 0 : width
		width:			340 * preferencesModel.uiScale
		height:			modulesMenu.height
		color:			jaspTheme.fileMenuColorBackground
		border.width:	1
		border.color:	jaspTheme.uiBorder

		Behavior on x { enabled: preferencesModel.animationsOn; PropertyAnimation { duration: jaspTheme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }


		MouseArea
		{
			id:				gottaCatchEmAll //Clicks that is
			anchors.fill:	parent
			z:				-6
		}

		Flickable
		{
			id:						modulesFlick
			flickableDirection:		Flickable.VerticalFlick
			contentHeight:			modules.height
			contentWidth:			modules.width

			anchors
			{
				top:				parent.top
				topMargin:			5 * preferencesModel.uiScale
				left:				parent.left
				right:				vertScroller.left
				bottom:				parent.bottom
			}

			Column
			{
				id:			modules
				spacing:	4  * preferencesModel.uiScale
				width:		slidePart.width - vertScroller.width

				property int buttonMargin:	3  * preferencesModel.uiScale
				property int buttonWidth:	width - (buttonMargin * 2)
				property int buttonHeight:	40  * preferencesModel.uiScale

				MenuButton
				{
					id:					addModuleButton
					text:				qsTr("Install Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					anchors.left:		parent.left
					onClicked: 			moduleInstallerDialog.open()
					iconSource:			jaspTheme.iconPath + "/install_icon.png"  // icon from https://icons8.com/icon/set/install/cotton
					showIconAndText:	true
					iconLeft:			false
					toolTip:			qsTr("Install a module")
					visible:			preferencesModel.developerMode
					focus:				currentIndex === -2
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
					text:				folderSelected ? (dynamicModules.developersModuleInstallButtonEnabled ? qsTr("Install Developer Module") : qsTr("Installing Developer Module")) : qsTr("Select a Developer Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					anchors.left:		parent.left
					onClicked: 			folderSelected ? dynamicModules.installJASPDeveloperModule() : preferencesModel.browseDeveloperFolder()
					toolTip:			folderSelected ? (dynamicModules.developersModuleInstallButtonEnabled ? qsTr("Install selected developer module") : qsTr("Installing developer module now")) : qsTr("Select a developer module by clicking here")
					visible:			preferencesModel.developerMode
					enabled:			dynamicModules.developersModuleInstallButtonEnabled
					focus:				currentIndex === -1

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
					id:		repeater
					model:	ribbonModelUncommon

					Rectangle
					{
						width:				modules.buttonWidth
						height:				modules.buttonHeight
						anchors.leftMargin: modules.buttonMargin
						anchors.left:		parent.left
						color:				isSpecial || dynamicModule.status !== "error" ? "transparent" : jaspTheme.red

						CheckBox
						{
							id:					moduleButton
							label:				displayText
							checked:			ribbonEnabled
							onCheckedChanged:	ribbonModelUncommon.setModuleEnabled(index, checked)
							enabled:			isSpecial || !(dynamicModule.loading || dynamicModule.installing)
							font:				jaspTheme.fontRibbon
							focus:				index === currentIndex

							toolTip:			isSpecial									? qsTr("Ready") //Always ready!
												: dynamicModule.installing					? qsTr("Installing: %1\n").arg(dynamicModule.installLog)
												: dynamicModule.loading						? qsTr("Loading: %1\n").arg(dynamicModule.loadLog)
												: dynamicModule.status === "readyForUse"	? qsTr("Loaded and ready for use!")
												: dynamicModule.status === "error"			? qsTr("Error occurred!")
																							: qsTr("Not ready for use?")

							anchors
							{
								left			: parent.left
								right			: minusButton.left
								verticalCenter	: parent.verticalCenter
							}
						}

						MenuButton
						{
							z:				1
							id:				minusButton
							visible:		!isBundled && !isSpecial
							iconSource:		hovered ? jaspTheme.iconPath + "/delete_icon.png" : jaspTheme.iconPath + "/delete_icon_gray.png"  // icon from https://icons8.com/icon/set/delete/material
							width:			visible ? height : 0
							onClicked:		dynamicModules.uninstallJASPModule(moduleName)
							toolTip:		qsTr("Uninstall module ") + displayText
							anchors
							{
								right			: parent.right
								verticalCenter	: parent.verticalCenter
							}
						}
					}
				}
			}
		}

		JASPScrollBar
		{
			id:				vertScroller
			flickable:		modulesFlick
			manualAnchor:	true
			vertical:		true
			anchors
			{
				top:	modulesFlick.top
				right:	parent.right
				bottom:	modulesFlick.bottom
			}
		}

		focus: true

		Item
		{
			id:			dropShadow
			y:			0
			x:			-width
			height:		parent.height
			width:		jaspTheme.shadowRadius

			Rectangle
			{
				anchors.centerIn: parent
				rotation:	90
				gradient:	Gradient {
					GradientStop { position: 0.0; color: jaspTheme.shadow }
					GradientStop { position: 1.0; color: "transparent" } }
				height:		dropShadow.width
				width:		dropShadow.height
			}
		}
	}
}
