import QtQuick
import QtQuick.Controls as QTC
import QtQuick.Layouts
import JASP.Controls
import "./FileMenu"

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

	Keys.onEscapePressed:	closeAndFocusRibbon();
	Keys.onRightPressed:	closeAndFocusRibbon();
	Keys.onLeftPressed:		closeAndFocusRibbon();

	function closeAndFocusRibbon()
	{
		opened       = false;
		ribbon.focus = true;
	}

	Keys.onTabPressed:		{ increaseIndex(); }
	Keys.onBacktabPressed:	{ decreaseIndex(); }
	Keys.onDownPressed:		{ increaseIndex(); }
	Keys.onUpPressed:		{ decreaseIndex(); }

	function increaseIndex()
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

	function decreaseIndex()
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

	onCurrentIndexChanged:
	{
		if (currentIndex < 0)
			vertScroller.scrollToElement(addModuleButton)
		else
			vertScroller.scrollToElement(repeater.itemAt(currentIndex))
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
			contentHeight:			workspaceSpecs.visible ? workspaceSpecs.height : modules.height
			contentWidth:			width

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
				id:			workspaceSpecs
				spacing:	jaspTheme.rowSpacing
				width:		slidePart.width - vertScroller.width
				visible:	ribbonModel.dataMode

				MenuHeader
				{
					headertext:	qsTr("Workspace settings")
					anchorMe:	false
					width:		parent.width - (2 * jaspTheme.generalMenuMargin)
					x:			jaspTheme.generalMenuMargin
				}


				PrefsGroupRect
				{
					spacing:	jaspTheme.rowSpacing
					width:		parent.width - (jaspTheme.generalAnchorMargin * 2)
					color:		jaspTheme.uiBackground


					Text
					{
						anchors.margins:	3 * preferencesModel.uiScale
						text:				qsTr("Name: %1").arg(workspaceModel.name)
					}

					TextArea
					{
						anchors.margins:	3 * preferencesModel.uiScale
						title:				qsTr("Description: ")
						height:				100 * jaspTheme.uiScale
						control.padding:	3 * jaspTheme.uiScale
						text:				workspaceModel.description
						onEditingFinished: 	if(workspaceModel.description !== text) workspaceModel.description = text
						applyScriptInfo:	""
						placeholderText:	"..."
						undoModel:			columnModel
						useTabAsSpaces:		false
						nextTabItem:		missingValues
						wrapMode:			TextEdit.Wrap
						anchors
						{
							left:			parent.left
							right:			parent.right
							margins:		jaspTheme.generalAnchorMargin
						}

					}
				}

				PrefsMissingValues
				{
					id:								missingValues
					width:							parent.width - (jaspTheme.generalAnchorMargin * 2)
					x:								jaspTheme.generalAnchorMargin
					model:							workspaceModel
					resetButtonTooltip:				qsTr("Reset missing values with the ones set in Data Preferences")
					showWorkspaceMissingValues:		false
				}
			}

			Column
			{
				id:			modules
				spacing:	4  * preferencesModel.uiScale
				width:		slidePart.width - vertScroller.width
				visible:	!ribbonModel.dataMode

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
					activeFocusOnTab:	false
				}

				QTC.ToolSeparator
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
					visible:			preferencesModel.developerMode && !preferencesModel.directLibpathEnabled
					enabled:			dynamicModules.developersModuleInstallButtonEnabled
					focus:				currentIndex === -1
					activeFocusOnTab:	false

					readonly property bool folderSelected: preferencesModel.developerFolder != ""
				}
				
				MenuButton
				{
					id:					addDeveloperModuleDirectButton
					text:				moduleSelected ? qsTr("Install Developer Module") : qsTr("Select a Developer Module")
					width:				modules.buttonWidth
					height:				modules.buttonHeight
					anchors.leftMargin: modules.buttonMargin
					anchors.left:		parent.left
					onClicked: 			moduleSelected ? dynamicModules.installJASPDeveloperModule() : fileMenuModel.showAdvancedPreferences()
					toolTip:			moduleSelected ? qsTr("Install selected developer module") : qsTr("Select a developer module by filling in the relevant preferences")
					visible:			preferencesModel.developerMode && preferencesModel.directLibpathEnabled
					focus:				currentIndex === -1
					activeFocusOnTab:	false

					readonly property bool moduleSelected: preferencesModel.directLibpathEnabled && preferencesModel.directLibpathFolder != "" && preferencesModel.directDevModName != ""
				}

				QTC.ToolSeparator
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
							forwardKeys:		true
							Keys.forwardTo:		[modulesMenu]

							toolTip:			isSpecial									? qsTr("Ready") //Always ready!
												: dynamicModule.installing					? qsTr("Installing: %1\n").arg(dynamicModule.installLog)
												: dynamicModule.loading						? qsTr("Loading: %1\n").arg(dynamicModule.loadLog)
												: dynamicModule.status === "readyForUse"	? qsTr("Loaded and ready for use!")
												: dynamicModule.status === "error"			? qsTr("Error occurred!")
																							: qsTr("Not ready for use?")

							anchors
							{
								left			: parent.left
                                right			: refreshButton.left
								verticalCenter	: parent.verticalCenter
							}
						}


                        MenuButton
                        {
                            z:				1
                            id:				refreshButton
                            visible:		!isBundled && !isSpecial
                            iconSource:		jaspTheme.iconPath + "/redo.svg"
                            width:			visible ? height : 0
                            onClicked:		dynamicModules.refreshDeveloperModule();
                            toolTip:		qsTr("Refresh developer module ") + displayText
                            anchors
                            {
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
