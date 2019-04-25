import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0
import FileOperation 1.0

FocusScope
{
	id: fileMenu

	Keys.onEscapePressed: fileMenuModel.visible = false;

	function updateNavigationKeys()
	{
		for (var i = 0; i < actionRepeaterId.count; i++)
		{
			var nextActElt = (i < (actionRepeaterId.count - 1) ? actionRepeaterId.itemAt(i + 1) : actionRepeaterId.itemAt(0)).children[0]

			actionRepeaterId.itemAt(i).children[0].KeyNavigation.down	= nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.tab	= nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.right	= resourceRepeaterId.count > 0 ? resourceRepeaterId.itemAt(0).children[0] : null
		}

		for (var j = 0; j < resourceRepeaterId.count; j++)
		{
			var nextResElt = (j < (resourceRepeaterId.count- 1) ? resourceRepeaterId.itemAt(j + 1) : resourceRepeaterId.itemAt(0)).children[0]

			resourceRepeaterId.itemAt(j).children[0].KeyNavigation.down	= nextResElt
			resourceRepeaterId.itemAt(j).children[0].KeyNavigation.tab	= nextResElt

			if (selectedActionMenu !== null)
				resourceRepeaterId.itemAt(j).children[0].KeyNavigation.left = selectedActionMenu
		}
	}

	function waitForClickButton(typeRole)
	{
		return typeRole === FileOperation.Close || typeRole === FileOperation.Save || typeRole === FileOperation.About;
	}

	function actionHasSubmenu(typeRole)
	{
		return !waitForClickButton(typeRole)
	}

	function showToolSeperator(typeRole)
	{
		return typeRole === FileOperation.Close || typeRole === FileOperation.Preferences || typeRole === FileOperation.About
	}

	width:		slidePart.width
	height:		600
	z:			1
	visible:	fileMenuAnimation.running ? actionMenu.x + actionMenu.width > 0 : fileMenuModel.visible

	property int  actionButtionHeight:		35 * preferencesModel.uiScale
	property int  resourceButtonHeight:		1.5 * actionButtionHeight
	property int  nbColumns:				1 + (selectedActionMenu !== null && selectedActionMenu.hasResourceMenu ? 1 : 0 )
	property int  colWidths:				150 * preferencesModel.uiScale
	property var  selectedActionMenu:		null

	focus: fileMenuModel.visible

	Item
	{
		id:		slidePart

		x:		!fileMenuModel.visible ? -(resourceScreen.otherColumnsWidth + resourceScreen.width) : 0
		width:	resourceScreen.x + resourceScreen.width
		height: fileMenu.height

		Behavior on x
		{
			PropertyAnimation
			{
				id:				fileMenuAnimation
				duration:		Theme.fileMenuSlideDuration
				easing.type:	Easing.OutCubic
			}
		}

		Rectangle
		{
			id:				actionMenu
			anchors.left:	parent.left
			width:			fileMenu.colWidths //fileMenuModel.visible ?  : 0
			height:			parent.height
			z:				2

			color:			Theme.fileMenuColorBackground
			border.width:	1
			border.color:	Theme.uiBorder

			onVisibleChanged:	if (visible) actionRepeaterId.itemAt(0).getButton().forceActiveFocus()

			Column
			{
				id: fileAction
				spacing: 4
				width: parent.width - Theme.generalAnchorMargin

				anchors
				{
					top: parent.top
					topMargin: 5
					horizontalCenter: parent.horizontalCenter
				}

				Repeater
				{
					id: actionRepeaterId

					model: fileMenuModel.actionButtons

					Item
					{
						id:				itemActionMenu
						width:			parent.width - 6
						anchors.left:	parent.left
						height:			actionButtionHeight + actionToolSeperator.height
						enabled:		enabledRole

						function getButton()
						{
							return actionMenuButton
						}

						MenuButton
						{
							id:				actionMenuButton

							isIcon:				false
							hasSubMenu:			hasResourceMenu
							clickOnHover:		true
							clickWhenFocussed:	!waitForClickButton(typeRole)

							width:			itemActionMenu.width
							height:			actionButtionHeight
							anchors
							{
								leftMargin: 3
								left:		itemActionMenu.left
							}

							text:			nameRole
							selected:		selectedActionMenu === actionMenuButton

							property bool hasResourceMenu:	actionHasSubmenu(typeRole)
							property int myTypeRole:		typeRole

							onActiveFocusChanged:
							{
								if (activeFocus)
								{
									selectedActionMenu = actionMenuButton
									fileMenuModel.fileoperation = typeRole
									updateNavigationKeys()
								}
							}

							Connections
							{
								target: fileMenuModel
								onActionButtonSelected: if(typeRole === action) actionMenuButton.forceActiveFocus(); //If setting from cpp!
							}

							onClicked:
							{
								if (typeRole === FileOperation.About)
								{
									fileMenuModel.showAboutRequest()
									fileMenuModel.visible = false;
								}
								else
								{
									fileMenuModel.actionButtons.buttonClicked(typeRole)
									updateNavigationKeys()
								}
							}
						}

						ToolSeparator
						{
							id:					actionToolSeperator
							anchors.top:		actionMenuButton.bottom
							width:				actionMenuButton.width
							anchors.topMargin:	showToolSeperator(typeRole) ? 3 : 0
							anchors.left:		actionMenuButton.left

							orientation:		Qt.Horizontal
							visible:			showToolSeperator(typeRole)
						}
					}
				}
			}
		}

		Rectangle
		{
			id:				resourceMenu
			width:			fileMenu.colWidths
			anchors.left:	actionMenu.right
			height:			parent.height

			color:			Theme.fileMenuColorBackground
			border.width:	1
			border.color:	Theme.uiBorder
			z:				1

			property bool hasButtons: resourceRepeaterId.count > 0

			visible: hasButtons

			Column
			{
				id:							resourceLocation

				anchors.top:				parent.top
				anchors.topMargin:			5 * preferencesModel.uiScale
				anchors.horizontalCenter:	parent.horizontalCenter
				width:						parent.width - Theme.generalAnchorMargin

				spacing:					6 * preferencesModel.uiScale

				Repeater
				{
					id: resourceRepeaterId

					model: fileMenuModel.resourceButtonsVisible

					Item
					{

						id:					itemResourceMenu
						width:				parent.width - 6
						height:				resourceButtonHeight
						anchors.leftMargin: 3
						anchors.left:		parent.left
						enabled:			enabledRole

						MenuButton
						{
							id:				resourceButton
							isIcon:			false
							hasSubMenu:		true
							width:			parent.width
							height:			parent.height
							anchors.left:	parent.left

							text:			nameRole
							clickOnHover:	true
							selected:		activeFocus

							onClicked:		fileMenuModel.resourceButtonsVisible.clicked(typeRole)
						}

					}
				}
			}
		}

		focus: true

		Item
		{
			id:		dropShadow

			y:		0
			x:		resourceScreen.x + resourceScreen.width
			height:	resourceScreen.height
			width:	Theme.shadowRadius

			visible: resourceScreen.visible
			z: -3

			Rectangle
			{
				anchors.centerIn: parent
				height: dropShadow.width
				width: dropShadow.height

				rotation: -90
				gradient: Gradient
				{
					GradientStop
					{
						position: 0.0
						color: Theme.shadow
					}
					GradientStop
					{
						position: 1.0
						color: "transparent"
					}
				}
			}
		}

		Rectangle
		{
			property real otherColumnsWidth: fileMenu.colWidths * fileMenu.nbColumns
			property bool aButtonVisible: selectedActionMenu !== null && selectedActionMenu.hasResourceMenu && fileMenuModel.resourceButtons.currentQML !== ''

			property real desiredWidth: Math.min(mainWindowRoot.width - otherColumnsWidth, 600 * preferencesModel.uiScale)
			property real desiredX:     otherColumnsWidth - (aButtonVisible ? 0 : desiredWidth)

			property string previousQML: ""
			property string currentQML: ""

			id:				resourceScreen

			x:				desiredX
			width:			desiredWidth
			height:			parent.height

			border.width:	1
			border.color:	Theme.grayDarker
			color:			Theme.fileMenuColorBackground
			z:				-2

			Behavior on x
			{
				PropertyAnimation
				{
					id:				fileMenuResourceLoaderAnimation
					duration:		Theme.fileMenuSlideDuration
					easing.type:	Easing.OutCubic
				}
			}

			onXChanged:
				if(resourceScreen.x === resourceScreen.desiredX && resourceScreen.currentQML === "" && resourceScreen.previousQML !== "")
					resourceScreen.previousQML = "";

			Connections
			{
				target:	fileMenuModel.resourceButtons
				onCurrentQMLChanged:
				{
					resourceScreen.previousQML = resourceScreen.currentQML
					resourceScreen.currentQML  = fileMenuModel.resourceButtons.currentQML
				}
			}

			Loader
			{
				id:					showSelectedSubScreen
				anchors.fill:		parent
				source:				resourceScreen.currentQML === "" && resourceScreen.x > resourceScreen.desiredX ? resourceScreen.previousQML : resourceScreen.currentQML
			}
		}

		MouseArea
		{
			id:				gottaCatchEmAll //Clicks that is
			anchors.fill:	parent
			z:				-6
		}
	}
}
