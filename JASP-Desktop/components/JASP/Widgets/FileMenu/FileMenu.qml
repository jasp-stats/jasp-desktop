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
			var nextActElt = actionRepeaterId.itemAt((i + 1) % actionRepeaterId.count).children[0]

			actionRepeaterId.itemAt(i).children[0].KeyNavigation.down	= nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.tab	= nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.right	= resourceRepeaterId.count > 0 ? resourceRepeaterId.itemAt(0).children[0] : null
		}

		for (var j = 0; j < resourceRepeaterId.count; j++)
		{
			var nextResElt = resourceRepeaterId.itemAt((j + 1) % resourceRepeaterId.count).children[0]

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
	property int  colWidths:				150
	property var  selectedActionMenu:		null

	onSelectedActionMenuChanged:	updateNavigationKeys();

	Connections
	{
		target:				fileMenuModel
		onVisibleChanged:	if(fileMenuModel.visible) fileMenu.forceActiveFocus(); else fileMenu.focus = false;
	}

	Item
	{
		id:		slidePart

		property real desiredX: !fileMenuModel.visible ? -(resourceScreen.otherColumnsWidth + resourceScreen.width) : 0

		x:		desiredX
		width:	(fileMenu.nbColumns * fileMenu.colWidths * preferencesModel.uiScale) + (resourceScreen.aButtonVisible ? resourceScreen.width : 0)
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
			width:			fileMenu.colWidths * preferencesModel.uiScale
			height:			parent.height
			z:				2

			color:			Theme.fileMenuColorBackground
			border.width:	1
			border.color:	Theme.uiBorder

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
						width:			parent.width - (6 * preferencesModel.uiScale)
						anchors.left:	parent.left
						height:			actionButtionHeight + actionToolSeperator.height
						enabled:		enabledRole

						MenuButton
						{
							id:					actionMenuButton

							hasSubMenu:			hasResourceMenu
							clickOnHover:		true
							clickWhenFocussed:	!waitForClickButton(typeRole)

							width:				itemActionMenu.width
							height:				actionButtionHeight
							anchors
							{
								leftMargin: 3  * preferencesModel.uiScale
								left:		itemActionMenu.left
							}

							text:			nameRole
							selected:		selectedRole

							property bool hasResourceMenu:	actionHasSubmenu(typeRole)

							onSelectedChanged: if(selected) selectedActionMenu = actionMenuButton;
							onClicked:			fileMenuModel.actionButtons.selectedAction = typeRole
							Keys.onLeftPressed: fileMenuModel.visible = false;
						}

						ToolSeparator
						{
							id:					actionToolSeperator
							anchors.top:		actionMenuButton.bottom
							width:				actionMenuButton.width
							anchors.topMargin:	(showToolSeperator(typeRole) ? 3 : 0)  * preferencesModel.uiScale
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
			id:					resourceMenu

			width:				fileMenu.colWidths * preferencesModel.uiScale
			height:				parent.height
			anchors.left:		actionMenu.right
			anchors.leftMargin: hasButtons ? 0 : - fileMenu.colWidths * preferencesModel.uiScale

			Behavior on anchors.leftMargin
			{
				PropertyAnimation
				{
					id:				resourceMenuAnimation
					duration:		Theme.fileMenuSlideDuration
					easing.type:	Easing.OutCubic
				}
			}

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
					id:		resourceRepeaterId
					model:	fileMenuModel.resourceButtonsVisible

					Item
					{

						id:					itemResourceMenu
						width:				parent.width - (6 * preferencesModel.uiScale)
						height:				resourceButtonHeight
						anchors.leftMargin: 3 * preferencesModel.uiScale
						anchors.left:		parent.left
						enabled:			enabledRole

						MenuButton
						{
							id:				resourceButton
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
			property real otherColumnsWidth:	fileMenu.colWidths * fileMenu.nbColumns * preferencesModel.uiScale
			property bool aButtonVisible:		selectedActionMenu !== null && selectedActionMenu.hasResourceMenu && fileMenuModel.resourceButtons.currentQML !== ''

			property real desiredWidth:			Math.min(mainWindowRoot.width - otherColumnsWidth, 600 * preferencesModel.uiScale)
			property real desiredX:				otherColumnsWidth - (aButtonVisible ? 0 : desiredWidth)

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

				onLoaded:			if(resourceScreen.currentQML !== "") forceActiveFocus();
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
