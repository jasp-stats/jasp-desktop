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
			var nextActElt = (i < (actionRepeaterId.count- 1) ? actionRepeaterId.itemAt(i + 1) : actionRepeaterId.itemAt(0)).children[0]
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.down = nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.tab = nextActElt
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.right = resourceRepeaterId.itemAt(0).children[0]
		}

		for (var j = 0; j < resourceRepeaterId.count; j++)
		{
			var nextResElt = (j < (resourceRepeaterId.count- 1) ? resourceRepeaterId.itemAt(j + 1) : resourceRepeaterId.itemAt(0)).children[0]
			resourceRepeaterId.itemAt(j).children[0].KeyNavigation.down = nextResElt
			resourceRepeaterId.itemAt(j).children[0].KeyNavigation.tab = nextResElt
			if (selectedActionMenu)
				resourceRepeaterId.itemAt(j).children[0].KeyNavigation.left = selectedActionMenu
		}
	}

	function waitForClickButton(typeRole)
	{
		if (typeRole === FileOperation.Close)
			return true
		if (typeRole === FileOperation.Save)
			return true
		if (typeRole === FileOperation.About)
			return true
		return false
	}

	function actionHasSubmenu(typeRole)
	{
		return !waitForClickButton(typeRole)
	}

	function showToolSeperator(typeRole)
	{
		if (typeRole === FileOperation.Close)
			return true
		if (typeRole === FileOperation.Preferences)
			return true
		if (typeRole === FileOperation.About)
			return true
		return false
	}

	width: slidePart.width
	height: 600
	z: 1
	visible: fileMenuAnimation.running ? actionMenu.x + actionMenu.width > 0 : fileMenuModel.visible

	property int actionButtionHeight: 35 * preferencesModel.uiScale
	property int resourceButtonHeight: 1.5 * actionButtionHeight
	property int nbColumns: selectedActionMenu ? (selectedActionMenu.hasResourceMenu ? 2 : 1) : 2
	property int colWidths: 150 * preferencesModel.uiScale
	property var selectedActionMenu: false

	focus: fileMenuModel.visible

	MouseArea
	{
		id: gottaCatchEmAll //Clicks that is
		anchors.fill: parent
		z: -6
	}

	Item
	{
		id: slidePart

		x: 0
		width: !fileMenuModel.visible ? 0 : resourceScreen.x + resourceScreen.width
		height: fileMenu.height
		clip: true

		Behavior on width
		{
			PropertyAnimation
			{
				id: fileMenuAnimation
				duration: Theme.fileMenuSlideDuration
				easing.type: Easing.OutCubic
			}
		}

		Rectangle
		{
			id: actionMenu
			anchors.left: parent.left
			width: fileMenu.colWidths //fileMenuModel.visible ?  : 0
			height: parent.height

			color: Theme.fileMenuColorBackground
			border.width: 1
			border.color: Theme.uiBorder

			onVisibleChanged:
			{
				if (visible) {
					actionRepeaterId.itemAt(0).getButton().forceActiveFocus()
				}
			}

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
						id: itemActionMenu
						width: parent.width - 6
						anchors.left: parent.left
						height: actionButtionHeight + actionToolSeperator.height
						enabled: enabledRole

						function getButton()
						{
							return actionMenuButton
						}

						MenuButton
						{
							clickOnHover: true
							isIcon: false
							hasSubMenu: hasResourceMenu

							id: actionMenuButton
							width: itemActionMenu.width
							height: actionButtionHeight
							anchors.leftMargin: 3
							anchors.left: itemActionMenu.left

							text: nameRole
							selected: selectedActionMenu === actionMenuButton

							property bool hasResourceMenu: actionHasSubmenu(typeRole)
							property int myTypeRole: typeRole
							clickWhenFocussed: !waitForClickButton(typeRole)

							onActiveFocusChanged:
							{
								if (activeFocus)
								{
									selectedActionMenu = actionMenuButton
									updateNavigationKeys()
								}
							}

							onClicked:
							{
								if (typeRole === FileOperation.About)
									fileMenuModel.showAboutRequest()
								else
									fileMenuModel.actionButtons.buttonClicked(typeRole)
								updateNavigationKeys()
							}
						}

						ToolSeparator
						{
							id: actionToolSeperator
							anchors.top: actionMenuButton.bottom
							width: actionMenuButton.width
							anchors.topMargin: showToolSeperator(
												   typeRole) ? 3 : 0
							anchors.left: actionMenuButton.left

							orientation: Qt.Horizontal
							visible: showToolSeperator(typeRole)
						}
					}
				}
			}
		}

		Rectangle
		{
			id: resourceMenu
			width: fileMenu.colWidths
			anchors.left: actionMenu.right
			height: parent.height

			color: Theme.fileMenuColorBackground
			border.width: 1
			border.color: Theme.uiBorder

			Column
			{
				id: resourceLocation

				anchors.top: parent.top
				anchors.topMargin: 5
				anchors.horizontalCenter: parent.horizontalCenter
				width: parent.width - Theme.generalAnchorMargin

				spacing: 6

				Repeater
				{
					id: resourceRepeaterId

					model: fileMenuModel.resourceButtonsVisible

					Item
					{

						id: itemResourceMenu
						width: parent.width - 6
						height: resourceButtonHeight
						anchors.leftMargin: 3
						anchors.left: parent.left
						enabled: enabledRole

						MenuButton
						{
							id: resourceButton
							isIcon: false
							hasSubMenu: true
							width: parent.width
							height: parent.height
							anchors.left: parent.left

							text: nameRole
							clickOnHover: true
							selected: activeFocus

							onClicked: fileMenuModel.resourceButtonsVisible.clicked(typeRole)
						}

					}
				}
			}
		}

		focus: true

		Item
		{
			id: dropShadow

			y: 0
			x: resourceScreen.x + resourceScreen.width
			height: resourceScreen.height
			width: Theme.shadowRadius

			visible: resourceScreen.visible

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
			property bool aButtonVisible: selectedActionMenu && selectedActionMenu.hasResourceMenu && fileMenuModel.resourceButtons.currentQML !== ''

			id: resourceScreen

			x: otherColumnsWidth
			width: !aButtonVisible ? 0 : Math.min(fileMenu.parent.width - otherColumnsWidth, 600 * preferencesModel.uiScale)
			height: parent.height

			border.width: 1
			border.color: Theme.grayDarker
			color: Theme.fileMenuColorBackground
			z: -2

			Loader
			{
				id: showSelectedSubScreen
				anchors.fill: parent
				source: fileMenuModel.resourceButtons.currentQML
			}
		}
	}
}
