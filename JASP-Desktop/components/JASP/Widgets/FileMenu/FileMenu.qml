import QtQuick 2.11
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0
import FileOperation 1.0

FocusScope
{
	id:			fileMenu

	function updateNavigationKeys()
	{
		for (var i = 0; i < actionRepeaterId.count; i++)
		{
			var nextActElt = (i < (actionRepeaterId.count - 1) ? actionRepeaterId.itemAt(i+1) : actionRepeaterId.itemAt(0)).children[0];
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.down = nextActElt;
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.tab = nextActElt;
			actionRepeaterId.itemAt(i).children[0].KeyNavigation.right = resourceRepeaterId.itemAt(0);
		}

		for (var j = 0; j < resourceRepeaterId.count; j++)
		{
			var nextResElt = (j < (resourceRepeaterId.count - 1) ? resourceRepeaterId.itemAt(j+1) : resourceRepeaterId.itemAt(0));
			resourceRepeaterId.itemAt(j).KeyNavigation.down = nextResElt;
			resourceRepeaterId.itemAt(j).KeyNavigation.tab = nextResElt;
			resourceRepeaterId.itemAt(j).KeyNavigation.left = actionRepeaterId.itemAt(lastfocus_action).children[0];
		}

	}

	function waitForClickButton (typeRole)
	{
		if (typeRole === FileOperation.Close) return true;
		if (typeRole === FileOperation.Save) return true;
		return false;
	}

	function showToolSeperator (typeRole)
	{
		if (typeRole === FileOperation.Close) return true;
		if (typeRole === FileOperation.Preferences) return true;
		if (typeRole === FileOperation.About) return true;
		return false;
	}

	function actionHasSubmenu (typeRole)
	{
		if (typeRole === FileOperation.Close) return false;
		if (typeRole === FileOperation.Save) return false;
		if (typeRole === FileOperation.About) return false;
		return true;
	}

	width:		slidePart.width
	height:		600
	z:			1
	visible:	fileMenuAnimation.running ? actionMenu.x + actionMenu.width > 0 : fileMenuModel.visible

	property variant resourcesbuttons:		["Recent Files", "Current File", "Computer", "OSF", "Data Library"]
	property int action_button_height:		35 * preferencesModel.uiScale
	property int resource_button_height:	1.5 * action_button_height
	property int colWidths:					150 * preferencesModel.uiScale
	property int lastfocus_action:			0
	property bool focusonresources:			false

	focus: fileMenuModel.visible

	Item
	{
		id:		slidePart

		x:		fileMenuModel.visible ? 0 : -(colWidths * 2)
		width:	(resourceScreen.x + resourceScreen.width)
		height:	fileMenu.height

		Behavior on x { PropertyAnimation { id: fileMenuAnimation; duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }

		MouseArea
		{
			id:				gottaCatchEmAll //Clicks that is
			anchors.fill:	parent
			z:				-6
		}

		Timer {

			id: waitOnhoverTimer  //Resource Menu is refreshed after action navigation. Ignore that Onhover event.

			interval: 750
			running: false
			repeat: false
		}


		Rectangle
		{
			id:				actionMenu
			anchors.left:	parent.left
			width:			fileMenu.colWidths //fileMenuModel.visible ?  : 0
			height:			parent.height

			color:			Theme.uiBackground
			border.width:	1
			border.color:	Theme.uiBorder

			Column
			{
				id:					fileAction
				spacing:			4
				width:				parent.width - Theme.generalAnchorMargin

				anchors
				{
					top:				parent.top
					topMargin:			5
					horizontalCenter:	parent.horizontalCenter
				}

				Repeater
				{
					id: actionRepeaterId

					model:	fileMenuModel.actionButtons

					Component.onCompleted:
					{
						updateNavigationKeys()
						itemAt(0).focus = true;
					}

					Item{
						id:				itemActionMenu
						width:			parent.width-6
						anchors.left:	parent.left
						height:			action_button_height + actionToolSeperator.height


						MenuButton
						{
							property bool ignoreFocusChanged: false
							property bool highlight: activeFocus || (focusonresources && index ===lastfocus_action)

							id:					actionMenuButton
							width:				itemActionMenu.width
							height:				action_button_height
							anchors.leftMargin: 3
							anchors.left:		itemActionMenu.left

							text:				nameRole
							color:				highlight ? Theme.buttonColorPressed : "transparent"
							selected:			fileMenuModel.fileoperation === typeRole
							enabled:			enabledRole

							onHoveredChanged:
							{
								if (hovered)
								{
									if (!activeFocus)
									{
										ignoreFocusChanged = true;
										focus = true;
										lastfocus_action = index
										focusonresources = false
									}

									if (!waitForClickButton(typeRole))
										fileMenuModel.actionButtons.buttonClicked(typeRole);
									updateNavigationKeys();
									ignoreFocusChanged = false;
								}
							}

							onFocusChanged:
							{
								if (activeFocus && !ignoreFocusChanged )
								{
									waitOnhoverTimer.restart();
									lastfocus_action = index
									focusonresources = false
									if (!waitForClickButton(typeRole))
										fileMenuModel.actionButtons.buttonClicked(typeRole);
									updateNavigationKeys();
								}
							}

							onClicked:
							{
								fileMenuModel.actionButtons.buttonClicked(typeRole);
								if (typeRole===FileOperation.About)
									fileMenuModel.showAboutRequest();
								updateNavigationKeys();
							}
						}

						Image {
							id: rightMenuIndicator

							anchors.verticalCenter: actionMenuButton.verticalCenter
							anchors.right : actionMenuButton.right
							height:	0.4 * actionMenuButton.height
							width: height

							source: "qrc:/icons/large-arrow-right.png"
							visible: actionHasSubmenu(typeRole)
							opacity: !actionMenuButton.highlight ? 0.3 : 1
						}

						ToolSeparator
						{
							id:					actionToolSeperator
							anchors.top:		actionMenuButton.bottom
							width :				actionMenuButton.width
							anchors.topMargin: showToolSeperator(typeRole) ? 3 : 0
							anchors.left:		actionMenuButton.left

							orientation: Qt.Horizontal
							visible: showToolSeperator(typeRole)
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

			color:			Theme.uiBackground
			border.width:	1
			border.color:	Theme.uiBorder

			Column
			{
				id: resourceLocation

				anchors.top:				parent.top
				anchors.topMargin:			5
				anchors.horizontalCenter:	parent.horizontalCenter
				width:						parent.width - Theme.generalAnchorMargin

				spacing:					6

				Repeater
				{
					id: resourceRepeaterId

					model:					fileMenuModel.resourceButtonsVisible

					MenuButton
					{
						property bool ignoreFocusChanged: false

						id:					resourceButton
						width:				parent.width-6
						height:				resource_button_height
						anchors.leftMargin: 3
						anchors.left:		parent.left

						text:				nameRole
						color:				_pressed || activeFocus ? Theme.buttonColorPressed :	_showHovered ? "transparent"						: "transparent"
						selected:			qmlRole === fileMenuModel.resourceButtons.currentQML
						enabled:			true

						onHoveredChanged:
						{
							if (hovered )
							{
								if (!waitOnhoverTimer.running)
								{
									if (!activeFocus)
									{
										ignoreFocusChanged = true;
										focus = true;
										focusonresources = true;
									}
									fileMenuModel.resourceButtonsVisible.clicked(typeRole);
									ignoreFocusChanged = false;
								}
							}
						}

						onFocusChanged:
						{
							{
								fileMenuModel.resourceButtonsVisible.clicked(typeRole);
								focusonresources = true;
							}

						}

						onClicked:
						{
							fileMenuModel.resourceButtonsVisible.clicked(typeRole);
						}
					}
				}
			}
		}

		focus: true

		Item
		{
			id:			dropShadow

			y:			0
			x:			resourceScreen.x + resourceScreen.width
			height:		resourceScreen.height
			width:		Theme.shadowRadius

			visible:	resourceScreen.visible

			Rectangle
			{
				anchors.centerIn: parent
				height:		dropShadow.width
				width:		dropShadow.height

				rotation:	-90
				gradient:	Gradient {
					GradientStop { position: 0.0; color: Theme.shadow }
					GradientStop { position: 1.0; color: "transparent" } }
			}
		}

		Rectangle
		{
			property real otherColumnsWidth:	fileMenu.colWidths * 2
			property bool aButtonVisible:		fileMenuModel.resourceButtons.currentQML !== ''

			id: resourceScreen

			x:				otherColumnsWidth - (aButtonVisible && fileMenuModel.visible ? 0 : width)
			width:			fileMenu.parent.width - otherColumnsWidth
			height:			parent.height
			visible:		fileMenuModel.visible || x + width > otherColumnsWidth + 1

			border.width:	1
			border.color:	Theme.grayDarker
			color:			Theme.uiBackground
			z:				-2

			Behavior on x { PropertyAnimation { duration: Theme.fileMenuSlideDuration; easing.type: Easing.OutCubic  } }
			onXChanged: if(x + width <= otherColumnsWidth && !fileMenuModel.visible) fileMenuModel.resourceButtons.currentQML = ""

			Loader
			{
				id:					showSelectedSubScreen
				anchors.fill:		parent
				source:				fileMenuModel.resourceButtons.currentQML
			}
		}
	}
}
