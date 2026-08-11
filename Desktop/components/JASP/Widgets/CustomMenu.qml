//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import QtQuick.Controls
import QtQuick.Effects
import JASP.Controls				as JASPControl

FocusScope
{
	id							: menu
	width						: menuRectangle.width
	height						: menuRectangle.height
	visible						: showMe
	Accessible.role				: Accessible.Menu
	Accessible.name				: menuTitle
	Accessible.ignored			: !showMe
	x							: Math.min(sourcePos.x + realOffsetX, sceneWidth - (width  + 2) ) // Move the custom menu to the right if there is not enough space
	y							: sourcePos.y + realOffsetY
	property var	props		: undefined
	property bool	hasIcons	: true
	property bool	hasSubMenus	: false
	property int	realOffsetX	: menuMinIsMin ? Math.max(menuOffset.x + menuScroll.x, 0) : menuOffset.x + menuScroll.x
	property int	realOffsetY	: menuMinIsMin ? Math.max(menuOffset.y + menuScroll.y, 0) : menuOffset.y + menuScroll.y
	property point	menuOffset	: "0,0"
	property point	menuScroll	: "0,0" // Extra offset due to the scrolling where the custom menu is anchored
	property point	sourcePos	: "0,0"
	property real	sceneWidth	: mainWindowRoot.width
	property real	sceneHeight	: mainWindowRoot.height
	property bool	showMe		: false
	property var    sourceItem  : null
	property bool	menuMinIsMin: false // If set to true, this prevents the CustomMenu from going out of the scene by having a negative offset
	property point	scrollOri	: "0,0" //Just for other qmls to use as a general storage of the origin of their scrolling
	property bool	isSubMenu	: false
	property string menuTitle	: "Analysis menu"

	property int	currentIndex: -1

	property bool _hadFocus: false

	onSourceItemChanged: { menu.currentIndex = -1; }

	onActiveFocusChanged:
	{
		if (activeFocus)
			_hadFocus = true
		else if (_hadFocus && showMe && !isSubMenu)
		{
			_hadFocus = false
			closeMenu()
		}
	}

	Connections
	{
		// As the sourcePos is calculated with the mapToItem function, there is no binding. So re-calculate the sourcePos each time the X or Y of the sourceItem changes.
		target:	menu.sourceItem
		function onXChanged()
		{
			setSourcePos()
		}
		function onYChanged()
		{
			setSourcePos()
		}
	}

	Keys.onPressed: (event)=>
	{
		switch(event.key)
		{
		case Qt.Key_Up:
		case Qt.Key_Backtab:
			navigate(-1);
			event.accepted = true;
			break;
		case Qt.Key_Down:
		case Qt.Key_Tab:
			navigate(1);
			event.accepted = true;
			break;
		case Qt.Key_Left:
			parentNavigate(-1);
			event.accepted = true;
			break;
		case Qt.Key_Right:
			parentNavigate(1);
			event.accepted = true;
			break;
		case Qt.Key_Return:
		case Qt.Key_Space:
			if (currentIndex > -1)
				callMenuAction(currentIndex)
			break;
		case Qt.Key_Escape:
			menu.currentIndex = -1;
			closeMenu();
			break;
		default:
			break;
		}
	}

	onPropsChanged:
	{
		hasIcons	= (menu.props === undefined || "undefined" === typeof(menu.props["hasIcons"]))		? true	: menu.props["hasIcons"]
		hasSubMenus = (menu.props === undefined || "undefined" === typeof(menu.props["hasSubMenus"]))	? false : menu.props["hasSubMenus"]
		menuTitle	= (menu.props === undefined || "undefined" === typeof(menu.props["menuTitle"]))		? "Analysis menu" : menu.props["menuTitle"]

		if (menu.props === undefined || menu.props["model"] !== resultMenuModel)
			resultsJsInterface.runJavaScript("window.setSelection(false);")
	}

	function setSourcePos()
	{
		if (menu.sourceItem)
			menu.sourcePos = menu.sourceItem.mapToItem(null, 1, 1);
	}

	function toggle(item, props, x_offset = 0, y_offset = 0)
	{
		if (item === menu.sourceItem && menu.visible)
			hide()
		else
			show(item, props, x_offset, y_offset);
	}

	function show(item, props, x_offset = 0, y_offset = 0)
	{
		menu.sourceItem     = item;
		setSourcePos()
		menu.props          = props;
		// If the offset is not directly give, a menu should be set just onder the source item, or right beside the source item if it is a submenu.
		menu.menuOffset.x	= x_offset !== 0 ? x_offset : (menu.isSubMenu ? Qt.binding(function() { return item.width; }) : 0)
		menu.menuOffset.y	= y_offset !== 0 ? y_offset : (menu.isSubMenu ? 0 : Qt.binding(function() { return item.height; }))
		menu.menuScroll		= "0,0";
		menu.showMe			= true;
		menu.sceneWidth		= Qt.binding(function() { return mainWindowRoot.width })
		menu.sceneHeight	= Qt.binding(function() { return mainWindowRoot.height })

		menu.forceActiveFocus();
		navigate(1)
	}

	function hide()
	{
		menu.showMe			= false;
		menu.sourceItem     = null;
		menu.props			= undefined;
		menu.menuMinIsMin	= false;
		menu.menuOffset		= "0,0"
		menu.menuScroll		= "0,0"
		menu.sourcePos		= "0,0"
		menu.currentIndex   = -1;
	}

	function navigate(direction)
	{
		if (menu.props["navigateFunc"] === undefined || typeof(menu.props["navigateFunc"]) === "undefined")
			menu.currentIndex = mod(menu.currentIndex + direction, repeater.count)
		else
			menu.currentIndex = menu.props["navigateFunc"](currentIndex, direction);
	}

	function parentNavigate(direction)
	{
		if (menu.props["parentNavigateFunc"] === undefined || typeof(menu.props["parentNavigateFunc"]) === "undefined")
			return;
		menu.props["parentNavigateFunc"](direction);
	}

	function closeMenu()
	{
		if (menu.sourceItem !== null)
		{
			menu.sourceItem.forceActiveFocus();
			if (menu.sourceItem.myMenuOpen !== undefined && typeof(menu.sourceItem.myMenuOpen) !== 'undefined')
				menu.sourceItem.myMenuOpen = false;
		}
		menu.hide();
	}

	function callMenuAction(index)
	{
		menu.props['functionCall'](index)
		if (menu.sourceItem !== null)
			menu.sourceItem.forceActiveFocus()
	}

	function currentMenuItem(index)
	{
		return repeater.itemAt(index).item
	}

	Rectangle
	{
		id				: menuRectangle
		z				: menuShadow.z + 1
		color			: jaspTheme.fileMenuColorBackground
		focus			: true
		width			: column.columnWidth + 2 * jaspTheme.contentMargin + itemScrollbar.width + itemScrollbar.anchors.margins
		implicitHeight	: column.height + 2 * jaspTheme.contentMargin
		height			: (menu.y + implicitHeight) > sceneHeight ? (sceneHeight - menu.y) : implicitHeight // The menu should not exceed the scene

		MouseArea
		{
			anchors.fill	: parent
			acceptedButtons	: Qt.NoButton
			onWheel			: (wheel)=> { wheel.accepted = true; }
		}

		JASPControl.JASPScrollBar
		{
			id				: itemScrollbar
			flickable		: itemFlickable
			manualAnchor	: true
			vertical		: true
			z				: 1337

			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				margins		: 2
			}
		}

		JASPControl.ScrollMoreIndicator 
		{
			id: 		scrollingGuideBottom
			
			anchors
			{
				left: 	parent.left
				right: 	itemScrollbar.left
				bottom: parent.bottom
			}

			extraSpace: itemFlickable.contentHeight - (itemFlickable.contentY + itemFlickable.height)
		}
		
		JASPControl.ScrollMoreIndicator 
		{
			id: 		scrollingGuideTop
			
			anchors
			{
				left: 	parent.left
				right: 	itemScrollbar.left
				top:	parent.top
			}
			
			upsideDown:	true
			extraSpace: itemFlickable.contentY
		}

		Flickable
		{
			id						: itemFlickable
			anchors.fill			: parent
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: menuRectangle.width
			contentHeight			: menuRectangle.implicitHeight

			Item // Need an extra Item round the Column, in order to set the top and left margin
			{
				anchors.fill			: parent
				anchors.topMargin		: jaspTheme.contentMargin
				anchors.leftMargin		: jaspTheme.contentMargin

				Column
				{
					id		: column
					z		: menuRectangle.z + 1
					spacing	: jaspTheme.menuSpacing

					property real columnWidth	: 0

					function computeColumnWidth()
					{
						let maxW = 0

						for (let i = 0; i < repeater.count; ++i)
							if (repeater.itemAt(i) && repeater.itemAt(i).item) // It might be null...
								maxW = Math.max(repeater.itemAt(i).item.implicitWidth, maxW)

						column.columnWidth = maxW
					}

					Repeater
					{
						id		: repeater
						model	: menu.props === undefined ? undefined : menu.props["model"]

						onItemAdded: (index, item)=>
						{
							column.columnWidth = Math.max(column.columnWidth, item.item?.implicitWidth ?? 0)
						}

						delegate: Loader
						{
							sourceComponent :
							{
								if(model.modelData !== undefined)
								{
									if(model.modelData.startsWith("---"))
									{
										if(model.modelData == "---")	return menuSeparator;
										else							return menuGroupTitle;
									}
									return menuDelegate;
								}

								if (model.isSeparator !== undefined && model.isSeparator)			return menuSeparator;
								else if (model.isGroupTitle !== undefined && model.isGroupTitle)	return menuGroupTitle;

								return menuDelegate
							}

							Component
							{
								id: menuDelegate

								Rectangle
								{
									id:		menuItem
									width:	column.columnWidth
									height: jaspTheme.menuItemHeight
									color:	(model.modelData === undefined) && !menuItem.itemEnabled
													? "transparent"
													: mouseArea.pressed || index == currentIndex
														? jaspTheme.buttonColorPressed
														: mouseArea.containsMouse
															? jaspTheme.buttonColorHovered
															: "transparent"

									// The menuItemImage and menuItemText are anchored from the left, and menuItemShortcut from the right
									// The implicitWidth takes care for an itemPadding at the left side (set in menuItemImage or menuItemText) and an itemPadding at the right side
									// The computeColumnWidth function uses this implicitWidth to compute the column width
									implicitWidth			: menuItemText.x + menuItemText.implicitWidth + jaspTheme.itemPadding + (menuItemShortcut.text ? menuItemShortcut.implicitWidth + menuItemShortcut.anchors.rightMargin : 0)
									onImplicitWidthChanged	: column.computeColumnWidth()

									property bool itemEnabled	: menu.props.hasOwnProperty("enabled") ? menu.props["enabled"][index] : (model.modelData !== undefined || model.isEnabled)

									Accessible.role		: Accessible.MenuItem
									Accessible.name		: (model.modelData !== undefined ? model.modelData : displayText)
									Accessible.onPressAction: { if (menuItem.itemEnabled) callMenuAction(index) }

									Image
									{
										id						: menuItemImage
										height					: menu.hasIcons ? (menuItem.height - (2 * jaspTheme.contentMargin)) : 0
										width					: height

										source					: menu.props.hasOwnProperty("icons") ? menu.props["icons"][index] : (model.modelData !== undefined ? "" : menuImageSource)
										smooth					: true
										mipmap					: true
										fillMode				: Image.PreserveAspectFit

										anchors
										{
											left				: parent.left
											leftMargin			: jaspTheme.itemPadding
											verticalCenter		: parent.verticalCenter
										}
									}

									Text
									{
										id						: menuItemText
										text					: model.modelData !== undefined ? model.modelData : displayText
										font					: jaspTheme.font
										color					: menuItem.itemEnabled ? jaspTheme.black : jaspTheme.gray

										anchors
										{
											left				: menu.hasIcons ? menuItemImage.right : parent.left
											leftMargin			: jaspTheme.itemPadding
											verticalCenter		: parent.verticalCenter
										}
									}

									Text
									{
										id					: menuItemShortcut
										text				: menu.props.hasOwnProperty("shortcut") ? menu.props["shortcut"][index] : ""
										font				: jaspTheme.font
										color				: menuItem.itemEnabled ? jaspTheme.black : jaspTheme.gray
										anchors
										{
											right			: parent.right
											rightMargin		: jaspTheme.itemPadding
											verticalCenter	: parent.verticalCenter
										}
									}

									MouseArea
									{
										id				: mouseArea
										hoverEnabled	: true
										anchors.fill	: parent
										onClicked		: callMenuAction(index)
										enabled			: menuItem.itemEnabled
									}
								}
							}

							Component
							{
								id: menuGroupTitle

								Rectangle
								{
									id		: menuItem
									width	: column.columnWidth
									height	: (isSmall ? 0.666 : 1) * jaspTheme.menuGroupTitleHeight
									color	: (model.modelData === undefined) && !menuItem.itemEnabled
													? "transparent"
													: groupMouseArea.pressed || index == currentIndex
														? jaspTheme.buttonColorPressed
														: groupMouseArea.containsMouse
															? jaspTheme.buttonColorHovered
															: "transparent"


									// Same logic as in menuDelegate
									implicitWidth			: menuItemText.x + menuItemText.implicitWidth + jaspTheme.itemPadding + (subMenuItemArrow.visible ? subMenuItemArrow.width + jaspTheme.menuSpacing : 0)
									onImplicitWidthChanged	: column.computeColumnWidth()

									property bool itemEnabled :	menu.props.hasOwnProperty("enabled") ? menu.props["enabled"][index] : (model.modelData !== undefined || model.isEnabled)

									Accessible.role		: Accessible.MenuItem
									Accessible.name		: (model.modelData !== undefined ? model.modelData.substring(3) : displayText)
									Accessible.onPressAction: { if (menuItem.itemEnabled) callMenuAction(index) }

									Image
									{
										id					: menuItemImage
										height				: visible ? (parent.height - (jaspTheme.contentMargin * 2)) : 0
										width				: height

										source				: model.modelData !== undefined ? "" : menuImageSource
										smooth				: true
										mipmap				: true
										fillMode			: Image.PreserveAspectFit
										visible				: source != ""

										anchors
										{
											left			: parent.left
											leftMargin		: jaspTheme.contentMargin
											verticalCenter	: parent.verticalCenter
										}
									}

									Text
									{
										id					: menuItemText
										text				: model.modelData !== undefined ? model.modelData.substring(3) : displayText
										font				: isSmall ? jaspTheme.fontGroupTitleSmall : jaspTheme.fontGroupTitle
										color				: jaspTheme.textEnabled
										verticalAlignment	: Text.AlignVCenter
										anchors
										{
											left			: menuItemImage.visible ? menuItemImage.right : parent.left
											leftMargin		: jaspTheme.itemPadding
											verticalCenter	: parent.verticalCenter
										}
									}

									Image
									{
										id					: subMenuItemArrow
										height				: visible ? (15 * preferencesModel.uiScale) : 0
										width				: height
										visible				: menu.hasSubMenus

										source				: jaspTheme.iconPath + "arrow-right.png"
										smooth				: true
										mipmap				: true
										fillMode			: Image.PreserveAspectFit

										verticalAlignment	: Text.AlignVCenter
										anchors
										{
											right			: parent.right
											rightMargin		: jaspTheme.menuSpacing
											verticalCenter	: parent.verticalCenter
										}
									}

									MouseArea
									{
										id				: groupMouseArea
										hoverEnabled	: true
										anchors.fill	: parent
										onClicked		: callMenuAction(index)
										enabled			: subMenuItemArrow.visible
									}


								}
							}

							Component
							{
								id	: menuSeparator
								ToolSeparator { 
									orientation	: Qt.Horizontal; 
									width: column.columnWidth
									Accessible.role		: Accessible.Separator
									Accessible.name		: "Separator"
								}
							}
						}
					}
				}
			}
		}
	}

	RectangularShadow
	{
		id				: menuShadow
		anchors.fill	: menuRectangle
		color			: jaspTheme.shadow
		blur			: 5
		spread			: 2
		radius			: menuRectangle.radius
		offset.x		: 2
		offset.y		: 2
	}
}
