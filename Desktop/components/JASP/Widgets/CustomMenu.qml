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

import QtQuick						2.15
import QtQuick.Controls				2.4
import JASP.Controls				1.0 as JASPControl
import Qt5Compat.GraphicalEffects

FocusScope
{
	id							: menu
	width						: menuRectangle.width
	height						: menuRectangle.height
	visible						: showMe && activeFocus
	x							: Math.min(menuMinPos.x + (menuMinIsMin ? Math.max(0, menuX) : menuX), menuMaxPos.x - (width  + 2) )
	y							: Math.min(menuMinPos.y + (menuMinIsMin ? Math.max(0, menuY) : menuY), menuMaxPos.y - (height + 2) )
	property var	props		: undefined
	property bool	hasIcons	: true
	property real	_iconPad	: 5 * preferencesModel.uiScale
	property int	menuX		: menuOffset.x + menuScroll.x
	property int	menuY		: menuOffset.y + menuScroll.y
	property point	menuOffset	: "0,0"
	property point	menuScroll	: "0,0"
	property point	menuMinPos	: "0,0"
	property point	menuMaxPos	: "0,0"
	property bool	menuMinIsMin: false
	property bool	showMe		: false
	property var    sourceItem  : null
	property point	scrollOri	: "0,0" //Just for other qmls to use as a general storage of the origin of their scrolling

	property int	currentIndex: -1

	Keys.onPressed: (event)=>
	{
		if (event.key === Qt.Key_Up)
		{
			if (menu.props["navigateFunc"] === undefined || typeof(menu.props["navigateFunc"]) === "undefined")
				menu.currentIndex = mod(menu.currentIndex - 1, repeater.count)
			else
				menu.currentIndex = menu.props["navigateFunc"](currentIndex, -1);
		}
		else if (event.key === Qt.Key_Down)
		{
			if (menu.props["navigateFunc"] === undefined || typeof(menu.props["navigateFunc"]) === "undefined")
				menu.currentIndex = mod(menu.currentIndex + 1, repeater.count)
			else
				menu.currentIndex = menu.props["navigateFunc"](currentIndex, 1);
		}
		else if (event.key === Qt.Key_Return || event.key === Qt.Key_Space)
		{
			if (currentIndex > -1)
			{
				menu.props['functionCall'](currentIndex)
				menu.currentIndex = -1;
			}
		}
		else if (event.key === Qt.Key_Escape)
		{
			menu.currentIndex = -1;
			if (menu.sourceItem !== null)
			{
				menu.sourceItem.forceActiveFocus();
				if (menu.sourceItem.myMenuOpen !== undefined && typeof(menu.sourceItem.myMenuOpen) !== 'undefined')
					menu.sourceItem.myMenuOpen = true;
			}
			menu.hide();
		}
	}

	onPropsChanged:
	{
		hasIcons = (menu.props === undefined || "undefined" === typeof(menu.props["hasIcons"])) ? true : menu.props["hasIcons"]

		if (menu.props === undefined || menu.props["model"] !== resultMenuModel)
			resultsJsInterface.runJavaScript("window.setSelection(false);")
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
		menu.menuMaxPos.x	= Qt.binding(function() { return mainWindowRoot.width;  });
		menu.menuMaxPos.y	= Qt.binding(function() { return mainWindowRoot.height; });
		menu.menuMinPos     = item.mapToItem(null, 1, 1);
		menu.props          = props;
		menu.menuOffset.x	= x_offset;
		menu.menuOffset.y	= y_offset;
		menu.menuScroll		= "0,0";
		menu.showMe			= true;

		menu.forceActiveFocus();

	}

	function hide()
	{
		menu.showMe			= false;
		menu.sourceItem     = null;
		menu.props			= undefined;
		menu.menuMinIsMin	= false;
		menu.menuOffset		= "0,0"
		menu.menuScroll		= "0,0"
		menu.menuMinPos		= "0,0"
	}

	function resizeElements(newWidth)
	{
		for (var i = 0; i < repeater.count; ++i)
			repeater.itemAt(i).width = newWidth;
	}

	Rectangle
	{
		id		: menuRectangle
		z		: menuShadow.z + 1
		color	: jaspTheme.fileMenuColorBackground
		focus	: true
		width	: column.maxWidth + (itemScrollbar.visible ? itemScrollbar.width : 0)
		height	: menuOffset.y + column.maxHeight > menuMaxPos.y ? menuMaxPos.y - menuOffset.y : column.maxHeight

		MouseArea
		{
			anchors.fill	: parent
			acceptedButtons	: Qt.NoButton
			onWheel			: wheel.accepted = true
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

		Flickable
		{
			id						: itemFlickable
			anchors.fill			: parent
			anchors.topMargin		: jaspTheme.menuPadding / 2
			anchors.leftMargin		: jaspTheme.menuPadding / 2
			anchors.rightMargin		: itemScrollbar.width + anchors.margins
			clip					: true
			boundsBehavior			: Flickable.StopAtBounds
			contentWidth			: column.width
			contentHeight			: column.height


			Column
			{
				id		: column
				z		: menuRectangle.z + 1
				spacing	: jaspTheme.menuSpacing

				property real maxWidth	: 0
				property real maxHeight	: 0

				Repeater
				{
					id		: repeater
					model	: menu.props === undefined ? undefined : menu.props["model"]

					onItemAdded: (index, item)=>
					{
						if (index === 0)
						{
							column.maxWidth  = 0;
							column.maxHeight = 0;
						}

						column.maxHeight = column.maxHeight + (item.height + jaspTheme.menuSpacing)

						if (index === count - 1)
							column.maxHeight += (jaspTheme.menuPadding - jaspTheme.menuSpacing)

						column.maxWidth = Math.max(item.width + jaspTheme.menuPadding, column.maxWidth);

						if (index === count - 1)
							menu.resizeElements(column.maxWidth);
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
								width:	initWidth
								height: jaspTheme.menuItemHeight
								color:	(model.modelData === undefined) && !model.isEnabled
												? "transparent"
												: mouseArea.pressed || index == currentIndex
													? jaspTheme.buttonColorPressed
													: mouseArea.containsMouse
														? jaspTheme.buttonColorHovered
														: "transparent"

								property double initWidth: (menu.hasIcons ? menuItemImage.width : 0) + menuItemText.implicitWidth + (menu.hasIcons ? menu._iconPad * 5 : menu._iconPad * 4)

								Image
								{
									id						: menuItemImage
									height					: menuItem.height - (2 * menu._iconPad)
									width					: menuItem.height - menu._iconPad

									source					: model.modelData !== undefined ? "" : menuImageSource
									smooth					: true
									mipmap					: true
									fillMode				: Image.PreserveAspectFit

									anchors.left			: parent.left
									anchors.leftMargin		: menu._iconPad * 2
									anchors.verticalCenter	: parent.verticalCenter
								}

								Text
								{
									id					: menuItemText
									text				: model.modelData !== undefined ? model.modelData : displayText
									font				: jaspTheme.font
									color				: model.modelData !== undefined || isEnabled ? jaspTheme.black : jaspTheme.gray
									anchors
									{
										left			: menu.hasIcons ? menuItemImage.right : parent.left
										leftMargin		: menu.hasIcons ? menu._iconPad : menu._iconPad * 2
										rightMargin		: menu._iconPad * 2
										verticalCenter	: parent.verticalCenter
									}
								}

								MouseArea
								{
									id				: mouseArea
									hoverEnabled	: true
									anchors.fill	: parent
									onClicked		: menu.props['functionCall'](index)
									enabled			: model.modelData !== undefined || isEnabled
								}
							}
						}

						Component
						{
							id: menuGroupTitle

							Item
							{
								id		: menuItem
								width	: initWidth
								height	: jaspTheme.menuGroupTitleHeight

								property double initWidth: menuItemImage.width + menuItemText.implicitWidth + 15 * preferencesModel.uiScale

								Image
								{
									id					: menuItemImage
									height				: parent.height - (menu._iconPad * 2)
									width				: height

									source				: model.modelData !== undefined ? "" : menuImageSource
									smooth				: true
									mipmap				: true
									fillMode			: Image.PreserveAspectFit
									visible				: source != ""

									anchors
									{
										top				: parent.top
										left			: parent.left
										bottom			: parent.bottom
										leftMargin		: visible ? menu._iconPad : 0
									}
								}

								Text
								{
									id					: menuItemText
									text				: model.modelData !== undefined ? model.modelData.substring(3) : displayText
									font				: jaspTheme.fontGroupTitle
									color				: jaspTheme.textEnabled
									anchors
									{
										left			: menuItemImage.right
										leftMargin		: menu._iconPad
										verticalCenter	: parent.verticalCenter
									}
								}
							}
						}

						Component
						{
							id	: menuSeparator
							ToolSeparator { orientation	: Qt.Horizontal }
						}
					}
				}
			}
		}
	}

	RectangularGlow
	{
		id				: menuShadow
		anchors.fill	: menuRectangle
		color			: jaspTheme.shadow
		spread			: 0.2
		cornerRadius	: menuRectangle.radius + glowRadius
		glowRadius		: 5
	}
}
