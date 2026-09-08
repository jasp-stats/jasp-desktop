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
import JASP
import JASP.Widgets

Item
{
	id				: ribbonButton
	width			: implicitWidth * preferencesModel.ribbonBarHeightScale
	implicitWidth	: separator ? 2 * jaspTheme.ribbonButtonPadding : (innerText.width > _imgIndWidth ? innerText.width : _imgIndWidth) + (2 * jaspTheme.ribbonButtonPadding) // + 2*tbutton.width
	height			: jaspTheme.ribbonButtonHeight * preferencesModel.ribbonBarHeightScale
	
	z				: 1
	objectName		: "ribbonButton"

	property alias	text		: innerText.text
	property int	listIndex   : -1
	property string	source		: ""
	property bool	enabled		: true
	property bool	separator	: false
	property bool	isModule	: false //Only actual modules can be dragged to another spot in the ribbon
	property bool	ready		: false
	property string moduleName	: "???"
	property string toolTip		: ""
	property var	menu		: []
	property bool	myMenuOpen	: false
	property bool	showPressed	: ribbonButton.activeFocus || myMenuOpen

	onMyMenuOpenChanged: if(!myMenuOpen) myMenuOpen = false; //Break the binding

	property real _imgIndWidth: backgroundImage.width + (menuIndicator.visible ? (menuIndicator.width + menuIndicator.anchors.leftMargin) * 2 : 0)

	signal clicked

	ToolTip.text:				ribbonButton.toolTip
	ToolTip.visible:			ribbonButton.toolTip !== "" && mice.containsMouse && !ribbonButton.showPressed && !buttonContent.Drag.active
	ToolTip.toolTip.background:		Rectangle { color: jaspTheme.tooltipBackgroundColor; radius: jaspTheme.borderRadius }
	
	Keys.onPressed: (event) =>
	{
		if (event.key === Qt.Key_Escape)
		{
			customMenu.hideMenus();
		}
		else if (event.key === Qt.Key_Return || event.key === Qt.Key_Space)
		{
			if (ribbonButton.enabled && ribbonButton.ready)
			{
				ribbonButton.startOrShowMenu();
				event.accepted = true;
			}
		}
		else if (event.key === Qt.Key_Down)
		{
			if (ribbonButton.ready && ribbonButton.enabled)
				ribbonButton.showMyMenu();
			event.accepted = true;
		}
	}

	onFocusChanged:
	{
		if (!ribbonButton.focus)
		{
			myMenuOpen = false;
			customMenu.hideMenus();
		}
	}

	function startOrShowMenu()
	{
		if (ribbonButton.menu.rowCount() === 0) //Probably special?
		{
			customMenu.hideMenus()
            messages.log("startOrShowMenu() for " + ribbonButton.moduleName)
			ribbonModel.analysisClicked("", "", "", ribbonButton.moduleName)

		}
		else if (ribbonButton.menu.rowCount() === 1)
		{
			customMenu.hideMenus()
			ribbonModel.analysisClicked(ribbonButton.menu.getFirstAnalysisFunction(), ribbonButton.menu.getFirstAnalysisQML(), ribbonButton.menu.getFirstAnalysisTitle(), ribbonButton.moduleName)
		}
		else
			showMyMenu();
	}

	function showMyMenu()
	{
		if (ribbonButton.menu.rowCount() <= 1)
			return

		var functionCall = function (index)
		{
			let menuModel	= customMenu.props['model']

			if (index < 0 || index >= menuModel.rowCount())
				return;

			let subMenuModel = menuModel.getSubMenu(index)
			if (subMenuModel)
				showMySubMenu(subMenuModel, index)
			else
			{
				ribbonModel.analysisClicked(menuModel.getAnalysisFunction(index), menuModel.getAnalysisQML(index), menuModel.getAnalysisTitle(index), ribbonButton.moduleName)
				customMenu.hideMenus();
				customMenu.focus = false;
			}
		}

		// Key Navigation with Up or Down. Only navigate valid analysis items
		//	@index
		//	@direction: +1 or -1
		var navigateFunc = function (index, direction)
		{
			let menuModel	= customMenu.props['model']
			let nextIndex = mod(index + direction, menuModel.rowCount());
			let hasSubMenus = customMenu.hasSubMenus

			while(true)
			{
				let name	  = menuModel.getAnalysisFunction(nextIndex);
				let isEnabled = menuModel.isAnalysisEnabled(nextIndex);

				// If it is disabled, and not an anlysis entry, nor a group with a submenu: skip it
				if ((hasSubMenus || (name !== "" && name !== '???')) && isEnabled)
					break;

				nextIndex = mod(nextIndex + direction, menuModel.rowCount());
			}
			return nextIndex;
		}

		// Forward navigation call to parent list
		//	@index
		//	@direction: +1 or -1
		var parentNavigateFunc = function (direction)
		{
			let menuModel		= customMenu.props['model']
			let subMenuModel	= subMenuModel = menuModel.getSubMenu(customMenu.currentIndex)

			if (direction === 1 && subMenuModel)
				showMySubMenu(subMenuModel, customMenu.currentIndex)
			else
			{
				customMenu.hideMenus()
				jaspRibbons.forceActiveFocus();
				jaspRibbons.navigateFunction(direction);
				if (buttonList.currentItem)
					buttonList.currentItem.showMyMenu();
			}
		}

		var props =
		{
			"model"					: ribbonButton.menu,
			"functionCall"			: functionCall,
			"hasIcons"				: ribbonButton.menu.hasIcons(),
			"hasSubMenus"			: ribbonButton.menu.hasSubMenus(),
			"navigateFunc"			: navigateFunc,
			"parentNavigateFunc"	: parentNavigateFunc
		};

		customMenu.toggle(ribbonButton, props);

		myMenuOpen = Qt.binding(function() { return customMenu.visible && customMenu.sourceItem == ribbonButton; });

	}

	function showMySubMenu(subMenu, menuIndex)
	{
		if (subMenu.rowCount() === 0)
			return

		var subMenuFunctionCall = function (index)
		{
			let subMenuModel	= customSubMenu.props['model']

			if (index < 0 || index >= subMenuModel.rowCount())
				return;

			ribbonModel.analysisClicked(subMenuModel.getAnalysisFunction(index), subMenuModel.getAnalysisQML(index), subMenuModel.getAnalysisTitle(index), ribbonButton.moduleName)
			customSubMenu.hide();
			customSubMenu.focus = false;
		}

		// Key Navigation with Up or Down. Only navigate valid analysis items
		//	@index
		//	@direction: +1 or -1
		var subMenuNavigateFunc = function (index, direction)
		{
			let subMenuModel	= customSubMenu.props['model']
			let nextIndex		= mod(index + direction, subMenuModel.rowCount());
			let startIndex		= nextIndex
			while(true)
			{
				let name	  = subMenuModel.getAnalysisFunction(nextIndex);
				let isEnabled = subMenuModel.isAnalysisEnabled(nextIndex);

				if (name !== "" && name !== '???' && isEnabled)
					break;

				nextIndex = mod(nextIndex + direction,subMenuModel .rowCount());
				if (nextIndex === startIndex)
					break;
			}
			return nextIndex;
		}

		// Forward navigation call to parent list
		//	@index
		//	@direction: +1 or -1
		var subMenuParentNavigateFunc = function (direction)
		{
			if (direction === 1)
			{
				customMenu.hideMenus()
				jaspRibbons.forceActiveFocus();
				jaspRibbons.navigateFunction(direction);
				if (buttonList.currentItem)
					buttonList.currentItem.showMyMenu();
			}
			else
			{
				customSubMenu.hide()
				customMenu.forceActiveFocus();
			}

		}

		var props =
		{
			"model"					: subMenu,
			"functionCall"			: subMenuFunctionCall,
			"hasIcons"				: subMenu.hasIcons(),
			"navigateFunc"			: subMenuNavigateFunc,
			"parentNavigateFunc"	: subMenuParentNavigateFunc
		};

		let subItem = customMenu.currentMenuItem(menuIndex)

		customSubMenu.toggle(subItem, props);
	}

	//Marks this button as the spot the dragged module will land on, and the buttons in between slide aside
	//to show the gap. Nothing is reordered until the mouse is released. Data, the R-console and the
	//separators are no target: moveModule would refuse them anyway and they must keep their place. The
	//target is not cleared on exit, so passing over the seams between buttons does not make them slide back
	//and forth.
	DropArea
	{
		anchors.fill:	parent
		keys:			["ribbonModule"]

		onEntered:		if(ribbonButton.isModule) jaspRibbons.dropTargetIndex = ribbonButton.listIndex
	}

	Rectangle
	{
		id:			separatorLine
		visible:	separator
		color:		jaspTheme.gray
		width:		2 * jaspTheme.uiScale
		radius:		width
		height:		parent.height * 0.6
		anchors
		{
			horizontalCenter:	parent.horizontalCenter
			top:				parent.top
			topMargin:			parent.height * 0.2
		}
	}

	Rectangle
	{
		id		: borderLeft
		color   : myMenuOpen  ? jaspTheme.grayDarker  : jaspTheme.gray
		width   : showPressed ? 1 : 0
		anchors
		{
			left:				parent.left
			top:				parent.top
			bottom:				parent.bottom
		}
	}

	Rectangle
	{
		id		: borderRight
		width   : showPressed ? 1 : 0
		color   : myMenuOpen  ? jaspTheme.grayDarker  : jaspTheme.gray
		anchors
		{
			right	: parent.right
			top		: parent.top
			bottom	: parent.bottom
		}
	}

	Rectangle
	{
		id					: buttonContent
		width				: ribbonButton.implicitWidth //Not parent.implicitWidth: while being dragged the parent is the ribbon
		height				: jaspTheme.ribbonButtonHeight
		scale				: preferencesModel.ribbonBarHeightScale * (mice.containsMouse && !ribbonButton.showPressed ? jaspTheme.ribbonScaleHovered : 1)
		visible				: !separator
		color			: separator || !(showPressed || buttonContent.Drag.active) ? "transparent" : jaspTheme.grayLighter

		//Spelled out rather than centerIn, because AnchorChanges cannot undo that one
		anchors
		{
			horizontalCenter	: ribbonButton.horizontalCenter
			verticalCenter		: ribbonButton.verticalCenter
		}

		//Only the button slides aside, not the drop area around it, so the module being dragged keeps hitting
		//the same buttons however far they have moved out of its way
		transform: Translate
		{
			x:	jaspRibbons.buttonShift(ribbonButton.listIndex)

			Behavior on x
			{
				enabled: jaspRibbons.slideButtons && preferencesModel.animationsOn
				NumberAnimation { duration: 150; easing.type: Easing.OutQuad }
			}
		}

		Drag.keys:			["ribbonModule"]
		Drag.active:		mice.drag.active
		Drag.hotSpot.x:		width  / 2
		Drag.hotSpot.y:		height / 2

		states:
		[
			State
			{
				name:	"dragging"
				when:	buttonContent.Drag.active

				//Out of the list while being dragged, otherwise it is positioned by it, cannot follow the cursor
				//and would be clipped at the edges of the ribbon
				ParentChange	{ target: buttonContent; parent: jaspRibbons																		}
				AnchorChanges	{ target: buttonContent; anchors.horizontalCenter: undefined; anchors.verticalCenter: undefined							}
				PropertyChanges	{ restoreEntryValues: false; buttonContent { z: 20 }																}
			},

			State
			{
				name:	"chilling"
				when:	!buttonContent.Drag.active

				ParentChange	{ target: buttonContent; parent: ribbonButton																		}
				AnchorChanges	{ target: buttonContent; anchors.horizontalCenter: ribbonButton.horizontalCenter; anchors.verticalCenter: ribbonButton.verticalCenter	}
			}
		]

		RectangularShadow
		{
			z:					-1 //Behind the button's own background, so only the blur around it shows
			anchors.centerIn:	parent
			width:				parent.width
			height:				parent.height
			visible:			buttonContent.Drag.active
			color:				jaspTheme.grayDarker
			blur:				10
			spread:				3
			radius:				jaspTheme.borderRadius
			offset.x:			0
			offset.y:			0
		}

		Image
		{
			id:			backgroundImage
			z:			1
			width:		(37 / 28) * height
			height:		jaspTheme.ribbonButtonHeight - ( (2 * jaspTheme.ribbonButtonPadding) + innerText.anchors.topMargin + innerText.height ) //28
			opacity:	ribbonButton.enabled ? 1 : 0.5
			smooth:		true
			mipmap:		true
			fillMode:	Image.PreserveAspectFit
			visible:	ribbonButton.ready
			source:		ribbonButton.source === "" ? jaspTheme.iconPath + "error.png" : ribbonButton.source

			anchors
			{
				top				: parent.top
				topMargin		: jaspTheme.ribbonButtonPadding
				horizontalCenter: parent.horizontalCenter
			}

		}

		LoadingIndicator
		{
			anchors.top:		 backgroundImage.top
			anchors.left:		 backgroundImage.left
			width:				 backgroundImage.width
			height:				 backgroundImage.height
			visible:			!ribbonButton.ready
			z:					 backgroundImage.z + 1
		}

		Rectangle
		{
			id:					updateBadge
			z:					backgroundImage.z + 2
			width:				16 * preferencesModel.uiScale
			height:				width
			radius:				width / 2
			color:				"transparent"
			visible:			moduleLibrary.updatableModuleNames.indexOf(ribbonButton.moduleName) >= 0
			anchors
			{
				top:			backgroundImage.top
				left:			backgroundImage.left
				topMargin:		-4
				leftMargin:		-4
			}

			Image
			{
				anchors.fill:		parent
				source:				jaspTheme.iconPath + "updateIcon.svg"
				smooth:				true
			}

			ToolTip.text:		qsTr("Update available")
			ToolTip.visible:	mouseBadge.containsMouse

			MouseArea
			{
				id:				mouseBadge
				anchors.fill:	parent
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
				onClicked:		modulesMenu.opened = true
			}
		}

		Image
		{
			id:					menuIndicator

			anchors.left:		backgroundImage.right
			anchors.leftMargin: 5   * preferencesModel.uiScale
			height:				0.3 * backgroundImage.height
			width:				visible ? height : 0
			anchors.top:		backgroundImage.top
			source:				jaspTheme.iconPath + "/toolbutton-menu-indicator.svg"
			opacity:			ribbonButton.enabled ? 1 : 0.5
			visible:			ribbonButton.menu && ribbonButton.menu.rowCount() > 1
		}

		Text
		{
			id							: innerText

			anchors.horizontalCenter	: backgroundImage.horizontalCenter
			anchors.top					: backgroundImage.bottom
			anchors.topMargin			: 5 * preferencesModel.uiScale
			color						: ribbonButton.enabled ? jaspTheme.black : jaspTheme.gray
			font						: jaspTheme.fontRibbon
			renderType					: Text.QtRendering //Because this might be transformed and ugly if done natively
		}

		MouseArea
		{
			id				: mice
			anchors.fill	: parent
			hoverEnabled	: true
			acceptedButtons	: Qt.LeftButton
			cursorShape		: buttonContent.Drag.active ? Qt.ClosedHandCursor : Qt.PointingHandCursor
			enabled			: ribbonButton.enabled

			//A click is only delivered when the press did not turn into a drag, so the same mouse area can open
			//the menu and pick the button up. Deliberately unbounded: minimumX and maximumX are measured in the
			//ribbon's own coordinates, so any bound there pins the buttons nearest that edge - the very ones that
			//need room to be dragged past their neighbour, and to reach the strip that starts the auto scroll.
			drag.target		: ribbonButton.isModule ? buttonContent : null
			drag.axis		: Drag.XAxis

			drag.onActiveChanged:
			{
				if(drag.active)	jaspRibbons.startModuleDrag(ribbonButton.listIndex, ribbonButton.width, buttonContent);
				else			jaspRibbons.finishModuleDrag(ribbonButton.listIndex);
			}

			onClicked: (mouse)=>
			{
				if(!ribbonButton.ready) return; //Be patient!

				if (myMenuOpen)
				{
					ribbonButton.focus = false;
					customMenu.hideMenus();
				}
				else
				{
					fileMenuModel.visible	= false;
					modulesMenu.opened		= false;
					ribbon.focusOutFileMenu();
					ribbon.focusOutModules();
					ribbon.focusOutPreviousRibbonButton();
					ribbon.goToRibbonIndex(ribbonButton.listIndex);
					ribbonButton.startOrShowMenu();
				}
				mouse.accepted = false;
			}
		}
	}
}
