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

import QtQuick 2.11
import JASP.Widgets 1.0


FocusScope
{
	id		: ribbonBar
	height	: ribbonMenu.height

	// This property is required to show filemenu button press in KeyNavigation
	property bool isFileMenuPressed: false

	function focusOnRibbonMenu()
	{
		ribbonMenu.focus = true;
		ribbonMenu.setCurrentIndex('first');
	}

	function focusOutRibbonBar()
	{
		modulesPlusButton.showPressed = false;
		isFileMenuPressed             = false;
		ribbonMenu.focusOut();
	}

	function focusOutFileMenu()
	{
		isFileMenuPressed        = false;
		fileMenuOpenButton.focus = false;
	}

	function focusOutModules()
	{
		modulesPlusButton.showPressed = false;
		modulesPlusButton.focus       = false;
	}

	function focusOutPreviousRibbonButton()
	{
		ribbonMenu.focusOut();
	}

	function goToRibbonIndex(_index)
	{
		ribbonMenu.setCurrentIndex('other', _index);
	}

	Keys.onPressed: (event) =>
	{
		if      (event.key === Qt.Key_Left)
		{
			if (modulesPlusButton.focus)
			{
				modulesPlusButton.focus        = false;
				modulesPlusButton.showPressed  = false;
				ribbonMenu.focus               = true;
				ribbonMenu.setCurrentIndex('last');
			}
			else if (fileMenuOpenButton.focus)
			{
				fileMenuOpenButton.focus = false;
				isFileMenuPressed        = false;
				showModulesMenuPressed();
			}
		}
		else if (event.key === Qt.Key_Right)
		{
			if (modulesPlusButton.focus)
			{
				modulesPlusButton.focus       = false;
				modulesPlusButton.showPressed = false;
				showFileMenuPressed();
			}
			else if (fileMenuOpenButton.focus)
			{
				fileMenuOpenButton.focus = false;
				isFileMenuPressed        = false;
				ribbonMenu.focus         = true;
				ribbonMenu.setCurrentIndex('first')
			}
		}
		else if (event.key === Qt.Key_Return || event.key === Qt.Key_Space || event.key === Qt.Key_Down)
		{
			if (modulesPlusButton.focus)
				modulesMenu.opened = true;
			else if (fileMenuOpenButton.focus)
			{
				fileMenuModel.visible = true;
				isFileMenuPressed     = false;
			}
		}
		else if (event.key === Qt.Key_Escape)
		{
			if      (modulesPlusButton.focus)
			{
				modulesMenu.opened            = false;
				modulesPlusButton.focus       = false;
				modulesPlusButton.showPressed = false;
			}
			else if (fileMenuOpenButton.focus)
			{
				fileMenuModel.visible    = false;
				isFileMenuPressed        = false;
				fileMenuOpenButton.focus = false;
			}
			else
				ribbonMenu.focusOut();
			ribbonBar.focus = false;
		}
	}

	function showModulesMenuPressed()
	{
		modulesPlusButton.focus       = true;
		modulesPlusButton.showPressed = true;
	}

	function showFileMenuPressed()
	{
		isFileMenuPressed        = true;
		fileMenuOpenButton.focus = true;
	}

	Rectangle
	{
		color			: jaspTheme.uiBackground
		anchors.fill	: parent
		z				: -1
	}

	MenuArrowButton
	{
		id			: fileMenuOpenButton
		toolTip		: qsTr("Show main menu")
		showPressed	: fileMenuModel.visible || isFileMenuPressed
		buttonType	: MenuArrowButton.ButtonType.Hamburger
		z			: 2
		width		: 0.75 * height

		onClicked:
		{
			fileMenuModel.visible = !fileMenuModel.visible;

			if (fileMenuModel.visible)
				showFileMenuPressed();
			else
				focusOutFileMenu();

			modulesMenu.opened			  = false;
			modulesPlusButton.showPressed = false;
			isFileMenuPressed             = false;
			ribbonMenu.focusOut();
			customMenu.hide()
		}

		anchors
		{
			top:			parent.top
			left:			parent.left
			bottom:			parent.bottom
		}
	}

	Ribbons
	{
		id	: ribbonMenu
		z	: 0

		anchors
		{
			top			: parent.top
			left		: fileMenuOpenButton.right
			right		: modulesPlusButton.left
			leftMargin	: -1
			rightMargin	: -1
		}
	}

	MenuArrowButton
	{
		id			: modulesPlusButton
		toolTip		: qsTr("Show modules menu")
		buttonType	: MenuArrowButton.ButtonType.Plus
		width		: 0.75 * height
		showPressed	: modulesMenu.opened
		z			: 2

		onClicked	:
		{
			modulesMenu.opened = !modulesMenu.opened;

			if (modulesMenu.opened)
				showModulesMenuPressed();
			else
				focusOutModules();

			fileMenuModel.visible = false;
			isFileMenuPressed     = false;
			customMenu.hide()
			ribbonMenu.focusOut();
		}

		anchors
		{
			top		: parent.top
			right	: parent.right
			bottom	: parent.bottom
		}
	}
	
	Rectangle  // a line underneath ribbonbar, just like in filemenu
	{
		z		: 3
		height	: 1
		color	: jaspTheme.uiBorder

		anchors
		{
			top		: parent.bottom
			left	: parent.left
			right	: parent.right
		}
	}

	Rectangle
	{
		id		: leftShadow
		y		: ribbonMenu.height
		z		: 1
		height	: jaspTheme.shadowRadius
		width   : customMenu.visible && customMenu.sourceItem !== null && customMenu.sourceItem.objectName === "ribbonButton" ? customMenu.x : ribbonBar.width

		anchors
		{
			left	: parent.left
		}

		gradient	: Gradient {
			GradientStop { position: 0.0; color: jaspTheme.shadow }
			GradientStop { position: 1.0; color: "transparent" } }
	}
	
	Rectangle
	{
		id		: rightShadow
		y		: ribbonMenu.height
		z		: 1
		height	: jaspTheme.shadowRadius
		width	: customMenu.visible  && customMenu.sourceItem !== null && customMenu.sourceItem.objectName === "ribbonButton" ? (ribbonBar.width - (customMenu.x + customMenu.width)) : 0

		anchors
		{
			right	: parent.right
		}

		gradient	: Gradient {
			GradientStop { position: 0.0; color: jaspTheme.shadow }
			GradientStop { position: 1.0; color: "transparent" } }
	}
}
