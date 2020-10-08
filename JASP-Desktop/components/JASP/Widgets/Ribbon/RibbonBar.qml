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

	Rectangle
	{
		color			: jaspTheme.uiBackground
		anchors.fill	: parent
		z				: -1
	}

	MenuArrowButton
	{
		id			: fileMenuOpenButton
		showPressed	: fileMenuModel.visible
		buttonType	: MenuArrowButton.ButtonType.Hamburger
		z			: 2
		width		: 0.75 * height

		onClicked:
		{
			fileMenuModel.visible	= !fileMenuModel.visible;
			modulesMenu.opened		= false;
			
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
		toolTip		: qsTr("Show Modules Menu")
		buttonType	: MenuArrowButton.ButtonType.Plus
		width		: 0.75 * height
		showPressed	: modulesMenu.opened
		z			: 2

		onClicked	:
		{
			modulesMenu.opened		= !modulesMenu.opened;
			fileMenuModel.visible	= false;
			
			customMenu.hide()
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
