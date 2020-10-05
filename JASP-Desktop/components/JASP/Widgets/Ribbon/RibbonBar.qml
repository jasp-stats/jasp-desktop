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
		width		: height
		showArrow	: fileMenuModel.visible
		hamburger	: true
		z			: 2

		onClicked:
		{
			fileMenuModel.visible	= !fileMenuModel.visible;
			modulesMenu.opened		= false;
			
			customMenu.hide()
		}

		anchors
		{
			top		: parent.top
			left	: parent.left
			bottom	: parent.bottom
		}
	}

	Ribbons
	{
		id	: ribbonMenu
		z	: 0
		
		leftSideSpace:	fileMenuOpenButton.width
		rightSideSpace: modulesPlusButton.width

		anchors
		{
			top		: parent.top
			right	: modulesPlusButton.right
			left	: fileMenuOpenButton.left
		}
	}

	MenuArrowButton
	{
		id			: modulesPlusButton
		width		: height
		hamburger	: false
		toolTip		: qsTr("Show Modules Menu")

		onClicked	:
		{
			modulesMenu.opened		= !modulesMenu.opened;
			fileMenuModel.visible	= false;
			
			customMenu.hide()
		}

		showArrow	: modulesMenu.opened
		z			: 2

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
