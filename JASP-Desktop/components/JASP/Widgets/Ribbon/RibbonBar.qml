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
import JASP.Theme 1.0

FocusScope
{
	id		: ribbonBar
	height	: ribbonMenu.height

	Rectangle
	{
		color			: Theme.uiBackground
		anchors.fill	: parent
		z				: -1
	}

	MenuArrowButton
	{
		id			: fileMenuOpenButton
		width		: height
		showArrow	: fileMenuModel.visible
		toolTip		: showArrow ? qsTr("Close Menu") : qsTr("Open Menu" )
		toolTipVisible: false
		hamburger	: true
		z			: 2

		onClicked	:
		{
			customMenu.visible		= false;
			fileMenuModel.visible	= !fileMenuModel.visible;
			modulesMenu.opened = false;
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

		anchors
		{
			top		: parent.top
			right	: modulesPlusButton.left
			left	: fileMenuOpenButton.right
		}

		MouseArea
		{
			id: ribbonMenuMouseArea
			anchors.fill: parent
			enabled:  fileMenuModel.visible  || modulesMenu.opened
			cursorShape: !mainWindow.datasetLoaded || enabled ? Qt.ArrowCursor : Qt.PointingHandCursor

			onClicked: {
				fileMenuModel.visible =false;
				modulesMenu.opened = false;
			}
		}
	}

	MenuArrowButton
	{
		id			: modulesPlusButton
		width		: height
		hamburger	: false
		toolTip		: qsTr("Add Module")

		onClicked	:
		{
			customMenu.visible	= false;
			modulesMenu.opened	= !modulesMenu.opened;
			fileMenuModel.visible = false;
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

	Rectangle
	{
		id		: shadow
		y		: ribbonMenu.height
		z		: 1
		height	: Theme.shadowRadius

		anchors
		{
			left	: parent.left
			right	: parent.right
		}

		gradient	: Gradient {
			GradientStop { position: 0.0; color: Theme.shadow }
			GradientStop { position: 1.0; color: "transparent" } }

		Rectangle  // a line underneath ribbonbar, just like in filemenu
		{
			z		: 3
			height	: 1
			color	: Theme.uiBorder

			anchors
			{
				top		: parent.top
				left	: parent.left
				right	: parent.right
			}
		}
	}
}
