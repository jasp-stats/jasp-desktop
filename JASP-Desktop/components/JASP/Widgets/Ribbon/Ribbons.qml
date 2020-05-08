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
import QtQuick.Controls 2.4


Item
{
	id:				jaspRibbons
	objectName:		"jaspRibbon"
	width:			500
	height:			jaspTheme.ribbonButtonHeight

	MouseArea
	{
		id:					convertVerticalIntoHorizontalScrolling
		z:					10
		anchors.fill:		parent
		acceptedButtons:	Qt.NoButton
		onWheel:
		{
			var bigWheel = Math.abs(wheel.angleDelta.x) > Math.abs(wheel.angleDelta.y) ? wheel.angleDelta.x : wheel.angleDelta.y;
			buttonList.flick(1000 * bigWheel / 120, 0)
		}
	}

	ListView
	{
		id:								buttonList
		z:								0
		model:							ribbonModelFiltered
		orientation:					ListView.Horizontal
		currentIndex:					ribbonModelFiltered.highlightedModuleIndex
		height:							parent.height
		boundsBehavior:					Flickable.StopAtBounds

		highlightFollowsCurrentItem:	true
		highlightMoveDuration:			20

		onDragStarted:					customMenu.hide()
		onMovementStarted:				customMenu.hide()

		anchors
		{
			left:			parent.left
			right:			parent.right
			verticalCenter:	parent.verticalCenter
		}


		delegate: RibbonButton
		{
			text:			model.moduleTitle
			moduleName:		model.moduleName
			source:			model.ribbonButton ? ((model.isDynamic ? "file:" : "qrc:/icons/") + model.ribbonButton.iconSource) : ""
			menu:			model.ribbonButton ? model.ribbonButton.analysisMenu : undefined
			toolTip:		model.ribbonButton ? model.ribbonButton.toolTip : undefined
			enabled:		model.ribbonButton ? model.active : false
			visible:		model.ribbonButton ? true : false
		}
	}

	property real fadeOutMultiplier: 1.5

	Item
	{
		id:			fadeOutLeft
		width:		height * Math.min(fadeOutMultiplier, ((buttonList.contentX - buttonList.originX) / height))
		visible:	width > 0
		z:			1
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			left:		parent.left
			leftMargin:	-2
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: jaspTheme.uiBackground	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			-90
		}
	}

	Item
	{
		id:			fadeOutRight
		width:		height * Math.min(fadeOutMultiplier, (((buttonList.originX + buttonList.contentWidth) - (buttonList.contentX + buttonList.width)) / height))
		visible:	width > 0
		z:			1
		anchors
		{
			top:			parent.top
			bottom:			parent.bottom
			right:			parent.right
			rightMargin:	-2
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: jaspTheme.uiBackground	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			90
		}
	}
}
