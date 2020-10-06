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
		anchors.fill:		buttonList
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
			left:			leftArrow.right
			right:			rightArrow.left
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
	
	MenuArrowButton
	{
		id:				leftArrow
		z:				1
		buttonType:		MenuArrowButton.ButtonType.LeftArrow
		visible:		fadeOutLeft.visible
		width:			height * 0.4
		iconScale:		0.4
		onClicked:		buttonList.flick(jaspTheme.maximumFlickVelocity, 0)
		
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			left:		parent.left
		}	
		
	}

	property real fadeOutMultiplier: 1

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
			left:		leftArrow.right
			leftMargin:	-2
		}
		
		Rectangle  // a line on the side "under which" ribbonbuttons can dissappear
		{
			z		: 3
			width	: 1
			color	: jaspTheme.uiBorder
	
			anchors
			{
				top		: parent.top
				left	: parent.left
				bottom	: parent.bottom
			}
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: jaspTheme.shadow	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			-90
		}
	}
	
	MenuArrowButton
	{
		id:				rightArrow
		z:				1
		buttonType:		MenuArrowButton.ButtonType.RightArrow
		visible:		fadeOutRight.visible
		width:			leftArrow.width
		iconScale:		leftArrow.iconScale
		onClicked:		buttonList.flick(-jaspTheme.maximumFlickVelocity, 0)
		
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			right:		parent.right
		}	
		
	}

	Item
	{
		id:			fadeOutRight
		width:		height * Math.min(fadeOutMultiplier, (((buttonList.originX + buttonList.contentWidth) - (buttonList.contentX + buttonList.width + 1)) / height))
		visible:	width > 0
		z:			1
		anchors
		{
			top:			parent.top
			bottom:			parent.bottom
			right:			rightArrow.left
			rightMargin:	-2
		}
		
		Rectangle  // a line on the side "under which" ribbonbuttons can dissappear
		{
			z		: 3
			width	: 1
			color	: jaspTheme.uiBorder
	
			anchors
			{
				top		: parent.top
				right	: parent.right
				bottom	: parent.bottom
			}
		}

		Rectangle
		{
			gradient: Gradient
			{
				GradientStop { position: 0.0; color: jaspTheme.shadow	}
				GradientStop { position: 1.0; color: "transparent"		}
			}
			width:				parent.height
			height:				parent.width
			anchors.centerIn:	parent
			rotation:			90
		}
	}
}
