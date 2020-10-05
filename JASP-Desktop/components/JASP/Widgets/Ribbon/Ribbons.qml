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
	
	property double leftSideSpace:  0
	property double rightSideSpace: 0

	MouseArea
	{
		id:					convertVerticalIntoHorizontalScrolling
		z:					10
		anchors.fill:		parent
		acceptedButtons:	Qt.NoButton
		cursorShape:		customMenu.visible ? Qt.PointingHandCursor : Qt.OpenHandCursor
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
		//cacheBuffer:					leftSideSpace + rightSideSpace
		displayMarginBeginning:			leftSideSpace  * 2
		displayMarginEnd:				rightSideSpace * 2

		highlightFollowsCurrentItem:	true
		highlightMoveDuration:			20

		onDragStarted:					customMenu.hide()
		onMovementStarted:				customMenu.hide()

		anchors
		{
			left:			parent.left
			right:			parent.right
			leftMargin:		leftSideSpace
			rightMargin:	rightSideSpace
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
	
	Rectangle
	{
		id:				darkeningLeft
		width:			leftSideSpace * 0.333333
		color:			jaspTheme.uiBackground
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			left:		parent.left
		}
	}

	Item
	{
		id:			fadeOutLeft
		width:		leftSideSpace + Math.min(leftSideSpace, ((buttonList.contentX - buttonList.originX)))
		visible:	width > 0
		z:			1
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			left:		darkeningLeft.right
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
	
	
	Rectangle
	{
		id:				darkeningRight
		width:			rightSideSpace * 0.333333
		color:			jaspTheme.uiBackground
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
		width:		rightSideSpace + Math.min(rightSideSpace, (((buttonList.originX + buttonList.contentWidth) - (buttonList.contentX + buttonList.width))))
		visible:	width > 0
		z:			1
		anchors
		{
			top:			parent.top
			bottom:			parent.bottom
			right:			darkeningRight.left
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
