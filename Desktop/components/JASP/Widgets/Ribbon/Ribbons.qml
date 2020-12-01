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

import QtQuick			2.15
import QtQuick.Controls 2.15


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
		cursorShape:		Qt.PointingHandCursor
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
		clip:							true
		interactive:					false
		highlightFollowsCurrentItem:	true
		highlightMoveDuration:			20
		reuseItems:						true

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
			text:			 model.moduleTitle
			moduleName:		 model.moduleName
			source:			!model.ribbonButton || model.ribbonButton.iconSource === "" ? ""		: (!model.ribbonButton.special ? "file:" : "qrc:/icons/") + model.ribbonButton.iconSource
			menu:			!model.ribbonButton ? undefined : model.ribbonButton.analysisMenu
			toolTip:		!model.ribbonButton ? undefined : model.ribbonButton.toolTip
			enabled:		 model.ribbonButton && model.active
			visible:		 model.ribbonButton
			ready:			 model.ribbonButton && (model.ribbonButton.ready || model.ribbonButton.special || model.ribbonButton.error)
		}
	}
	
	property real ribbonFlickSpeed: 400
	
	Timer
	{
		id:			scrollLikeAChump
		repeat:		true
		interval:	300
		
		property bool timerWentOffAlready:	false
		property bool goLeft:				false
		
		function anArrowPressed(goLeftPlease)
		{
			interval			= 300;
			goLeft				= goLeftPlease;
			timerWentOffAlready = false;
			
			start();
		}
		
		onTriggered: 
		{
			timerWentOffAlready = true;
			interval			= 10;
			
			if(goLeft)	buttonList.flick( ribbonFlickSpeed, 0);
			else		buttonList.flick(-ribbonFlickSpeed, 0);
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
		onClicked:		if(!scrollLikeAChump.timerWentOffAlready) buttonList.flick(2 * ribbonFlickSpeed, 0)
		
		onPressedChanged: 
			if(pressed)							scrollLikeAChump.anArrowPressed(true);
			else if(scrollLikeAChump.goLeft)	scrollLikeAChump.stop();
		
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			left:		parent.left
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
		onClicked:		if(!scrollLikeAChump.timerWentOffAlready) buttonList.flick(-2 * ribbonFlickSpeed, 0)
		
		onPressedChanged: 
			if(pressed)							scrollLikeAChump.anArrowPressed(false);
			else if(!scrollLikeAChump.goLeft)	scrollLikeAChump.stop();
		
		anchors
		{
			top:		parent.top
			bottom:		parent.bottom
			right:		parent.right
		}	
		
	}

	property real fadeOutMultiplier: 0.15

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
		}
		
		Rectangle  // a line on the side "under which" ribbonbuttons can dissappear
		{
			z		: 3
			width	: 1
			color	: jaspTheme.uiBorder
	
			anchors
			{
				top			: parent.top
				left		: parent.left
				bottom		: parent.bottom
				leftMargin	: -1
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
		}
		
		Rectangle  // a line on the side "under which" ribbonbuttons can dissappear
		{
			z		: 3
			width	: 1
			color	: jaspTheme.uiBorder
	
			anchors
			{
				top			: parent.top
				right		: parent.right
				bottom		: parent.bottom
				rightMargin	: -1
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
