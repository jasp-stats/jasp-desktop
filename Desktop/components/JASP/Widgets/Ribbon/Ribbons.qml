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
import JASP.Controls
import JASP

Item
{
	id:				jaspRibbons
	objectName:		"jaspRibbon"
	width:			500
	height:			jaspTheme.ribbonButtonHeight * preferencesModel.ribbonBarHeightScale

	onActiveFocusChanged: buttonList.focus = true;

	//While a ribbon button is dragged the other buttons slide aside to show the gap it will drop into. The
	//model is only reordered once, on release: moveModule writes the order to the settings and a drag would
	//otherwise rewrite them all the way over.
	property int	draggedIndex:		-1
	property int	dropTargetIndex:	-1
	property real	draggedWidth:		0
	property Item	draggedButton:		null

	//Turned off just before the model is reordered, so the buttons do not slide back out of a gap that is
	//about to be filled by the dragged module anyway
	property bool	slideButtons:		false

	//How far a button must slide to open up that gap. Ribbon buttons are as wide as their title, but every
	//button in between moves over by exactly the width of the dragged one, whatever their own width is.
	function buttonShift(button)
	{
		if(draggedIndex < 0 || dropTargetIndex < 0 || button === draggedIndex)
			return 0;

		if(draggedIndex < dropTargetIndex)	return button >  draggedIndex	&& button <= dropTargetIndex	? -draggedWidth : 0;
		else								return button >= dropTargetIndex	&& button <  draggedIndex		?  draggedWidth : 0;
	}

	function startModuleDrag(index, width, button)
	{
		customMenu.hideMenus();
		buttonList.cancelFlick(); //Otherwise a flick left over from the scroll wheel keeps fighting the auto scroll

		draggedIndex	= index;
		dropTargetIndex	= index;
		draggedWidth	= width;
		draggedButton	= button;
		slideButtons	= true;
	}

	function finishModuleDrag(index)
	{
		//Stop sliding before the buttons are put back where the list wants them: the model move right after
		//this drops the module into the gap they were holding open.
		slideButtons	= false;

		let to			= dropTargetIndex;

		draggedIndex	= -1; //Stops the auto scroller, which runs on this
		dropTargetIndex	= -1;
		draggedButton	= null;

		if(to >= 0 && to !== index)
			ribbonModel.moveModule(ribbonModelFiltered.filteredRowToOriginal(index), ribbonModelFiltered.filteredRowToOriginal(to));
	}

	//The ribbon is usually wider than the window, so it scrolls along when the dragged module is held against
	//either end. Without this a module could only be moved among the buttons that happen to be in view.
	property real	autoScrollMargin:	20 * preferencesModel.uiScale
	property real	autoScrollStep:		8  * preferencesModel.uiScale

	//-1 to scroll left, +1 to scroll right, 0 to sit still
	function autoScrollDirection()
	{
		if(!draggedButton)
			return 0;

		if		(draggedButton.x							< buttonList.x						+ autoScrollMargin)	return -1;
		else if	(draggedButton.x + draggedButton.width		> buttonList.x + buttonList.width	- autoScrollMargin)	return  1;
		else																										return  0;
	}

	//Which button the dragged one is hanging over. The drop areas report that themselves while the mouse moves,
	//but during an auto scroll they slide underneath a module that stays right where it is, so no drop area is
	//entered or left and the target has to be worked out from the geometry instead.
	function updateDropTarget()
	{
		if(!draggedButton)
			return;

		let centerX	= (draggedButton.x + (draggedButton.width / 2)) - buttonList.x + buttonList.contentX,
			index	= buttonList.indexAt(centerX, buttonList.height / 2),
			button	= index >= 0 ? buttonList.itemAtIndex(index) : null;

		if(button && button.isModule)
			dropTargetIndex = index;
	}

	Timer
	{
		id:			autoScroller
		interval:	16
		repeat:		true
		running:	jaspRibbons.draggedIndex >= 0

		onTriggered:
		{
			let direction = jaspRibbons.autoScrollDirection();

			if(direction === 0)
				return;

			let minX	= buttonList.originX,
				maxX	= buttonList.originX + Math.max(0, buttonList.contentWidth - buttonList.width),
				newX	= Math.max(minX, Math.min(maxX, buttonList.contentX + (direction * jaspRibbons.autoScrollStep)));

			if(newX === buttonList.contentX)
				return;

			buttonList.contentX = newX;

			jaspRibbons.updateDropTarget();
		}
	}

	Connections
	{
		target:				ribbonModel
							
		function onDataModeChanged() { focusOut(); }
	}

	function setCurrentIndex(which, _index=null)
	{
		if      (which === 'last')
		{
			buttonList.currentIndex = buttonList.count - 1;
			if (!buttonList.itemAtIndex(buttonList.currentIndex))
				navigateFunction(-1);
		}
		else if (which === 'first')
		{
			buttonList.currentIndex = 0;
			if (!buttonList.itemAtIndex(0).enabled)
				navigateFunction(1);
		}
		else if (which === 'other')
		{
			buttonList.currentIndex = _index;
		}
	}

	function focusOut()
	{
		if (buttonList !== null && buttonList.currentItem !== null)
		{
			buttonList.currentItem.focus = false;
			buttonList.currentIndex      = -1;
		}
	}

	// navigation when left or right arrow keys are pressed
	// direction is an integer, acceptable values: {+1, 0, -1}
	function navigateFunction(direction)
	{
		let nextIndex = buttonList.currentIndex + direction;

		while(true)
		{
			if  (nextIndex === -1) {
				buttonList.currentItem.focus      = false;
				buttonList.currentItem.myMenuOpen = false;
				showFileMenuPressed();
				return;
			}
			else if (nextIndex === buttonList.count)
			{
				buttonList.currentItem.focus      = false;
				buttonList.currentItem.myMenuOpen = false;
				buttonList.currentIndex           = -1;
				showModulesMenuPressed();
				return;
			}

			if (buttonList.itemAtIndex(nextIndex).enabled && !buttonList.itemAtIndex(nextIndex).separator)
				break;

			nextIndex = nextIndex + direction;
		}

		if (nextIndex !== buttonList.currentIndex)
		{
			buttonList.currentItem.focus      = false;
			buttonList.currentItem.myMenuOpen = false;
			buttonList.currentIndex           = nextIndex;
		}
	}

	MouseArea
	{
		id:					convertVerticalIntoHorizontalScrolling
		z:					10
		anchors.fill:		buttonList
		acceptedButtons:	Qt.NoButton
		cursorShape:		Qt.PointingHandCursor
		onWheel:			(wheel)=>
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

		//The button being dragged is a child of its own delegate, so that delegate has to survive the auto
		//scroll: recycled or destroyed it would take the button down with it. Not bound to contentWidth,
		//which the list derives from the delegates it has created, so that would feed back on itself.
		reuseItems:						jaspRibbons.draggedIndex < 0
		cacheBuffer:					jaspRibbons.draggedIndex < 0 ? 320 : 100000 //320 is Qt's own default

		ALTNavigation.enabled:				true
		ALTNavigation.scopeOnly:			true
		ALTNavigation.strategy:				AssignmentStrategy.INDEXED
		ALTNavigation.requestedPostfix:		"M"

		onDragStarted:					customMenu.hideMenus()
		onMovementStarted:				customMenu.hideMenus()
		Keys.onPressed: (event) =>
		{
			if (event.key === Qt.Key_Left || event.key === Qt.Key_Backtab)
			{
				if (currentIndex === 0)
					showFileMenuPressed();
				else
					navigateFunction(-1);
				event.accepted  = true;
			}
			else if (event.key === Qt.Key_Right || event.key === Qt.Key_Tab)
			{
				if (currentIndex === buttonList.count - 1)
				{
					buttonList.currentItem.focus = false;
					buttonList.currentIndex      = -1;
					showModulesMenuPressed();
				}
				else
					navigateFunction(1);
				event.accepted = true;
			}
			else if (event.key   === Qt.Key_Return || event.key === Qt.Key_Space || event.key === Qt.Key_Down)
			{
				buttonList.focus = true;
				buttonList.currentItem.startOrShowMenu();
			}
		}

		anchors
		{
			left:			leftArrow.right
			right:			rightArrow.left
			verticalCenter:	parent.verticalCenter
		}

		delegate: RibbonButton
		{
			text:			 model.moduleTitle
			listIndex:       index
			moduleName:		 model.moduleName
			source:			!model.ribbonButton || model.ribbonButton.iconSource === "" ? ""		: (!model.ribbonButton.special ? "file:" :  jaspTheme.iconPath ) + model.ribbonButton.iconSource
			menu:			!model.ribbonButton ? undefined : model.ribbonButton.menu
			toolTip:		!model.ribbonButton ? undefined : model.ribbonButton.toolTip
			enabled:		 model.ribbonButton && model.active
			visible:		 model.ribbonButton
			ready:			 model.ribbonButton && (model.ribbonButton.ready || model.ribbonButton.special || model.ribbonButton.error)
			separator:		 model.ribbonButton && model.ribbonButton.separator
			isModule:		!!model.dynamicModule //Data, the R-console and the separators carry no module and stay where they are
			

			ALTNavigation.enabled:		!separator
//			ALTNavigation.y: 10
			ALTNavigation.index:		index
			ALTNavigation.onTagMatch:
			{
				ribbon.focusOnRibbonMenu();
				if(enabled && ready)
				{
					ribbon.goToRibbonIndex(listIndex);
					startOrShowMenu();
				}
			}
		}

		onFocusChanged:
		{
			if (!buttonList.focus)
			{
				if (buttonList.currentItem !== null)
				{
					buttonList.currentItem.myMenuOpen = false;
					buttonList.currentItem.focus      = false;
				}
			} 
			else
			{
				if (buttonList.currentIndex === -1)
					buttonList.currentIndex  = 0;

				buttonList.currentItem.focus = true;
			}
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
