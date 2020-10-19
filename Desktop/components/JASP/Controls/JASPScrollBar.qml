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
// Code based on http://stackoverflow.com/questions/17833103/how-to-create-scrollbar-in-qtquick-2-0

import QtQuick	2.0;
import QtQml	2.15


Item
{
									id							: scrollbar
									width						: vertical ? breadth   : undefined
									height						: vertical ? undefined : breadth
									visible						: flickable.visible && ((vertical ? flickable.visibleArea.heightRatio : flickable.visibleArea.widthRatio ) < 1.0)

	readonly	property int		visibleBreadth				: bigBar ? jaspTheme.scrollbarBoxWidthBig : jaspTheme.scrollbarBoxWidth
				property int		breadth						: visible ? visibleBreadth : 0
				property int		extraMarginRightOrBottom	: 0
				property int		extraMarginLeftOrTop		: 0
				property Flickable	flickable					: null
				property int		minimumLength				: 16 * preferencesModel.uiScale
				property string		bkColor						: jaspTheme.white
				property string		fgColor						: jaspTheme.gray
				property string		pressedColor				: jaspTheme.blueLighter
				property bool		showarrows					: false
				property bool		vertical					: true
				property bool		manualAnchor				: false
				property bool		bigBar						: false

	anchors
	{
		right:			manualAnchor ? undefined : flickable.right
		bottom:			manualAnchor ? undefined : flickable.bottom
		top:			manualAnchor ? undefined : vertical	? flickable.top				: undefined
		left:			manualAnchor ? undefined : vertical	? undefined					: flickable.left
		topMargin:		manualAnchor ? undefined : vertical ? extraMarginLeftOrTop		: undefined
		leftMargin:		manualAnchor ? undefined : vertical ? undefined					: extraMarginLeftOrTop
		rightMargin:	manualAnchor ? undefined : vertical ? undefined					: extraMarginRightOrBottom
		bottomMargin:	manualAnchor ? undefined : vertical ? extraMarginRightOrBottom	: undefined
	}




	function scroll(movement)
	{
		if(vertical)	flickable.contentY = Math.max (0, Math.min (flickable.contentY + (flickable.height * movement), flickable.contentHeight - flickable.height));
		else			flickable.contentX = Math.max (0, Math.min (flickable.contentX + (flickable.width  * movement), flickable.contentWidth  - flickable.width));
	}

	function scrollDown() { scroll( 0.125); }
	function scrollUp ()  { scroll(-0.125); }

	function scrollWheel(wheel)
	{
		if(scrollbar.vertical)
		{
					if(wheel.pixelDelta.y !== 0)	scrollbar.scroll(-wheel.pixelDelta.y / scrollbar.height)
			else	if(wheel.angleDelta.y < 0)		scrollbar.scrollDown();
			else	if(wheel.angleDelta.y > 0)		scrollbar.scrollUp();
		} else {
					if(wheel.pixelDelta.x !== 0)	scrollbar.scroll(-wheel.pixelDelta.x / scrollbar.width)
			else	if(wheel.angleDelta.x < 0)		scrollbar.scrollDown();
			else	if(wheel.angleDelta.x > 0)		scrollbar.scrollUp();
		}
	}
	
	Binding
	{
		restoreMode: Binding.RestoreBinding
		target:		handle;
		property:	scrollbar.vertical ? "y" : "x"
		when:		!clicker.drag.active
		value:		scrollbar.vertical ?
						(flickable.contentY * clicker.drag.maximumY / (flickable.contentHeight - flickable.height)) :
						(flickable.contentX * clicker.drag.maximumX / (flickable.contentWidth  - flickable.width))  ;
	}

	Binding
	{
		restoreMode: Binding.RestoreBinding
		target:		flickable
		property:	scrollbar.vertical ? "contentY" : "contentX"
		when:		(clicker.drag.active || clicker.pressed)
		value:		scrollbar.vertical ?
						(handle.y * (flickable.contentHeight - flickable.height) / clicker.drag.maximumY) :
						(handle.x * (flickable.contentWidth  - flickable.width)  / clicker.drag.maximumX) ;
	}

	Rectangle
	{
		id:				backScrollbar
		antialiasing:	true
		color:			bkColor
		anchors.fill:	parent
		border
		{
			width:		1
			color:		jaspTheme.grayDarker
		}
		
		MouseArea
		{
			id:				clicker
			hoverEnabled:	true
			anchors.fill:	parent;
			cursorShape:	Qt.PointingHandCursor

			onWheel:		scrollbar.scrollWheel(wheel)

			drag
			{
				target:		handle;
				minimumY:	!scrollbar.vertical ? 0 : 0
				maximumY:	!scrollbar.vertical ? 0 : (groove.height - handle.height)
				axis:		Drag.XAndYAxis
				minimumX:	scrollbar.vertical  ? 0 : 0
				maximumX:	scrollbar.vertical  ? 0 : (groove.width - handle.width)

			}

			onClicked:	if(scrollbar.vertical)	flickable.contentY = (mouse.y / groove.height * (flickable.contentHeight - flickable.height));
						else					flickable.contentX = (mouse.x / groove.width  * (flickable.contentWidth  - flickable.width));

		}
	}

	MouseArea
	{
		id:			btnUp
		height:		size
		width:		size

		property real size: !showarrows ? 0 : scrollbar.vertical ? scrollbar.width : scrollbar.height

		anchors
		{
			top:		parent.top;
			left:		parent.left;
			right:		scrollbar.vertical ? parent.right : undefined;
			bottom:		scrollbar.vertical ? undefined    : parent.bottom;

			margins:	backScrollbar.border.width + 1
		}

		onClicked:		scrollUp ();
		
		Image
		{
			source:					scrollbar.vertical ? jaspTheme.iconPath + "arrow-up.png" : jaspTheme.iconPath + "arrow-left.png"
			visible:				showarrows
			anchors.fill:			parent
			sourceSize.width:		width * 2
			sourceSize.height:		height * 2
		}
	}

	MouseArea
	{
		id:		btnDown
		height: btnUp.size
		width:	btnUp.size

		anchors
		{
			top:		scrollbar.vertical ? undefined   : parent.top
			left:		scrollbar.vertical ? parent.left : undefined
			right:		parent.right
			bottom:		parent.bottom
			margins:	backScrollbar.border.width + 1
		}

		onClicked:  scrollDown ();
		
		Image
		{
			source:					scrollbar.vertical ? jaspTheme.iconPath + "arrow-down.png" : jaspTheme.iconPath + "arrow-right.png"
			visible:				showarrows
			anchors.fill:			parent
			sourceSize.width:		width * 2
			sourceSize.height:		height * 2
		}
	}

	Item
	{
		id:		groove
		clip:	true
		property real basicMargin:			  backScrollbar.border.width + 1
		property real extraVerticalMargin:	  scrollbar.vertical ? btnUp.size + 1 : 0
		property real extraHorizontalMargin: !scrollbar.vertical ? btnUp.size + 1 : 0

		anchors
		{
			fill:			parent
			topMargin:		basicMargin + extraVerticalMargin
			leftMargin:		basicMargin + extraHorizontalMargin
			rightMargin:	basicMargin + extraHorizontalMargin
			bottomMargin:	basicMargin + extraVerticalMargin
		}

		Item
		{
			id:			handle;
			height:		!scrollbar.vertical ? parent.height : Math.max (scrollbar.minimumLength, (flickable.visibleArea.heightRatio * groove.height))
			width:		 scrollbar.vertical ? parent.width	: Math.max (scrollbar.minimumLength, (flickable.visibleArea.widthRatio  * groove.width))
			anchors
			{
				top:	scrollbar.vertical ? undefined		: parent.top
				left:	scrollbar.vertical ? parent.left	: undefined
				right:	scrollbar.vertical ? parent.right	: undefined
				bottom:	scrollbar.vertical ? undefined		: parent.bottom
			}
			
			Rectangle
			{
				id:				backHandle
				color:			clicker.pressed || clicker.containsMouse	? pressedColor	: fgColor
				anchors.fill:	parent
				
				Behavior on opacity { enabled: preferencesModel.animationsOn; NumberAnimation { duration: 150; } }
			}
		}
	}
}
