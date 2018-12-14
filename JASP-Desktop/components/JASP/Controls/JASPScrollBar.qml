/* Copyright (C) 2014 Jesper K. Pedersen <blackie@kde.org>
  
   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; see the file COPYING.  If not, write to
   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
*/

// Code based on http://stackoverflow.com/questions/17833103/how-to-create-scrollbar-in-qtquick-2-0

import QtQuick 2.0;

Item {
	id: scrollbar;

	property real breadth: visible ? (handleSize + 2 * (backScrollbar.border.width +1)) : 0
	width:  vertical ? breadth   : undefined;
	height: vertical ? undefined : breadth;

	property real extraMarginRightOrBottom	: 0
	property real extraMarginLeftOrTop		: 0
	property Flickable flickable			: null;
	property int handleSize					: 7;
	property int minimumLength				: 16
	property string bkColor					: Theme.white; //Use JASPTheme when available!
	property string fgColor					: Theme.black;
	property string pressedColor			: "#72a0cc";
	property bool outerradius				: false;
	property bool innerradius				: false;
	property bool showarrows				: false;
	property bool vertical					: true;
	property bool manualAnchor				: false;

	visible: (vertical ? flickable.visibleArea.heightRatio : flickable.visibleArea.widthRatio ) < 1.0;

	anchors {
		top:			manualAnchor ? 0 : vertical ? flickable.top : undefined;
		left:			manualAnchor ? 0 : vertical ? undefined	 :	flickable.left;
		right:			manualAnchor ? 0 : flickable.right;
		bottom:			manualAnchor ? 0 : flickable.bottom;
		topMargin:		manualAnchor ? 0 : vertical ? extraMarginLeftOrTop		: 0
		leftMargin:		manualAnchor ? 0 : vertical ? 0						: extraMarginLeftOrTop
		rightMargin:	manualAnchor ? 0 : vertical ? 0						: extraMarginRightOrBottom
		bottomMargin:	manualAnchor ? 0 : vertical ? extraMarginRightOrBottom	: 0
	}

	


	function scroll(movement)
	{
		if(vertical)	flickable.contentY = Math.max (0, Math.min (flickable.contentY + (flickable.height * movement), flickable.contentHeight - flickable.height));
		else			flickable.contentX = Math.max (0, Math.min (flickable.contentX + (flickable.width  * movement), flickable.contentWidth  - flickable.width));
	}

	function scrollDown() { scroll( 0.25); }
	function scrollUp ()  { scroll(-0.25); }
	
	Binding {
		target: handle;
		property: scrollbar.vertical ? "y" : "x";
		value: scrollbar.vertical ?
					(flickable.contentY * clicker.drag.maximumY / (flickable.contentHeight - flickable.height)):
					(flickable.contentX * clicker.drag.maximumX / (flickable.contentWidth  - flickable.width));
		when: (!clicker.drag.active);
	}

	Binding {
		target: flickable;
		property: scrollbar.vertical ? "contentY" : "contentX";
		value: scrollbar.vertical ?
					(handle.y * (flickable.contentHeight - flickable.height) / clicker.drag.maximumY):
					(handle.x * (flickable.contentWidth  - flickable.width)  / clicker.drag.maximumX);
		when: (clicker.drag.active || clicker.pressed);
	}

	Rectangle {
		id: backScrollbar;
		radius: outerradius ? width/2 : 0;
		antialiasing: true;
		color: bkColor;
		border {
			width: 1;
			color: Theme.grayDarker;
		}
		anchors { fill: parent; }
		
		MouseArea {
			anchors.fill: parent;
			onClicked: { }
		}
	}

	MouseArea {
		id: btnUp;
		property real size: !showarrows ? 0 : scrollbar.vertical ? scrollbar.width : scrollbar.height;
		height: size
		width:  size

		anchors {
			top:	parent.top;
			left:	parent.left;
			right:	scrollbar.vertical ? parent.right : undefined;
			bottom:	scrollbar.vertical ? undefined    : parent.bottom;

			margins: (backScrollbar.border.width +1);
		}
		onClicked: { scrollUp (); }
		
		Text {
			text:					showarrows ? "Δ" : "";
			color:					(btnUp.pressed ? "blue" : Theme.black);
			rotation:				scrollbar.vertical ? 0 : -90;
			anchors.centerIn:		parent;
			horizontalAlignment:	Text.AlignHCenter
			font.pixelSize:			btnUp.size
		}
	}

	MouseArea {
		id: btnDown;
		height: btnUp.size;
		width:	btnUp.size;

		anchors {
			top:	scrollbar.vertical ? undefined   : parent.top;
			left:	scrollbar.vertical ? parent.left : undefined;
			right:	parent.right;
			bottom: parent.bottom;
			margins: (backScrollbar.border.width +1);
		}
		onClicked: { scrollDown (); }
		
		Text {
			text:					showarrows ? "Δ" : "";
			color:					(btnDown.pressed ? "blue" : Theme.black);
			rotation:				scrollbar.vertical ? -180 : 90;
			anchors.centerIn:		parent;
			horizontalAlignment:	Text.AlignHCenter
			font.pixelSize:			btnUp.size
		}
	}

	Item {
		id: groove;
		clip: true;
		property real basicMargin:			  backScrollbar.border.width + 1
		property real extraVerticalMargin:	  scrollbar.vertical ? btnUp.size + 1 : 0
		property real extraHorizontalMargin: !scrollbar.vertical ? btnUp.size + 1 : 0

		anchors {
			fill: parent;
			topMargin:		basicMargin + extraVerticalMargin
			leftMargin:		basicMargin + extraHorizontalMargin
			rightMargin:	basicMargin + extraHorizontalMargin
			bottomMargin:	basicMargin + extraVerticalMargin
		}
		
		MouseArea {
			id: clicker;
			drag {
				target: handle;
				minimumY: !scrollbar.vertical ? 0 : 0;
				maximumY: !scrollbar.vertical ? 0 : (groove.height - handle.height);
				axis: Drag.XAndYAxis
				minimumX: scrollbar.vertical ? 0 : 0;
				maximumX: scrollbar.vertical ? 0 : (groove.width - handle.width);

			}
			anchors { fill: parent; }
			onClicked:
			{
				if(scrollbar.vertical)	flickable.contentY = (mouse.y / groove.height * (flickable.contentHeight - flickable.height));
				else					flickable.contentX = (mouse.x / groove.width  * (flickable.contentWidth  - flickable.width));
			}
		}

		Item {
			id: handle;
			height: !scrollbar.vertical ? parent.height : Math.max (scrollbar.minimumLength, (flickable.visibleArea.heightRatio * groove.height))
			width:   scrollbar.vertical ? parent.width	: Math.max (scrollbar.minimumLength, (flickable.visibleArea.widthRatio  * groove.width))
			anchors {
				top:	scrollbar.vertical ? undefined		: parent.top
				left:	scrollbar.vertical ? parent.left	: undefined
				right:	scrollbar.vertical ? parent.right	: undefined
				bottom:	scrollbar.vertical ? undefined		: parent.bottom
			}
			
			Rectangle {
				id:			backHandle;
				radius:		innerradius ? width/2 : 0;
				color:		(clicker.pressed ? pressedColor : fgColor);
				opacity:	(flickable.moving ? 0.5 : (clicker.pressed ? 1 : 0.2));
				anchors		{ fill: parent; }
				
				Behavior on opacity { NumberAnimation { duration: 150; } }
			}
		}
	}
}
