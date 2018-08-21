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
	width: (handleSize + 2 * (backScrollbar.border.width +1));
	visible: (flickable.visibleArea.heightRatio < 1.0);
	anchors {
		top: flickable.top;
		right: flickable.right;
		bottom: flickable.bottom;
		margins: 0;
	}
	
	property Flickable flickable               : null;
	property int       handleSize              : 20;
	property string bkColor                    : Qt.rgba(0.5, 0.5, 0.5, 0.85);
	property string fgColor                    : "black";
	property string pressedColor               : "#72a0cc" ;
	property bool outerradius                   : true;
	property bool innerradius                   : true;
	property bool showarrows                   : false;
	
	function scrollDown () {
		flickable.contentY = Math.min (flickable.contentY + (flickable.height / 4), flickable.contentHeight - flickable.height);
	}
	function scrollUp () {
		flickable.contentY = Math.max (flickable.contentY - (flickable.height / 4), 0);
	}
	
	Binding {
		target: handle;
		property: "y";
		value: (flickable.contentY * clicker.drag.maximumY / (flickable.contentHeight - flickable.height));
		when: (!clicker.drag.active);
	}
	Binding {
		target: flickable;
		property: "contentY";
		value: (handle.y * (flickable.contentHeight - flickable.height) / clicker.drag.maximumY);
		when: (clicker.drag.active || clicker.pressed);
	}
	Rectangle {
		id: backScrollbar;
		radius: outerradius ? width/2 : 0;
		antialiasing: true;
		color: bkColor;
		border {
			width: 2;
			color: "darkgray";
		}
		anchors { fill: parent; }
		
		MouseArea {
			anchors.fill: parent;
			onClicked: { }
		}
	}
	MouseArea {
		id: btnUp;
		height: showarrows ? width : 0;
		anchors {
			top: parent.top;
			left: parent.left;
			right: parent.right;
			margins: (backScrollbar.border.width +1);
		}
		onClicked: { scrollUp (); }
		
		Text {
			text: showarrows ? "V" : "";
			color: (btnUp.pressed ? "blue" : "black");
			rotation: -180;
			anchors.centerIn: parent;
		}
	}
	MouseArea {
		id: btnDown;
		height: showarrows ? width : 0;
		anchors {
			left: parent.left;
			right: parent.right;
			bottom: parent.bottom;
			margins: (backScrollbar.border.width +1);
		}
		onClicked: { scrollDown (); }
		
		Text {
			text: showarrows ? "V" : "";
			color: (btnDown.pressed ? "blue" : "black");
			anchors.centerIn: parent;
		}
	}
	Item {
		id: groove;
		clip: true;
		anchors {
			fill: parent;
			topMargin: (backScrollbar.border.width +1 + btnUp.height +1);
			leftMargin: (backScrollbar.border.width +1);
			rightMargin: (backScrollbar.border.width +1);
			bottomMargin: (backScrollbar.border.width +1 + btnDown.height +1);
		}
		
		MouseArea {
			id: clicker;
			drag {
				target: handle;
				minimumY: 0;
				maximumY: (groove.height - handle.height);
				axis: Drag.YAxis;
			}
			anchors { fill: parent; }
			onClicked: { flickable.contentY = (mouse.y / groove.height * (flickable.contentHeight - flickable.height)); }
		}
		Item {
			id: handle;
			height: Math.max (20, (flickable.visibleArea.heightRatio * groove.height));
			anchors {
				left: parent.left;
				right: parent.right;
			}
			
			Rectangle {
				id: backHandle;
				radius: innerradius ? width/2 : 0;
				color: (clicker.pressed ? pressedColor : fgColor);
				opacity: (flickable.moving ? 0.5 : (clicker.pressed ? 1 : 0.2));
				anchors { fill: parent; }
				
				Behavior on opacity { NumberAnimation { duration: 150; } }
			}
		}
	}
}
