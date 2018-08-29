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
import JASP.Theme 1.0

JASPControl {
    controlType: "Switch"
    implicitWidth: control.width; implicitHeight: control.height
    useDefaultBackground: true
    property alias text: control.text
    property alias checked: control.checked
    signal clicked();
    
    Component.onCompleted: {
        control.clicked.connect(clicked);
    }
    
    Switch {
        id: control
        height: control.indicator.height + 4
        width: control.indicator.width + label.implicitWidth + control.spacing + 6
        focus: true
                
        indicator: Rectangle {
            id: switchHandle
            width: Theme.switchHeight * 2.2
            height: Theme.switchHeight
            anchors.left: control.left
            anchors.leftMargin: 2
            anchors.top: control.top
            anchors.topMargin: 2            
            radius: Theme.switchHeight / 2
            color: Theme.light
            border.color: Theme.borderColor
    
            Rectangle {
                id: rectangle
    
                width: Theme.switchHeight
                height: Theme.switchHeight
                radius: Theme.switchHeight / 2
                color: Theme.light
                border.color: Theme.borderColor
            }
        }
        
        contentItem: Label {
            id: label
            anchors.left: control.indicator.right
            anchors.leftMargin: control.spacing
            anchors.top: control.top
            anchors.topMargin: 2
            text: control.text
        }
    }
}
