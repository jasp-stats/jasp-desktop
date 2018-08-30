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
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: control
    controlType: "ButtonGroup"
    hasTabFocus: false

    default property alias content: items.children
    property alias buttons: buttonGroup.buttons
    property bool isHorizontal: false
    property string title: ""
    property int leftPadding: Theme.groupContentPadding

    signal clicked(var item)

    implicitHeight: (control.title ? label.height : 0) + (isHorizontal ? row.height : column.height) 
    implicitWidth: (isHorizontal ? row.width : column.width) + (title ? control.leftPadding : 0)
       
    ButtonGroup {
        id: buttonGroup
    }

    Label {
        id: label
        text: control.title
        visible: control.title && control.visible ? true : false
        anchors.top: control.top
        anchors.left: control.left
        anchors.bottomMargin: Theme.rowSpacing
    }

    Item {
        id: items
        visible: false
    }

    ColumnLayout {
        id: column
        anchors.top: control.title ? label.bottom : control.top
        anchors.left: parent.left
        anchors.leftMargin: control.title ? control.leftPadding : 0
        spacing: Theme.rowSpacing
        visible: isHorizontal && control.visible ? false : true
    }

    RowLayout {
        id: row
        anchors.top: control.title ? label.bottom : control.top
        anchors.left: parent.left
        anchors.leftMargin: control.title ? control.leftPadding : 0
        spacing: Theme.rowSpacing
        visible: isHorizontal && control.visible ? true : false
    }
    
    function linkRadioButtons(item) {
        for (var i = 0; i < item.children.length; ++i) {
            var child = item.children[i];
            if (child instanceof JASPControl) {
                if (control.debug)
                    child.debug = true;
                if (child.controlType === "RadioButton") {
                    child.buttonGroup = buttonGroup;
                }
                else if (child.controlType !== "ButtonGroup") {
                    linkRadioButtons(child);
                } else {
                    console.log("ButtonGroup " + child.name + " found for " + control.name)                    
                }
            } else {
                linkRadioButtons(child)
            }
        }        
    }

    Component.onCompleted: {
        buttonGroup.clicked.connect(clicked);
        linkRadioButtons(items);
        var elt = isHorizontal ? row : column;
        while (items.children.length > 0) {
            var child = items.children[0];
            child.parent = elt;
        }
    }

}

