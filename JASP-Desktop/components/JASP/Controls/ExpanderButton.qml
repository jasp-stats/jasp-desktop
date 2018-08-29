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
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

FocusScope {
    id: expanderWrapper
    implicitHeight: expanderButton.height + (expanded ? 15 + expanderArea.height : 0)
    implicitWidth: form.formWidthAvailable
    anchors.topMargin: 15
    default property alias content: expanderArea.children
    
    property alias button: expanderButton
    property alias area: expanderArea
    property alias spacing: expanderArea.spacing
    property alias text: label.text
    property bool expanded: false
    property alias debug: expanderButton.debug

    readonly property string iconsFolder: "qrc:/images/"
    readonly property string expandedIcon: "expander-arrow-down.png"
    readonly property string contractedIcon: "expander-arrow-up.png"
    
    property var childControls: []

    clip: true
    Behavior on implicitHeight {
        PropertyAnimation {duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3}
    }
  

    JASPControl {
        id: expanderButton
        controlType: "Expander"
        isBound: false
        controlBackground: expanderRectangle
        width: parent.width
        height: 22
        
        property var nextExpander: null
        
        function toggleExpander() {
            expanderWrapper.expanded = !expanderWrapper.expanded;            
        } 
        
        Keys.onSpacePressed: {
            toggleExpander()
        }
        Keys.onReturnPressed: {
            toggleExpander()
        }
        
        KeyNavigation.tab: expanderWrapper.expanded ? childControls[0] : nextExpander

        MouseArea {
            anchors.fill: parent
            onClicked: {
                expanderButton.toggleExpander();
                expanderButton.forceActiveFocus();
            }
        }
        
        Rectangle {
            id: expanderRectangle
            anchors.fill: parent
            border.width: 1
            border.color: Theme.borderColor
            radius: Theme.borderRadius
            color: debug ? Theme.debugBackgroundColor : Theme.white
            
            Image {
                id: icon
                height: 15; width: 15
                anchors.left: parent.left
                anchors.leftMargin: 6
                anchors.verticalCenter: parent.verticalCenter
                source: iconsFolder + (expanded ? expandedIcon : contractedIcon)
            }
            Text {
                id: label
                anchors.left: icon.right
                anchors.leftMargin: 5
                anchors.verticalCenter: parent.verticalCenter
            }
        }
    }

    ColumnLayout {
        id: expanderArea
        spacing: 10
        anchors.leftMargin: 5 
        anchors.top: expanderButton.bottom
        anchors.topMargin: 15
        anchors.bottomMargin: 20
        width: parent.width
    }
    
    Component.onCompleted: {
        form.getJASPControls(childControls, expanderArea)
        if (debug) {
            for (var i = 0; i < childControls.length; i++) {
                childControls[i].debug = true;
            }
        }
    }

}
