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
import JASP.Controls 1.0
import JASP.Theme 1.0


Rectangle {
    default property alias content: column.children
    property bool usesJaspResults: false
    property int formMargin: Theme.formMargin
    property int formWidthAvailable: width - 2 * formMargin
    property int majorVersion: 1
    property int minorVersion: 0
    width: Theme.formWidth
    color: Theme.analysisBackgroundColor
    
    property var jaspControls: []
    
    function getJASPControls(controls, item) {
        for (var i = 0; i < item.children.length; ++i) {
            var child = item.children[i];
            
            if (child instanceof ExpanderButton) {
                controls.push(child.button);
                getJASPControls(controls, child.area);
            } else if (child instanceof JASPControl) {
                if (child.hasTabFocus) {
                    controls.push(child);
                } else {
                    getJASPControls(controls, child);
                }
            } else {
                getJASPControls(controls, child);
            }
        }            
    }        
     
    TextField { visible: false; name: "plotWidth"; inputType: "integer"; text: "480" }
    TextField { visible: false; name: "plotHeight"; inputType: "integer"; text: "320" }
    
    
    Flickable {
        id: flickable
        anchors.fill: parent
        anchors.margins: formMargin
        contentHeight: column.childrenRect.height
        
        Rectangle {
            id: errorMessagesBox
            property alias text: errorMessagesText.text
            objectName: "errorMessagesBox"
            visible: false
            color: Theme.errorMessagesBackgroundColor
            width: parent.width
            height: errorMessagesText.height;
            Text {
                padding: 5
                verticalAlignment: Text.AlignVCenter
                id: errorMessagesText
            }
        }

        ColumnLayout {
            id: column
            anchors.top: errorMessagesBox.visible ? errorMessagesBox.bottom : parent.top
            spacing: 10
            width: parent.width            
        }

        ScrollBar.vertical: ScrollBar { policy: ScrollBar.AsNeeded}
    }
    
    Component.onCompleted: {
        var previousExpander = null;
        getJASPControls(jaspControls, column);
        for (var i = 0; i < jaspControls.length; i++) {
            var next = i >= (jaspControls.length-1) ? 0 : i+1;
            if (jaspControls[i].controlType !== "Expander")
                jaspControls[i].KeyNavigation.tab = jaspControls[next];
            else {
                if (previousExpander)
                    previousExpander.nextExpander = jaspControls[i];
                previousExpander = jaspControls[i];
            }
        }
        
        if (previousExpander)
            previousExpander.nextExpander = jaspControls[0];
    }
}

