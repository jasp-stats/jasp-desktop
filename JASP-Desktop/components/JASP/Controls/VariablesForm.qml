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
import JASP.Theme 1.0

Item {
    id: variablesForm
    implicitWidth: parent.width
    height: Theme.defaultListHeight
    implicitHeight: height
    
    default property alias content: items.children
    property int listWidth: parent.width * 2 / 5
    property alias availableVariablesList: availableVariablesList
    property alias defaultAssignedVariablesList: defaultAssignedVariablesList
    
    property int marginBetweenVariablesLists: 8
    property bool showDefaultAssignedVariablesList: true

    VariablesList {
        id: availableVariablesList
        name: "allAvailableVariables"
        width: listWidth
        height: parent.height
        anchors.top: parent.top
        anchors.left: parent.left
    }

    AssignedVariablesList {
        id: defaultAssignedVariablesList
        name: showDefaultAssignedVariablesList ? "variables" : ""
        width: listWidth
        visible: showDefaultAssignedVariablesList
    }
    
    Item {
        id: items
    }
    
    Component.onCompleted: {
        var titleHeight = Theme.variablesListTitle;
        
        if (availableVariablesList.title && !defaultAssignedVariablesList.title)
            defaultAssignedVariablesList.title = " "; // Add a space as title so that the assignedList aligns with the availableList
        var allJASPControls = showDefaultAssignedVariablesList ? [defaultAssignedVariablesList] : []
        
        for (var i = 0; i < items.children.length; ++i) {
            var child = items.children[i];
            if (child.name && child.visible) {
                allJASPControls.push(child);                
            }
        }
        
        var minHeight = 0;
        var allAssignedVariablesList = [];
        var changeableHeightControls = [];
        for (i = 0; i < allJASPControls.length; i++) {
            var control = allJASPControls[i];
            if (control instanceof AssignedVariablesList)
                allAssignedVariablesList.push(control);
            if (i > 0)
                minHeight += marginBetweenVariablesLists;
            if (control.singleItem)
                minHeight += control.height;
            else if (control.height !== Theme.defaultListHeight)
                // If the height of this List item was changed, don't change it
                minHeight += control.height
            else {
                changeableHeightControls.push(control);
                if (control.title)
                    minHeight += titleHeight;
            }
            // Do not set the parent in the previous loop: this removes it from the items children. 
            allJASPControls[i].parent = variablesForm;
        }        
        
        // Set the height of controls (that have not singleItem set or where the height is already specifically set)
        // so that the AssignedVariablesList column is as long as the AvailableVariablesList column. 
        if (changeableHeightControls.length > 0) {
            var controlHeight = (availableVariablesList.height - minHeight) / changeableHeightControls.length;
            if (controlHeight < 25)
                controlHeight = 25; // Set a minimum height
            for (i = 0; i < changeableHeightControls.length; i++) {
                changeableHeightControls[i].height = changeableHeightControls[i].title ? (titleHeight + controlHeight) : controlHeight;
            }
        }
        
        var anchorTop = variablesForm.top;
        for (i = 0; i < allJASPControls.length; ++i) {
            allJASPControls[i].width = Qt.binding(function (){ return variablesForm.listWidth});
            allJASPControls[i].anchors.top = anchorTop;
            allJASPControls[i].anchors.topMargin = i === 0 ? 0 : marginBetweenVariablesLists;
            allJASPControls[i].anchors.right = variablesForm.right;
            anchorTop = allJASPControls[i].bottom;
        }
                
        var component = Qt.createComponent("AssignButton.qml");
        for (i = 0; i < allAssignedVariablesList.length; i++) {
            var assignedList = allAssignedVariablesList[i];
            availableVariablesList.dropKeys.push(assignedList.name);
            assignedList.dropKeys.push(availableVariablesList.name);
            for (var j = 0; j < allAssignedVariablesList.length; ++j) {
                assignedList.dropKeys.push(allAssignedVariablesList[j].name);
            }
            
            var assignButton = component.createObject(variablesForm, 
                                   {"leftSource" : availableVariablesList,
                                    "rightSource" : assignedList});
        }        
    }

}
