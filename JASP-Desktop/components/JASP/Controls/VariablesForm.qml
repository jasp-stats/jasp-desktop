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
    implicitWidth: form.formWidthAvailable
    implicitHeight: availableVariablesList.height
    
    default property alias content: items.children
    property alias formHeight: availableVariablesList.height
    property alias availableVariablesList: availableVariablesList
    property alias defaultAssignedVariablesList: assignedVariablesList
    
    property int marginBetweenVariablesLists: 8
    property bool showDefaultAssignedVariablesList: true
    
    readonly property int defaultListHeight: Theme.defaultListHeight

    VariablesList {
        id: availableVariablesList
        name: "allAvailableVariables"
        anchors.top: parent.top
        anchors.left: parent.left
    }

    AssignedVariablesList {
        id: assignedVariablesList
        name: "variables"
        visible: showDefaultAssignedVariablesList
    }
    
    Item {
        id: items
    }
    
    Component.onCompleted: {
        var titleHeight = 20;
        var marginBetweenList = marginBetweenVariablesLists;
        
        if (availableVariablesList.title && !assignedVariablesList.title)
            assignedVariablesList.title = " "; // Add a space as title so that the assignedList aligns with the availableList
        var allAssignedVariablesList = showDefaultAssignedVariablesList ? [assignedVariablesList] : [];
        var allJASPControls = showDefaultAssignedVariablesList ? [assignedVariablesList] : []
        var minHeight = 0;
        var multiItemsAssignedVariables = [];
        if (showDefaultAssignedVariablesList) {
            if (assignedVariablesList.singleItem)
                minHeight = assignedVariablesList.implicitHeight;
            else {
                multiItemsAssignedVariables.push(assignedVariablesList);
                if (assignedVariablesList.title)
                    minHeight = titleHeight;
            }
        }
        for (var i = 0; i < items.children.length; ++i) {
            var child = items.children[i];
            if (child.name && child.visible) {
                if (child instanceof AssignedVariablesList)
                    allAssignedVariablesList.push(child);
                minHeight += marginBetweenList;
                if (child.singleItem)
                    minHeight += child.implicitHeight;
                else if (child.height !== defaultListHeight)
                    minHeight += child.height
                else {
                    multiItemsAssignedVariables.push(child);
                    if (child.title)
                        minHeight += titleHeight;
                }
                allJASPControls.push(child);                
            }
        }
        
        for (i = 0; i < allJASPControls.length; i++) {
            // Do not set the parent in the previous loop: this removes it from the items children. 
            allJASPControls[i].parent = variablesForm;
        }
        
        if (multiItemsAssignedVariables.length > 0) {
            var multiItemsHeight = (availableVariablesList.height - minHeight) / multiItemsAssignedVariables.length;
            if (multiItemsHeight < 25)
                multiItemsHeight = 25;
            for (i = 0; i < multiItemsAssignedVariables.length; i++) {
                var _height = multiItemsAssignedVariables[i].title ? (titleHeight + multiItemsHeight) : multiItemsHeight;
                multiItemsAssignedVariables[i].height = _height;
                multiItemsAssignedVariables[i].implicitHeight = _height;
            }
        }
        var anchorTop = variablesForm.top;
        for (i = 0; i < allJASPControls.length; ++i) {
            allJASPControls[i].anchors.top = anchorTop;
            allJASPControls[i].anchors.topMargin = i === 0 ? 0 : marginBetweenList;
            allJASPControls[i].anchors.right = variablesForm.right
            allJASPControls[i].anchors.rightMargin = 40 // Due to the f...g OK button            
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
