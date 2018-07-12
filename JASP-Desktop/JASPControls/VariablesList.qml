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
import QtQml.Models 2.2
import Theme 1.0

JASPControl
{
    id: variablesList
    controlType: "ListView"
    backgroundRectangle: rectangle
    implicitWidth: parent.implicitWidth / 3
    implicitHeight: singleItem ? 45 : 350
    
    property var model
    property string listViewType: "availableVariables"
    property string itemType: "variables"
    property string title
    property alias dropKeys: dropArea.keys
    property bool singleItem: false
    property bool hasSelectedItems: false; // Binding does not work on array length: listView.selectedItems.length > 0;
    property var syncModels
    
    readonly property int rectangleY: rectangle.y    
    readonly property string nominalIconFile: "variable-nominal-inactive.svg"
    readonly property string ordinalIconFile: "variable-ordinal-inactive.svg"
    readonly property string scaleIconFile: "variable-scale-inactive.svg"
    property var suggestedColumns: []
    property var allowedColumns: []
    property bool showVariableIcon: true
    property bool showElementBorder: false
    property int columns: 1
    
    signal itemDoubleClicked(int index);
    signal itemsDropped(var indexes, var dropList);
        
    function selectedItemsChanged() {
        hasSelectedItems = (listView.selectedItems.length > 0);
    }
    
    function moveSelectedItems(target) {
        console.log('move');
        if (!hasSelectedItems) {
            console.log('no item selected');
            return;
        }

        var selectedIndexes = [];
        for (var i = 0; i < listView.selectedItems.length; i++) {
            var selectedItem = listView.selectedItems[i];            
            selectedIndexes.push(selectedItem.rank);
        }
        
        // itemsDropped will change the listView, and that may call the onCurrentItemChanged
        // So we have to clear the selected items list before.
        listView.clearSelectedItems();
        itemsDropped(selectedIndexes, target);
    }    
    
    DropArea {
        id: dropArea
        anchors.fill: parent
        onEntered: {
             console.log(variablesList.name + ": Droparea entered" );
        }
        onContainsDragChanged: console.log(variablesList.name + ": containsDrag changed: " + (containsDrag ? "true" : "false"))
    }
    
    Text {
        id: text
        anchors.top: parent.top
        anchors.left: parent.left
        text: title
        height: title ? 20 : 0
    }    
    
    Rectangle {
        id: rectangle
        anchors.top: text.bottom
        anchors.left: parent.left
        height: variablesList.height - text.height
        width: parent.width
        color: debug ? Theme.debugBackgroundColor : Theme.controlBackgroundColor
        border.width: 1
        border.color: {
            if (dropArea.containsDrag) return Theme.containsDragBorderColor;
            return Theme.borderColor;
        }
        
        states: [
            State {
                when: dropArea.containsDrag
                PropertyChanges {
                    target: rectangle
                    border.width: 4
                    radius: 3
                }
            }
        ]
                
        Image {
            id: nominalIcon
            source: iconFolder + nominalIconFile
            visible: false
            height: 16
            width: 16
            z: 2
        }
        Image {
            id: ordinalIcon
            source: iconFolder + ordinalIconFile
            visible: false
            height: 16
            width: 16
            z: 2
        }
        Image {
            id: scaleIcon
            source: iconFolder + scaleIconFile
            visible: false
            height: 16
            width: 16
            z: 2
        }
        Component.onCompleted: {
            if (suggestedColumns.length === 0)
                suggestedColumns = allowedColumns
            
            for (var i = 0; i < suggestedColumns.length; i++) {
                var icon;
                if (suggestedColumns[i] === "nominal")
                    icon = nominalIcon;
                else if (suggestedColumns[i] === "ordinal")
                    icon = ordinalIcon;
                else if (suggestedColumns[i] === "scale")
                    icon = scaleIcon;
                if (icon) {
                    icon.anchors.bottom = rectangle.bottom;
                    icon.anchors.bottomMargin = 4
                    icon.anchors.right = rectangle.right;
                    icon.anchors.rightMargin = i * 20 + 4
                    icon.visible = true;
                }
            }
        }
        
        GridView {
            id: listView
            ScrollBar.vertical: ScrollBar {
                policy: ScrollBar.AsNeeded
            }
            cellHeight: 20
            cellWidth: width / variablesList.columns
            clip: true
            focus: true
            anchors.fill: parent
            anchors.margins: 4
            model: variablesList.model
            delegate: itemComponent
            
            property int startShiftSelected: 0;
            property int endShiftSelected: -1;
            property var selectedItems: [];
            property bool mousePressed: false;
            property bool shiftPressed: false;
            
            onCurrentItemChanged: {
                console.log(variablesList.name + ": index changed: " + listView.currentIndex)
                if (shiftPressed) {
                    console.log("item changed with shift pressed")
                    if (endShiftSelected >= 0)
                        selectShiftItems(false);
                    endShiftSelected = listView.currentIndex;
                    selectShiftItems(true);
                } else if (!mousePressed) {
                    var itemWrapper = listView.currentItem;
                    if (itemWrapper) {
                        var itemRectangle = itemWrapper.children[0];
                        itemWrapper.forceActiveFocus();
                        listView.clearSelectedItems();
                        listView.selectItem(itemRectangle, true);
                        listView.startShiftSelected = listView.currentIndex;
                        listView.endShiftSelected = -1;
                    }
                }
            }
            
            Keys.onPressed: {
                if (event.modifiers & Qt.ShiftModifier) {
                    console.log("Shift pressed")
                    shiftPressed = true;
                }
            }
            
            Keys.onReleased: {
                if (event.modifiers & Qt.ShiftModifier) {
                    console.log("Shift released")
                    shiftPressed = false;            
                }
            }
            
            Keys.onSpacePressed: {
                moveSelectedItems()
            }
            Keys.onReturnPressed: {
                moveSelectedItems()
            }
            
            function addSelectedItem(item) {
                item.selected = true;
                if (selectedItems.find(function(elt) {return elt.rank === item.rank}))
                    return;
                    
                var added = false;
                for (var i = 0; i < selectedItems.length; i++) {
                    if (item.rank < selectedItems[i].rank) {
                        selectedItems.splice(i, 0, item);
                        added = true;
                        break;
                    }
                }
                if (!added)
                    selectedItems.push(item);
                console.log(variablesList.name + ": selected item added");
                variablesList.selectedItemsChanged();
            }
            
            function removeSelectedItem(item) {
                item.selected = false;
                for (var i = 0; i < selectedItems.length; i++) {
                    if (item.rank === selectedItems[i].rank) {
                        selectedItems.splice(i, 1);
                        break;
                    }
                }
                variablesList.selectedItemsChanged();
            }
            
            function selectItem(item, selected) {
                if (selected)
                    listView.addSelectedItem(item);
                else
                    listView.removeSelectedItem(item);
            }        
            
            function clearSelectedItems() {
                for (var i = 0; i < selectedItems.length; i++) {
                    selectedItems[i].selected = false;
                }
                selectedItems = [];
                variablesList.selectedItemsChanged();
            }
            
            function selectShiftItems(selected) {
                var startIndex = listView.startShiftSelected;
                var endIndex = listView.endShiftSelected;
                if (startIndex > endIndex) {
                    var temp = startIndex;
                    startIndex = endIndex;
                    endIndex = temp;
                }
                for (var i = startIndex; i <= endIndex; i++) {
                    var item = listView.contentItem.children[i];
                    if (item)
                        listView.selectItem(item.children[0], selected);
                    else
                        console.log(variablesList.name + ": Unknown item at index " + i);
                }
            }
        }
    }
    
    Component {
        id: itemComponent
        FocusScope {
            id: itemWrapper
            height: listView.cellHeight
            width: listView.cellWidth
            Rectangle {
                id: itemRectangle
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                height: listView.cellHeight
                width: listView.cellWidth
                focus: true
                border.width: variablesList.showElementBorder ? 1 : 0
                border.color: Theme.grayLighter
                
                property bool selected: false
                property bool dragging: false
                property bool clearOtherSelectedItemsWhenClicked: false
                property int offsetX: 0
                property int offsetY: 0
                property int rank: index
                
                function setRelative(draggedRect) {
                    x = Qt.binding(function (){ return draggedRect.x + offsetX; })
                    y = Qt.binding(function (){ return draggedRect.y + offsetY; })
                }
                
                color: {
                    if (itemRectangle.selected)
                        return variablesList.activeFocus ? Theme.itemSelectedColor: Theme.itemSelectedNoFocusColor;
                    else if (mouseArea.containsMouse)
                        return Theme.itemHoverColor;
                    return Theme.controlBackgroundColor;
                }
                Drag.keys: [variablesList.name]
                Drag.active: mouseArea.drag.active
                Drag.hotSpot.x: itemRectangle.width / 2
                Drag.hotSpot.y: itemRectangle.height / 2
                
                ToolTip.visible: mouseArea.containsMouse
                ToolTip.delay: 300
                ToolTip.text: model.name
                ToolTip.toolTip.background: Rectangle {
                        id: tooltipRectangle
                        color: Theme.tooltipBackgroundColor
                        height: 25
                }
              
                Image {
                    id: icon
                    height: 15; width: 15
                    anchors.verticalCenter: parent.verticalCenter
                    source: variablesList.showVariableIcon ? model.type : ""
                    visible: variablesList.showVariableIcon
                }
                Text {
                    id: colName
                    x: variablesList.showVariableIcon ? 20 : 4  
                    text: model.name
                    anchors.verticalCenter: parent.verticalCenter
                    color: itemRectangle.color === Theme.itemSelectedColor ? Theme.white : Theme.black
                }
                
                states: [
                    State {
                        when: itemRectangle.dragging
                        ParentChange {
                            target: itemRectangle
                            parent: form
                        }
                        AnchorChanges {
                            target: itemRectangle
                            anchors.horizontalCenter: undefined
                            anchors.verticalCenter: undefined
                        }                        
                    }
                ]                
                
                MouseArea {
                    id: mouseArea
                    anchors.fill: parent
                    drag.target: parent
                    hoverEnabled: true
                    
                    onDoubleClicked: {
                        console.log("onDoubleClicked");
                        listView.clearSelectedItems(); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
                        itemDoubleClicked(index);
                    }
                    
                    onClicked: {
                        console.log(variablesList.name + ": onClicked");
                        if (itemRectangle.clearOtherSelectedItemsWhenClicked) {
                            listView.clearSelectedItems();
                            listView.selectItem(itemRectangle, true);
                        }
                    }
                    
                    onPressed: {
                        console.log(variablesList.name + ": onPressed");
                        listView.mousePressed = true;
                        listView.currentIndex = index;
                        itemRectangle.clearOtherSelectedItemsWhenClicked = false;
                        if (mouse.modifiers & Qt.ControlModifier) {
                            listView.selectItem(itemRectangle, !itemRectangle.selected);
                            listView.startShiftSelected = index;
                            listView.endShiftSelected = -1;
                        } else if (mouse.modifiers & Qt.ShiftModifier) {
                            if (listView.endShiftSelected >= 0)
                                listView.selectShiftItems(false)
                            listView.endShiftSelected = index;
                            listView.selectShiftItems(true);
                        } else {
                            itemWrapper.forceActiveFocus();
                            if (!itemRectangle.selected) {
                                listView.clearSelectedItems();
                                listView.selectItem(itemRectangle, true);
                            } else {
                                itemRectangle.clearOtherSelectedItemsWhenClicked = true;
                            }

                            listView.startShiftSelected = index;
                            listView.endShiftSelected = -1;
                        }                        
                    }
                    onReleased: {
                        console.log(variablesList.name + ": onReleased")
                        listView.mousePressed = false;
                    }
                    
                    drag.onActiveChanged: {
                        console.log(variablesList.name + ": drag.onActiveChanged: " + (drag.active ? "true" : "false"));
                        if (drag.active) {
                            if (itemRectangle.selected) {
                                itemRectangle.dragging = true;
                                for (var i = 0; i < listView.selectedItems.length; i++) {
                                    var selectedItem = listView.selectedItems[i];
                                    if (selectedItem.rank !== index) {
                                        selectedItem.dragging = true;
                                        console.log(variablesList.name + ": dragging set to true")
                                        selectedItem.offsetX = selectedItem.x - itemRectangle.x;
                                        selectedItem.offsetY = selectedItem.y - itemRectangle.y;
                                        selectedItem.setRelative(itemRectangle);                                
                                    }
                                }
                            }
                           
                        } else {
                            var selectedIndexes = [];
                            for (var i = 0; i < listView.selectedItems.length; i++) {
                                var selectedItem = listView.selectedItems[i];
                                selectedIndexes.push(selectedItem.rank);
                                console.log(variablesList.name + ": dropped item " + selectedItem.rank);
                                selectedItem.dragging = false;
                                console.log(variablesList.name + ": dragging set to true " + selectedItem.rank);
                                selectedItem.x = selectedItem.x; // break bindings
                                selectedItem.y = selectedItem.y;
                            }
                            if (itemRectangle.Drag.target) {
                                var dropTarget = itemRectangle.Drag.target.parent
                                if (dropTarget.singleItem && listView.selectedItems.length > 1)
                                    return;
                                
                                listView.clearSelectedItems(); // Must be before itemsDropped: listView does not exist anymore afterwards
                                var variablesListName = variablesList.name
                                itemsDropped(selectedIndexes, dropTarget);                               
                                console.log(variablesListName + ": items dropped");
                            }
                        }
                    }
                }
            }
        }
    }
}
