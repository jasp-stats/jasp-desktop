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

<<<<<<< HEAD
import QtQuick 2.10
import QtQuick.Controls 2.3
=======
import QtQuick 2.11
import QtQuick.Controls 2.4
>>>>>>> qmlFormsB
import QtQml.Models 2.2
import JASP.Theme 1.0

JASPControl
{
    id: variablesList
<<<<<<< HEAD
    controlType: "ListView"
    backgroundRectangle: rectangle
    implicitWidth: parent.implicitWidth / 3
    implicitHeight: singleItem ? 45 : 350
    
    property var model
    property string listViewType: "availableVariables"
=======
    controlType: "VariablesListView"
    controlBackground: rectangle
    implicitWidth: parent.width
    height: singleItem ? Theme.defaultSingleItemListHeight : Theme.defaultListHeight
    implicitHeight: height
    
    property var model
    property string listViewType: "AvailableVariables"
>>>>>>> qmlFormsB
    property string itemType: "variables"
    property string title
    property alias dropKeys: dropArea.keys
    property bool singleItem: false
<<<<<<< HEAD
=======
    property bool hasExtraControlColumns: false
>>>>>>> qmlFormsB
    property bool hasSelectedItems: false; // Binding does not work on array length: listView.selectedItems.length > 0;
    property var syncModels
    
    readonly property int rectangleY: rectangle.y    
    readonly property string nominalIconFile: "variable-nominal-inactive.svg"
    readonly property string ordinalIconFile: "variable-ordinal-inactive.svg"
    readonly property string scaleIconFile: "variable-scale-inactive.svg"
    property var suggestedColumns: []
    property var allowedColumns: []
<<<<<<< HEAD
    property bool showVariableIcon: true
    property bool showElementBorder: false
    property int columns: 1
    
    signal itemDoubleClicked(int index);
    signal itemsDropped(var indexes, var dropList);
=======
    property bool showVariableTypeIcon: true
    property bool showElementBorder: false
    property int columns: 1
    property bool draggable: true
    property string dropMode: "None"
    property bool dropModeInsert: dropMode === "Insert"
    property bool dropModeReplace: dropMode === "Replace"
    property bool dragOnlyVariables: false
    
    property var components: []
    property var controlColumns: []
    
    property int indexInDroppedListViewOfDraggedItem: -1
        
    signal itemDoubleClicked(int index);
    signal itemsDropped(var indexes, var dropList, int dropItemIndex);
    signal removeRowWithControls(string name);
    signal addRowWithControls(string name, var columns);    
>>>>>>> qmlFormsB
        
    function selectedItemsChanged() {
        hasSelectedItems = (listView.selectedItems.length > 0);
    }
    
    function moveSelectedItems(target) {
<<<<<<< HEAD
        console.log('move');
=======
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
        itemsDropped(selectedIndexes, target);
=======
        itemsDropped(selectedIndexes, target, -1);
>>>>>>> qmlFormsB
    }    
    
    DropArea {
        id: dropArea
        anchors.fill: parent
<<<<<<< HEAD
        onEntered: {
             console.log(variablesList.name + ": Droparea entered" );
        }
        onContainsDragChanged: console.log(variablesList.name + ": containsDrag changed: " + (containsDrag ? "true" : "false"))
=======
        onPositionChanged: {
            if (variablesList.singleItem || (!variablesList.dropModeInsert && !variablesList.dropModeReplace)) return;
            var itemIndex = Math.floor((drag.y - text.height) / listView.cellHeight);
            if (variablesList.columns > 1) {
                itemIndex = itemIndex * 2 + Math.floor(drag.x / listView.cellWidth);
            }

            if (itemIndex >= 0 && itemIndex < listView.contentItem.children.length) {
                var item = listView.contentItem.children[itemIndex].children[0];
                if (item && item.objectName === "itemRectangle") {
                    listView.itemContainingDrag = item
                    variablesList.indexInDroppedListViewOfDraggedItem = itemIndex
                } else {
                    console.log("dropArea: could not find child!")
                }
            } else {
                listView.itemContainingDrag = null
                variablesList.indexInDroppedListViewOfDraggedItem = -1
            }
        }
        onExited: {
            listView.itemContainingDrag = null
            variablesList.indexInDroppedListViewOfDraggedItem = -1
        }
>>>>>>> qmlFormsB
    }
    
    Text {
        id: text
        anchors.top: parent.top
        anchors.left: parent.left
        text: title
<<<<<<< HEAD
        height: title ? 20 : 0
=======
        height: title ? Theme.variablesListTitle : 0
>>>>>>> qmlFormsB
    }    
    
    Rectangle {
        id: rectangle
        anchors.top: text.bottom
        anchors.left: parent.left
        height: variablesList.height - text.height
        width: parent.width
        color: debug ? Theme.debugBackgroundColor : Theme.controlBackgroundColor
        border.width: 1
<<<<<<< HEAD
        border.color: {
            if (dropArea.containsDrag) return Theme.containsDragBorderColor;
            return Theme.borderColor;
        }
=======
        border.color: dropArea.containsDrag ? Theme.containsDragBorderColor : Theme.borderColor
>>>>>>> qmlFormsB
        
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
<<<<<<< HEAD
=======
            
            var previousTitle;
            var length = variablesList.resources.length
            for (var i = length - 1; i >= 0; i--) {
                var column = variablesList.resources[i];
                if (column instanceof ExtraControlColumn) {
                    variablesList.hasExtraControlColumns = true;
                    variablesList.controlColumns.push(column)
                    var columnType = column.type
                    var component = Qt.createComponent(columnType + ".qml");
                    var labelComponent = Qt.createComponent("Label.qml");
                    components.push(component);
                    if (column.title) {
                        var extraTitle = labelComponent.createObject(variablesList, {text: column.title});
                        extraTitle.anchors.right = previousTitle ? previousTitle.left : variablesList.right;
                        extraTitle.anchors.top = variablesList.top;
                    }
                }
            }
>>>>>>> qmlFormsB
        }
        
        GridView {
            id: listView
            ScrollBar.vertical: ScrollBar {
<<<<<<< HEAD
                policy: ScrollBar.AsNeeded
=======
                policy: ScrollBar.AlwaysOff
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
            
            onCurrentItemChanged: {
                console.log(variablesList.name + ": index changed: " + listView.currentIndex)
                if (shiftPressed) {
                    console.log("item changed with shift pressed")
=======
            property var itemContainingDrag
            
            onCurrentItemChanged: {
                if (shiftPressed) {
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
                    console.log("Shift pressed")
                    shiftPressed = true;
=======
                    shiftPressed = true;
                } else {
                    shiftPressed = false;
>>>>>>> qmlFormsB
                }
            }
            
            Keys.onReleased: {
                if (event.modifiers & Qt.ShiftModifier) {
<<<<<<< HEAD
                    console.log("Shift released")
=======
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
=======
                if (!item || item.objectName !== "itemRectangle") {
                    console.log("item is not an itemRectangle!!!!")
                    return;
                }
                if (!item.draggable)
                    return;
                
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
                console.log(variablesList.name + ": selected item added");
=======
>>>>>>> qmlFormsB
                variablesList.selectedItemsChanged();
            }
            
            function removeSelectedItem(item) {
<<<<<<< HEAD
=======
                if (!item || item.objectName !== "itemRectangle")
                    return;
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
            Rectangle {
                id: itemRectangle
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                height: listView.cellHeight
                width: listView.cellWidth
                focus: true
                border.width: variablesList.showElementBorder ? 1 : 0
                border.color: Theme.grayLighter
=======
            
            Rectangle {
                id: itemRectangle
                objectName: "itemRectangle"
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                // the height & width of itemWrapper & itemRectangle must be set independently of each other:
                // when the rectangle is dragged, it gets another parent but it must keep the same size,                
                height: listView.cellHeight
                width: listView.cellWidth
                focus: true
                border.width: containsDragItem && variablesList.dropModeReplace ? 2 : (variablesList.showElementBorder ? 1 : 0)
                border.color: containsDragItem && variablesList.dropModeReplace ? Theme.containsDragBorderColor : Theme.grayLighter
>>>>>>> qmlFormsB
                
                property bool selected: false
                property bool dragging: false
                property bool clearOtherSelectedItemsWhenClicked: false
                property int offsetX: 0
                property int offsetY: 0
                property int rank: index
<<<<<<< HEAD
=======
                property bool containsDragItem: listView.itemContainingDrag === itemRectangle
                property bool draggable: !variablesList.dragOnlyVariables || model.type === "variable"
>>>>>>> qmlFormsB
                
                function setRelative(draggedRect) {
                    x = Qt.binding(function (){ return draggedRect.x + offsetX; })
                    y = Qt.binding(function (){ return draggedRect.y + offsetY; })
                }
                
                color: {
<<<<<<< HEAD
                    if (itemRectangle.selected)
                        return variablesList.activeFocus ? Theme.itemSelectedColor: Theme.itemSelectedNoFocusColor;
=======
                    if (!itemRectangle.draggable)
                        return Theme.controlBackgroundColor;
                    else if (itemRectangle.selected)
                        return variablesList.activeFocus ? Theme.itemSelectedColor: Theme.itemSelectedNoFocusColor;
                    else if (itemRectangle.containsDragItem && variablesList.dropModeReplace)
                        return Theme.itemSelectedColor;
>>>>>>> qmlFormsB
                    else if (mouseArea.containsMouse)
                        return Theme.itemHoverColor;
                    return Theme.controlBackgroundColor;
                }
                Drag.keys: [variablesList.name]
                Drag.active: mouseArea.drag.active
                Drag.hotSpot.x: itemRectangle.width / 2
                Drag.hotSpot.y: itemRectangle.height / 2
                
<<<<<<< HEAD
                ToolTip.visible: mouseArea.containsMouse
=======
                // Use the ToolTip Attached property to avoid creating ToolTip object for each item
                ToolTip.visible: mouseArea.containsMouse && model.name && !itemRectangle.containsDragItem
>>>>>>> qmlFormsB
                ToolTip.delay: 300
                ToolTip.text: model.name
                ToolTip.toolTip.background: Rectangle {
                        id: tooltipRectangle
                        color: Theme.tooltipBackgroundColor
                        height: 25
                }
<<<<<<< HEAD
=======
                
                Rectangle {
                    height: 2
                    width: parent.width
                    color: Theme.red
                    visible: itemRectangle.containsDragItem && variablesList.dropModeInsert
                }
>>>>>>> qmlFormsB
              
                Image {
                    id: icon
                    height: 15; width: 15
                    anchors.verticalCenter: parent.verticalCenter
<<<<<<< HEAD
                    source: variablesList.showVariableIcon ? model.type : ""
                    visible: variablesList.showVariableIcon
                }
                Text {
                    id: colName
                    x: variablesList.showVariableIcon ? 20 : 4  
                    text: model.name
=======
                    source: variablesList.showVariableTypeIcon ? model.type : ""
                    visible: variablesList.showVariableTypeIcon
                }
                Text {
                    id: colName
                    x: variablesList.showVariableTypeIcon ? 20 : 4  
                    text: model.name
                    width: itemRectangle.width - x
                    elide: Text.ElideRight
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
                        }                        
                    }
                ]                
                
=======
                        }
                        PropertyChanges {
                            target: itemRectangle
                            opacity: 0.4
                        }
                    }
                ]

>>>>>>> qmlFormsB
                MouseArea {
                    id: mouseArea
                    anchors.fill: parent
                    drag.target: parent
                    hoverEnabled: true
                    
                    onDoubleClicked: {
<<<<<<< HEAD
                        console.log("onDoubleClicked");
=======
>>>>>>> qmlFormsB
                        listView.clearSelectedItems(); // Must be before itemDoubleClicked: listView does not exist anymore afterwards
                        itemDoubleClicked(index);
                    }
                    
                    onClicked: {
<<<<<<< HEAD
                        console.log(variablesList.name + ": onClicked");
=======
                        console.log("MouseArea click");
>>>>>>> qmlFormsB
                        if (itemRectangle.clearOtherSelectedItemsWhenClicked) {
                            listView.clearSelectedItems();
                            listView.selectItem(itemRectangle, true);
                        }
                    }
                    
                    onPressed: {
<<<<<<< HEAD
                        console.log(variablesList.name + ": onPressed");
=======
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
                        console.log(variablesList.name + ": onReleased")
=======
>>>>>>> qmlFormsB
                        listView.mousePressed = false;
                    }
                    
                    drag.onActiveChanged: {
<<<<<<< HEAD
                        console.log(variablesList.name + ": drag.onActiveChanged: " + (drag.active ? "true" : "false"));
=======
>>>>>>> qmlFormsB
                        if (drag.active) {
                            if (itemRectangle.selected) {
                                itemRectangle.dragging = true;
                                for (var i = 0; i < listView.selectedItems.length; i++) {
                                    var selectedItem = listView.selectedItems[i];
<<<<<<< HEAD
                                    if (selectedItem.rank !== index) {
                                        selectedItem.dragging = true;
                                        console.log(variablesList.name + ": dragging set to true")
=======
                                    if (selectedItem.objectName !== "itemRectangle") {
                                        console.log("This is not an itemRectangle!")
                                        continue;
                                    }

                                    if (selectedItem.rank !== index) {
                                        selectedItem.dragging = true;
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
                                console.log(variablesList.name + ": dropped item " + selectedItem.rank);
                                selectedItem.dragging = false;
                                console.log(variablesList.name + ": dragging set to true " + selectedItem.rank);
=======
                                selectedItem.dragging = false;
>>>>>>> qmlFormsB
                                selectedItem.x = selectedItem.x; // break bindings
                                selectedItem.y = selectedItem.y;
                            }
                            if (itemRectangle.Drag.target) {
                                var dropTarget = itemRectangle.Drag.target.parent
                                if (dropTarget.singleItem && listView.selectedItems.length > 1)
<<<<<<< HEAD
                                    return;
                                
                                listView.clearSelectedItems(); // Must be before itemsDropped: listView does not exist anymore afterwards
                                var variablesListName = variablesList.name
                                itemsDropped(selectedIndexes, dropTarget);                               
                                console.log(variablesListName + ": items dropped");
=======
                                    return;                                
                                    
                                listView.clearSelectedItems(); // Must be before itemsDropped: listView does not exist anymore afterwards
                                var variablesListName = variablesList.name
                                itemsDropped(selectedIndexes, dropTarget, dropTarget.indexInDroppedListViewOfDraggedItem);                               
>>>>>>> qmlFormsB
                            }
                        }
                    }
                }
            }
<<<<<<< HEAD
        }
    }
=======
            
            Component.onDestruction: {
                if (variablesList.hasExtraControlColumns)
                    removeRowWithControls(colName.text);
            }
            
            Component.onCompleted: {
                if (variablesList.hasExtraControlColumns) {
                    var length = variablesList.controlColumns.length;
                    var previousColumn;
                    var controls = [];
                    for (var i = 0; i < length; i++) {
                        var newControl = components[i].createObject(itemRectangle, variablesList.controlColumns[i]);
                        newControl.isBound = false;
                        newControl.anchors.right = previousColumn ? previousColumn.left : itemRectangle.right;
                        newControl.anchors.top = itemRectangle.top;
                        newControl.height = parent.height;
                        if (!variablesList.controlColumns[i].width)
                            newControl.width = newControl.implicitWidth;
                        colName.width -= newControl.width;
                        controls.push(newControl);
                        previousColumn = newControl;
                    }
                    
                    addRowWithControls(colName.text, controls);
                }
            }
        }
    }    
>>>>>>> qmlFormsB
}
