import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQml.Models 2.2
import JASP.Theme 1.0

JASPControl
{
    id: variablesTable
    controlType: "VariablesTable"
    controlBackground: rectangle
    implicitWidth: parent.width
    implicitHeight: 200
    
    property var model
    property var columnNames: []
    property string variableName: "variable"
    property string title
    property var syncModels
    property var components: []
    property var columns: []
    
    signal removeRow(string name);
    signal addRow(string name, var columns);
    
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
        height: variablesTable.height - text.height
        width: parent.implicitWidth
        color: debug ? Theme.debugBackgroundColor : Theme.controlBackgroundColor
        border.width: 1
        border.color: Theme.borderColor
        
        ListView {
            id: gridView
            ScrollBar.vertical: ScrollBar {
                policy: ScrollBar.AsNeeded
            }
            anchors.fill: parent            
            anchors.margins: 4
            clip: true
            focus: true
            model: variablesTable.model
            
            delegate: FocusScope {
                id: itemWrapper
                height: itemRectangle.height
                width: gridView.width
                Rectangle {
                    id: itemRectangle
                    property int row: index
                    anchors.leftMargin: 5
                    anchors.horizontalCenter: parent.horizontalCenter
                    anchors.verticalCenter: parent.verticalCenter
                    height: firstColumn.implicitHeight + 6
                    width: parent.width
                    color: row % 2 ? Theme.rowOnevenColor : Theme.rowEvenColor 
                        
                    Text {
                        id: firstColumn
                        text: model.name
                        anchors.verticalCenter: parent.verticalCenter
                        x: 4
                        elide: Text.ElideRight
                        width: parent.width / (1 + columns.length)
                    }
    
                }
                
                Component.onDestruction: {
                    console.log("Destroy row " + firstColumn.text);
                    removeRow(firstColumn.text);
                }
                
                Component.onCompleted: {
                    var previousColumn = firstColumn;
                    var length = columns.length;
                    var controls = [];
                    for (var i = 0; i < length; i++) {
                        var newControl = components[i].createObject(itemRectangle, columns[i]);
                        newControl.isBound = false;
                        newControl.anchors.left = previousColumn.right
                        newControl.anchors.verticalCenter = itemRectangle.verticalCenter
                        newControl.width = parent.width / (1 + length)
                        controls.push(newControl)
                        previousColumn = newControl;
                    }
                    
                    console.log("Add row " + firstColumn.text);
                    addRow(firstColumn.text, controls);
                }
                
            }
    
        }        
    }
    
    Component.onCompleted: {
        var length = resources.length
        for (var i = 0; i < length; i++) {
            var column = resources[i];
            if (column instanceof VariablesTableColumn) {
                columns.push(column)
                var columnType = column.type
                var component = Qt.createComponent(columnType + ".qml");
                components.push(component);
            }
        }
    }
    
}
