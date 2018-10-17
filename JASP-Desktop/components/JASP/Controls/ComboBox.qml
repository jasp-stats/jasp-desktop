import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: comboBox
    controlType: "ComboBox"
    implicitHeight: control.height
    implicitWidth: control.width + (label.visible ? labelSpacing + label.implicitWidth : 0)
    controlBackground: comboBoxBackground
    
    property int labelSpacing: 4
    
    property alias label: label
    property alias currentText: control.currentText
    property alias model: control.model
    property string modelType: Array.isArray(model) ? "array" : "keyvalue"
    property alias textRole: control.textRole
    property bool showVariableTypeIcon: false
    property var syncModels
    property alias currentIndex: control.currentIndex
    property alias control: control
    
    signal activated(int index);
    
    function resetWidth(value) {
        textMetrics.font = control.font
        textMetrics.text = value
        var newWidth = textMetrics.width + (comboBox.showVariableTypeIcon ? 20 : 4);
        console.log("New width for value " + value + ": " + newWidth);
        if (newWidth > control.modelWidth)
            control.modelWidth = newWidth;
    }
    
    Component.onCompleted: {
        control.activated.connect(activated);
    }
    
    RowLayout {
        spacing: label.visible ? labelSpacing : 0
        
        Label {
            id: label
            visible: label.text && comboBox.visible ? true : false
        }
        
        ComboBox {
            id:control
            focus: true
            spacing: 5
            height: Theme.comboBoxHeight
            property int modelWidth : 30
            implicitWidth: modelWidth + leftPadding + rightPadding + canvas.width
            textRole: "key"
            
            TextMetrics {
                id: textMetrics
            }

            onModelChanged: {
                console.log("model changed")
                var length = comboBox.modelType == "array" ? control.model.length : control.model.rowCount();
                for (var i = 0; i < length; i++) {
                    var value = comboBox.modelType === "array" ? model[i] : (comboBox.modelType === "variables" ? "" : control.model.get(i)[control.textRole]);
                    resetWidth(value);
                }
                
            }
            
            delegate: ItemDelegate {
                height: control.height
                contentItem: Rectangle {
                    anchors.fill: parent
                    Image {
                        id: contentIcon
                        height: 15; width: 15
                        anchors.verticalCenter: parent.verticalCenter
                        source: comboBox.showVariableTypeIcon ? model.type : ""
                        visible: comboBox.showVariableTypeIcon
                    }
                    
                    Text {
                        id: contentText
                        x: comboBox.showVariableTypeIcon ? 20 : 4                          
                        text: comboBox.modelType === "variables" ? model.name : (comboBox.modelType === "array" ? modelData : model[control.textRole])
                        font: control.font
                        elide: Text.ElideNone
                        verticalAlignment: Text.AlignVCenter
                    }
                }

                highlighted: control.highlightedIndex === index
            }
            
            
            indicator: Canvas {
                id: canvas
                x: control.width - width - control.spacing
                y: control.topPadding + (control.availableHeight - height) / 2
                width: 12
                height: 8
                contextType: "2d"
        
                Connections {
                    target: control
                    onPressedChanged: canvas.requestPaint()
                }
        
                onPaint: {
                    context.reset();
                    context.moveTo(0, 0);
                    context.lineTo(width, 0);
                    context.lineTo(width / 2, height);
                    context.closePath();
                    context.fillStyle = Theme.grayDarker;
                    context.fill();
                }
            }
        
            contentItem: Text {
                leftPadding: control.spacing
                rightPadding: control.indicator.width + control.spacing
        
                text: control.displayText
                font: control.font
                verticalAlignment: Text.AlignVCenter
                //elide: Text.ElideRight
                color: enabled ? Theme.black : Theme.grayDarker
            }
        
            background: Rectangle {
                id: comboBoxBackground
                implicitHeight: control.height
                border.color: Theme.borderColor
                border.width: 1
                radius: 2
                color: enabled ? Theme.controlBackgroundColor : Theme.disableControlBackgroundColor
            }
        
            popup: Popup {
                y: control.height - 1
                width: control.width
                implicitHeight: contentItem.implicitHeight
                padding: 1
        
                enter: Transition {
                        NumberAnimation { property: "opacity"; from: 0.0; to: 1.0 }
                    }
                contentItem: ListView {
                    clip: true
                    implicitHeight: contentHeight
                    model: control.popup.visible ? control.delegateModel : null
                    currentIndex: control.highlightedIndex        
                }
        
                background: Rectangle {
                    border.color: Theme.borderColor
                    radius: 2
                }
            }
        }
    }
}
