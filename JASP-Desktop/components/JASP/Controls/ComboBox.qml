import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: comboBox
    controlType: "ComboBox"
    implicitHeight: control.height
    controlBackground: comboBoxBackground
    
    property int labelSpacing: 4
    
    property alias label: label
    property alias currentText: control.currentText
    property alias model: control.model
    property alias textRole: control.textRole
    property alias currentIndex: control.currentIndex
    property alias control: control
    
    signal activated(int index);
    
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
            implicitWidth: modelWidth + 2*leftPadding + 2*rightPadding + canvas.width + 2*spacing
            property bool isArrayModel: Array.isArray(model)
            textRole: isArrayModel ? "" : "key"
            
            TextMetrics {
                id: textMetrics
            }

            onModelChanged: {
                textMetrics.font = control.font
                var length = control.isListModel ? control.model.rowCount() : control.model.length
                for(var i = 0; i < length; i++) {
                    textMetrics.text = control.isListModel ? control.model.get(i)[control.textRole] : model[i]
                    modelWidth = Math.max(textMetrics.width, modelWidth)
                }
            }
            
            delegate: ItemDelegate {
                height: control.height
                contentItem: Text {
                    text: control.isArrayModel ? modelData : model[control.textRole] 
                    font: control.font
                    elide: Text.ElideRight
                    verticalAlignment: Text.AlignVCenter
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
                elide: Text.ElideRight
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
