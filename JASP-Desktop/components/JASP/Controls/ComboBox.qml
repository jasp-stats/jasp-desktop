<<<<<<< HEAD
import QtQuick 2.10
import QtQuick.Controls 2.3
=======
import QtQuick 2.11
import QtQuick.Controls 2.4
>>>>>>> qmlFormsB
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: comboBox
    controlType: "ComboBox"
    implicitHeight: control.height
<<<<<<< HEAD
    backgroundRectangle: comboBoxBackground
=======
    implicitWidth: control.implicitWidth + (label.visible ? labelSpacing + label.implicitWidth : 0)
    width: implicitWidth
    controlBackground: comboBoxBackground
>>>>>>> qmlFormsB
    
    property int labelSpacing: 4
    
    property alias label: label
<<<<<<< HEAD
    property alias currentText: control.currentText
    property alias model: control.model
    property alias textRole: control.textRole
    property alias currentIndex: control.currentIndex
    property alias control: control
    
    signal activated(int index);
    
=======
    property string currentText
    property string currentIconPath
    property alias currentIndex: control.currentIndex
    property alias model: control.model
    property string textRole: "key"
    property string valueRole: "value"
    property bool showVariableTypeIcon: false
    property var syncModels
    property bool addEmptyValue: false
    property string emptyValue: qsTr("<no choice>")
    property alias control: control
    property bool initialized: false
    
    signal activated(int index);
    
    function resetWidth(value) {
        textMetrics.font = control.font
        textMetrics.text = value
        var newWidth = textMetrics.width + (comboBox.showVariableTypeIcon ? 20 : 4);
        if (newWidth > control.modelWidth) {
            control.modelWidth = newWidth;
            comboBox.width = comboBox.implicitWidth; // the width is not automatically updated by the implicitWidth...
        }
    }
    
>>>>>>> qmlFormsB
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
<<<<<<< HEAD
            property int modelWidth : 30
            implicitWidth: modelWidth + 2*leftPadding + 2*rightPadding + canvas.width + 2*spacing
            property bool isArrayModel: Array.isArray(model)
            textRole: isArrayModel ? "" : "key"
=======
            property int modelWidth: 30
            property bool isEmptyValue: comboBox.addEmptyValue && currentIndex <= 0
            implicitWidth: modelWidth + leftPadding + rightPadding + canvas.width
            textRole:comboBox.textRole            
>>>>>>> qmlFormsB
            
            TextMetrics {
                id: textMetrics
            }

<<<<<<< HEAD
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
=======
            delegate: ItemDelegate {
                height: control.height
                contentItem: Rectangle {
                    id: itemRectangle
                    anchors.fill: parent
                    property bool isEmptyValue: comboBox.addEmptyValue && index == 0
                    Image {
                        id: delegateIcon
                        x: 1
                        height: 15; width: 15
                        anchors.verticalCenter: parent.verticalCenter
                        source: (visible && comboBox.initialized) ? model.type : ""
                        visible: comboBox.showVariableTypeIcon && !itemRectangle.isEmptyValue
                    }
                    
                    Text {
                        x: delegateIcon.visible ? 20 : 4                          
                        text: comboBox.initialized ? model.name : ""
                        font: control.font
                        color: itemRectangle.isEmptyValue ? Theme.grayDarker : Theme.black
                        verticalAlignment: Text.AlignVCenter
                        anchors.horizontalCenter: itemRectangle.isEmptyValue ? parent.horizontalCenter : undefined
                    }
>>>>>>> qmlFormsB
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
        
<<<<<<< HEAD
            contentItem: Text {
                leftPadding: control.spacing
                rightPadding: control.indicator.width + control.spacing
        
                text: control.displayText
                font: control.font
                verticalAlignment: Text.AlignVCenter
                elide: Text.ElideRight
                color: enabled ? Theme.black : Theme.grayDarker
            }
        
=======
            contentItem: Item {
                Image {
                    id: contentIcon
                    height: 15; width: 15
                    x: 3
                    anchors.verticalCenter: parent.verticalCenter
                    source: comboBox.currentIconPath
                    visible: comboBox.showVariableTypeIcon && comboBox.currentIconPath && !control.isEmptyValue
                }
                
                Text {
                    x: contentIcon.visible ? 23 : 4
                    text: comboBox.currentText
                    font: control.font
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.horizontalCenter: control.isEmptyValue ? parent.horizontalCenter : undefined
                    color: (!enabled || control.isEmptyValue) ? Theme.grayDarker : Theme.black
                }
            }
                
>>>>>>> qmlFormsB
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
