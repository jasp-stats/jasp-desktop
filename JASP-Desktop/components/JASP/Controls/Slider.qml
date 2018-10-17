import QtQuick 2.11
import QtQuick.Controls 2.4
import JASP.Controls 1.0
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: slider
    controlType: "Slider"
    implicitHeight: control.height + (label.visible ? labelSpacing + label.implicitWidth : 0)
    implicitWidth: control.width 
    controlBackground: textField.controlBackground
    
    property int labelSpacing: 4
    
    property alias text: label.text
    property alias title: label.text
    property alias label: label
    property alias value: control.value
    property alias orientation: control.orientation
    property alias stepSize: control.stepSize
    property alias from: control.from
    property alias to: control.to
    property alias control: control
    property alias textField: textField
    property int decimals: 2
    property int power: Math.pow(10, decimals);
    
    signal moved();
    
    Component.onCompleted: {
        control.moved.connect(moved);
    }
    
    ColumnLayout {
        spacing: label.visible ? labelSpacing : 0
        
        Label {
            id: label
            visible: label.text && slider.visible ? true : false
        }
        
        Slider {
            id: control
            Layout.alignment: control.orientation === Qt.Vertical ? Qt.AlignHCenter : Qt.AlignLeft
            value: 0.5
            stepSize: 1 / slider.power
        
            background: Rectangle {
                
                id: sliderBackground
                x: control.leftPadding
                y: control.topPadding //+ control.availableHeight / 2 - height / 2
                implicitWidth: control.vertical ? Theme.sliderWidth : Theme.sliderLength
                implicitHeight: control.vertical ? Theme.sliderLength : Theme.sliderWidth
                width: control.vertical ? implicitWidth : control.availableWidth
                height: control.vertical ? control.availableHeight : implicitHeight
                radius: Theme.sliderWidth / 2
                color: Theme.sliderPartOn
        
                Rectangle {
                    width: control.vertical ? parent.width : control.visualPosition * parent.width
                    height: control.vertical ? control.visualPosition * parent.height : parent.height
                    color: Theme.sliderPartOff
                    radius: Theme.sliderWidth / 2
                }
            }
        
            handle: Rectangle {
                id: sliderHandle
                x: control.leftPadding + (control.vertical ? sliderBackground.radius - sliderHandle.radius : control.visualPosition * (control.availableWidth - width))
                y: control.topPadding + (control.vertical ? control.visualPosition * (control.availableHeight - height) : sliderBackground.radius - sliderHandle.radius)
                implicitWidth: Theme.sliderHandleDiameter
                implicitHeight: Theme.sliderHandleDiameter
                radius: Theme.sliderHandleDiameter / 2
                color: control.pressed ? Theme.itemSelectedColor : Theme.controlBackgroundColor
                border.color: Theme.borderColor
            }
            
            onMoved: {
                var intVal = Math.round(value * slider.power);
                var realVal = intVal / slider.power;
                
                if (textField.text != realVal) textField.text = realVal;
            }
            
            onValueChanged: {
                var intVal = Math.round(value * slider.power);
                var realVal = intVal / slider.power;
                value = realVal;
            }
                
        }
        
        TextField {
            id: textField
            text: control.value
            inputType: "number";
            isBound: false
            validator: DoubleValidator { bottom: control.from; top: control.to; decimals: slider.decimals }
            
            onEditingFinished: {
                if (control.value != text) {
                    control.value = value;
                    control.moved();
                }
            }
            onTextEdited: {
                if (text && !textField.control.acceptableInput)
                    text = control.value
            }
        }
    }    
}
