import QtQuick 2.11
import QtQuick.Controls 2.4
import JASPTheme 1.0


JASPControl {
    controlType: "ComboBox"
    implicitHeight: control.height
    property alias currentText: control.currentText
    property alias model: control.model
    property alias control: control
    signal activated();
    
    Component.onCompleted: {
        control.activated.connect(activated);
    }
    
    ComboBox {
        id: control
        height: Theme.comboBoxHeight
        focus: true
        width: parent.width
        background: backgroundRectangle

    }
}
