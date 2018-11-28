import QtQuick 2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3
import JASP.Theme 1.0

JASPControl {
    id: textArea
    controlType: "TextArea"
    implicitHeight: Theme.defaultTextAreaHeight
    implicitWidth: parent.width
    controlBackground: control.background
    
    property alias control: control
    property alias text: control.text
    property string textType
    property string applyScriptInfo
    property alias infoText: infoText.text
    property bool hasScriptError: false
    property alias font: control.font
    property alias textDocument: control.textDocument
    
    signal applyRequest()
    
    
    Flickable {
        id: flickable
        anchors.fill: parent
        
    
        TextArea.flickable: TextArea {
            id: control
            selectByMouse: true
            
            wrapMode: TextArea.Wrap
            
            background: Rectangle {
                border.width: 1
                border.color: Theme.borderColor
                
                Text {
                    id: infoText
                    z:2
                    anchors.bottom: parent.bottom
                    anchors.right: parent.right
                    anchors.margins: 4
                    leftPadding: 5
                    rightPadding: 5
                    bottomPadding: 3
                    topPadding: 3
                    text: textArea.applyScriptInfo
                    horizontalAlignment: Text.AlignHCenter
                    verticalAlignment: Text.AlignVCenter
                    color: textArea.hasScriptError ? Theme.black : Theme.grayDarker
                }
                
                Rectangle {
                    anchors.fill: infoText
                    color: textArea.hasScriptError ? Theme.errorMessagesBackgroundColor : "transparent"
                }

            }
            
            Keys.onPressed: {
                if (event.modifiers & Qt.ControlModifier) {
                    if (event.key === Qt.Key_Return || event.key === Qt.Key_Enter)
                        applyRequest();
                } else if ( event.key === Qt.Key_Tab) {
                    control.insert(control.cursorPosition, "  ")
                    event.accepted = true;
                } else {
                    infoText.text = textArea.applyScriptInfo;
                    textArea.hasScriptError = false;
                }
            }
        }
    
        ScrollBar.vertical: ScrollBar { }
    }    
}
