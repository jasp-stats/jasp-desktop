import QtQuick  2.12
import JASP     1.0

QtObject
{
                    id:         dialogRoot
    property string title:      qsTr("")
    property string text:       qsTr("")

    signal          yes();
    signal          no();
    signal          cancel();

    function open()
    {
        switch(messages.showYesNoCancelQML(title, text, qsTr("Yes"), qsTr("No"), qsTr("Cancel")))
        {
        case MessageForwarder.Yes:      dialogRoot.yes();       break;
        case MessageForwarder.No:       dialogRoot.no();        break;
        case MessageForwarder.Cancel:   dialogRoot.cancel();    break;
        };
    }
}