import QtQuick  2.12
import JASP     1.0

QtObject
{
                    id:         dialogRoot
    property string title:      qsTr("")
    property string text:       qsTr("")

    signal          yes();
    signal          no();

    function open()
    {
        if (messages.showYesNoQML(title, text, qsTr("Yes"), qsTr("No")))
        {
            dialogRoot.yes();
        } else {
            dialogRoot.no();
        }
    }
}