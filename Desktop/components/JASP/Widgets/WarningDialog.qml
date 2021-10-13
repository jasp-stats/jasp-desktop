import QtQuick  2.12
import JASP     1.0

QtObject
{
                    id:         dialogRoot
    property string title:      qsTr("")
    property string text:       qsTr("")

    function open()
    {
        messages.showWarningQML(title, text);
    }
}
