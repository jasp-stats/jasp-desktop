import QtQuick 2.9
import QtQuick.Controls 2.2

Button {
    id: buttonRef
    checkable: true
    property color color: Theme.black



    background: Item {
        //I dont like the standard checkboxes so here we go. Maybe we can make a nice JASP checkbox?

        Rectangle
        {
            anchors.top: parent.top
            anchors.bottom: parent.bottom
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.margins: 1

            width: height
            radius: width * 2

            color: "transparent"
            border.color: buttonRef.color
            border.width: 1

            Rectangle
            {
                anchors.margins: 3
                color: buttonRef.color
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.verticalCenter: parent.verticalCenter
                height: 2
            }

            Rectangle
            {
                anchors.margins: 3

                color: buttonRef.color
                anchors.top: parent.top
                anchors.bottom: parent.bottom
                anchors.horizontalCenter: parent.horizontalCenter
                width: 2
                visible: checked
            }

            MouseArea
            {
                anchors.fill: parent
                acceptedButtons: Qt.NoButton
                cursorShape: Qt.PointingHandCursor
            }

        }
    }


}
