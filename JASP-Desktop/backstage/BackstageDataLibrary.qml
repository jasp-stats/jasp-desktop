import QtQuick 2.9
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.3

Rectangle
{
    id:rect
    width:800
    height:800
    objectName: "rect"
    color: "#ececec"

    Label
    {
        id:headlabel
        width:400
        height:30
        anchors.top: parent.top
        text: "  Data Library"
        font.family: "SansSerif"
        font.pixelSize: 18
        color: "black"
    }

    ToolSeparator
    {
        id: firstseparator
        anchors.top: headlabel.bottom
        width: rect.width
        orientation: Qt.Horizontal
    }

    DataLibraryBreadCrumbs
    {
        id:datalibrarybreadcrumbs
        width: rect.width
        height:40
        anchors.top: firstseparator.bottom
        anchors.left: parent.left
        anchors.right: parent.right
    }

    ToolSeparator
    {
        id: secondseparator
        anchors.top: datalibrarybreadcrumbs.bottom
        width: rect.width
        orientation: Qt.Horizontal
    }

    DataLibraryList
    {
        id: datalibrarylist
        width: rect.width
        anchors.top: secondseparator.bottom
        anchors.left: parent.left
        anchors.right: parent.right
    }
}

