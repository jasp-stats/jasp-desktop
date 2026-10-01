import QtQuick
import QtQuick.Controls
import QtWebEngine

ApplicationWindow {
    visible: true
    width: 420
    height: 320
    title: "AXRepro"

    WebEngineView {
        id: web
        anchors.fill: parent
        visible: false
        url: "qrc:/web.html"
    }

    Button {
        id: menuBtn
        objectName: "openMenuBtn"
        text: "Open menu"
        anchors.centerIn: parent
        onClicked: menu.popup()
    }

    Menu {
        id: menu
        objectName: "theMenu"
        MenuItem { text: "Item A" }
        MenuItem { text: "Item B" }
        MenuItem { text: "Item C" }
    }

    Button {
        text: "Plain"
        anchors.bottom: parent.bottom
        anchors.horizontalCenter: parent.horizontalCenter
    }
}
