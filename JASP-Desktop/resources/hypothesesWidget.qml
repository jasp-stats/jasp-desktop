import QtQuick 2.9


Rectangle {

    id: rootDataset
    color: systemPalette.window

    SystemPalette {
        id: systemPalette
        colorGroup: SystemPalette.Active
    }

    HypothesisWindow {
        id: hypothesisWindow
        objectName: "hypothesisWindow"
        anchors.fill: parent
    }
}
