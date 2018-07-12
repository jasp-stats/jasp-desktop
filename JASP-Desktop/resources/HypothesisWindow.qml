import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4

import "SelectHypothesis"


FocusScope {
    id: filterContainer
    visible: true

    property bool opened: false
    property int minimumHeightTextBoxes: 50
    property bool showEasyFilter: true


    // onShowEasyFilterChanged: if(!showEasyFilter) filterEdit.text = analysisObject.getFilter()
    function toggle() {
        opened = !opened

        // filterEdit.text = analysisObject.getFilter()
        if (opened)
            height = parent.height / 2
    }

    function open() {
        if (!opened)
            toggle()
    }

    signal rCodeChanged(string rScript)

    Item {
        anchors.fill: parent

        HypothesisConstructor {
            anchors.bottom: helpEasyFilterButton.top
            anchors.right: parent.right
            anchors.left: parent.left
            anchors.top: parent.top

            id: easyFilterConstructor

            onRCodeChanged: filterContainer.rCodeChanged(rScript)
            clip: true

            function askIfChanged(closeFunc) {
                if (jsonChanged() || !lastCheckPassed) {
                    easySaveDialog.closeFunc = closeFunc
                    easySaveDialog.open()
                } else
                    closeFunc()
            }

            New.Dialog {
                id: easySaveDialog

                x: (easyFilterConstructor.width - width) / 2
                y: (easyFilterConstructor.height - height) / 2

                modal: true
                title: "Filter Changed"
                property var closeFunc: undefined

                footer: New.DialogButtonBox {
                    New.Button {
                        text: qsTr("Save")
                        onClicked: {
                            if (easyFilterConstructor.checkAndApplyFilter())
                                easySaveDialog.closeFunc()
                            easySaveDialog.close()
                        }
                    }

                    New.Button {
                        text: qsTr("Cancel")

                        onClicked: {
                            easySaveDialog.close()
                        }
                    }
                    New.Button {
                        text: qsTr("Discard")

                        onClicked: {
                            easySaveDialog.closeFunc()
                            easySaveDialog.close()
                        }
                    }
                }

                contentItem: Text {
                    text: "There are unapplied changes to your hypothesis; what would you like to do?"
                    wrapMode: Text.WrapAtWordBoundaryOrAnywhere
                }
            }
        }

        FilterButton {
            id: applyEasyFilter
            property bool showApplyNotApplied: easyFilterConstructor.somethingChanged
                                               || easyFilterConstructor.showStartupMsg
            text: showApplyNotApplied ? "Add restricted Hypothesis" : "Added"
            disabled: !easyFilterConstructor.somethingChanged

            anchors.left: parent.left
            anchors.right: helpEasyFilterButton.left
            anchors.bottom: parent.bottom
            anchors.top: helpEasyFilterButton.top
            anchors.rightMargin: 5

            onClicked: easyFilterConstructor.checkAndApplyFilter()

            toolTip: showApplyNotApplied ? "Click to add hypothesis" : "Hypothesis is already added"
        }

        FilterButton {
            id: helpEasyFilterButton
            iconSource: "qrc:/icons/QuestionMark.png"
            anchors.right: parent.right
            anchors.bottom: parent.bottom

            onClicked: mainWindow.showHelpFromQML("other/EasyFilterConstructor")
            toolTip: "Open Documentation"
        }
    }
}
