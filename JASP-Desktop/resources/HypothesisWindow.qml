import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4

import "SelectHypothesis"


FocusScope {
    id: filterContainer
    visible: true

    property bool opened: false
    property int minimumHeightTextBoxes: 50
    property string lastAppliedFilter: defaultFilter
    property bool showEasyFilter: true


    // onShowEasyFilterChanged: if(!showEasyFilter) filterEdit.text = engineSync.getFilter()
    function toggle() {
        opened = !opened

        // filterEdit.text = engineSync.getFilter()
        if (opened)
            height = parent.height / 2
    }

    function open() {
        if (!opened)
            toggle()
    }

    function sendFilter() {
        engineSync.sendFilter(generatedFilter, lastAppliedFilter)
    }

    function applyAndSendFilter(newFilter) {
        lastAppliedFilter = newFilter

        sendFilter()
    }

    function resetFilter() {
        filterEdit.text = defaultFilter
        applyAndSendFilter(defaultFilter)
    }

    signal rCodeChanged(string rScript)

    Item {
        anchors.fill: parent

        // visible: true // parent.showEasyFilter
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

    Item {
        id: rFilterFields
        visible: !parent.showEasyFilter
        anchors.fill: parent
        property real desiredMinimumHeight: 20 // filterButtons.height + (filterErrorScroll.visible ? filterErrorScroll.height : 0 ) + filterEditRectangle.desiredMinimumHeight

        Rectangle {
            id: filterEditRectangle
            color: "white"

            border.width: 1
            border.color: "green"
            //Layout.fillHeight: true
            //Layout.minimumHeight: applyFilter.height + hypothesisWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight
            property real desiredMinimumHeight: applyFilter.height
                                                + hypothesisWindow.minimumHeightTextBoxes
                                                + filterGeneratedBox.contentHeight

            anchors.top: parent.top
            anchors.bottom: filterErrorScroll.top
            anchors.left: parent.left
            anchors.right: parent.right

            New.ScrollView {
                id: filterScroller
                anchors.fill: parent
                clip: true

                contentWidth: width
                contentHeight: filterGeneratedBox.height + filterEditBox.height

                Rectangle {
                    id: filterGeneratedBox
                    anchors.top: parent.top
                    anchors.left: parent.left
                    anchors.right: parent.right

                    height: filterGeneratedEdit.contentHeight
                    color: "transparent"
                    border.color: "lightGray"
                    border.width: 1

                    New.TextArea {
                        id: filterGeneratedEdit
                        anchors.top: filterGeneratedBox.top
                        anchors.left: resetAllGeneratedFilters.right
                        anchors.right: filterGeneratedBox.right
                        text: generatedFilter + "\n"
                        height: contentHeight
                        readOnly: true
                        color: "gray"
                        selectByMouse: true
                        onActiveFocusChanged: if (!activeFocus)
                                                  deselect()

                        font.family: "Courier"
                        wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere
                    }

                    FilterButton {
                        id: resetAllGeneratedFilters
                        anchors.left: parent.left
                        anchors.verticalCenter: parent.verticalCenter
                        width: height // dataSetModel.columnsFilteredCount > 0 ? height : 0
                        iconSource: "../images/eraser_all.png"
                        visible: true // dataSetModel.columnsFilteredCount > 0
                        anchors.margins: 1
                        onClicked: dataSetModel.resetAllFilters()
                        height: filterGeneratedBox.height
                        toolTip: "Reset all checkmarks on all labels"

                        //background: Rectangle {	color: "transparent" }
                    }
                }

                Item {
                    //Must be here because otherwise filterEdit turns its clipping on, because it is in a scrollview...
                    id: filterEditBox
                    anchors.top: filterGeneratedBox.bottom
                    anchors.left: parent.left
                    anchors.right: parent.right
                    anchors.rightMargin: 5
                    height: filterEdit.height

                    New.TextArea {

                        id: filterEdit

                        anchors.top: parent.top
                        anchors.left: parent.left
                        anchors.right: parent.right
                        height: contentHeight + 30
                        selectByMouse: true
                        onActiveFocusChanged: if (!activeFocus)
                                                  deselect()

                        property bool changedSinceLastApply: text
                                                             !== filterContainer.lastAppliedFilter

                        font.family: "Courier"
                        wrapMode: New.TextArea.WrapAtWordBoundaryOrAnywhere
                    }
                }
            }

            function askIfChanged(closeFunc) {
                if (filterEdit.changedSinceLastApply) {
                    saveDialog.closeFunc = closeFunc
                    saveDialog.open()
                } else
                    closeFunc()
            }

            New.Dialog {
                id: saveDialog

                x: (filterEditRectangle.width - width) / 2
                y: (filterEditRectangle.height - height) / 2

                modal: true
                title: "Filter Changed"
                property var closeFunc: undefined

                footer: New.DialogButtonBox {
                    New.Button {
                        text: qsTr("Save")
                        onClicked: {
                            hypothesisWindow.applyAndSendFilter(filterEdit.text)
                            saveDialog.closeFunc()
                            saveDialog.close()
                        }
                    }

                    New.Button {
                        text: qsTr("Cancel")

                        onClicked: {
                            saveDialog.close()
                        }
                    }
                    New.Button {
                        text: qsTr("Discard")

                        onClicked: {
                            saveDialog.closeFunc()
                            saveDialog.close()
                        }
                    }
                }

                contentItem: Text {
                    text: "There are unapplied changes to your hypothesis; what would you like to do?"
                    wrapMode: Text.WrapAtWordBoundaryOrAnywhere
                }
            }
        }

        New.ScrollView {
            id: filterErrorScroll
            height: hypothesisWindow.minimumHeightTextBoxes //filterError.contentHeight//Math.min(filterError.contentHeight, hypothesisWindow.minimumHeightTextBoxes)

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: filterButtons.top

            New.TextArea {
                id: filterError
                color: "red"
                readOnly: true
                text: filterErrorText + "\n"

                selectByMouse: true
                onActiveFocusChanged: if (!activeFocus)
                                          deselect()

                font.family: "Courier"

                states: [
                    State {
                        name: "closed"
                        PropertyChanges {
                            target: filterErrorScroll
                            visible: false
                            height: 0
                        }
                        when: filterErrorText.length === 0
                    },
                    State {
                        name: "opened"
                        PropertyChanges {
                            target: filterErrorScroll
                            visible: true
                            height: filterError.contentHeight
                        } //Math.min( , hypothesisWindow.minimumHeightTextBoxes)

                        when: filterErrorText.length > 0
                    }
                ]
            }
        }
    }
}
