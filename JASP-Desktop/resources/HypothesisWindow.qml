//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

import QtQuick 2.7
import QtQuick.Controls 2.2 as New
import QtQuick.Controls 1.4

import "SelectHypothesis"


FocusScope {
    id: hypothesisContainer
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

    Item {
        anchors.fill: parent

        HypothesisConstructor {
            anchors.bottom: applyEasyFilter.top
            anchors.right: parent.right
            anchors.left: parent.left
            anchors.top: parent.top

            id: easyFilterConstructor
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
                title: "Hypothesis Changed"
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
            text: showApplyNotApplied ? "Add Hypothesis" : "Added"
            disabled: !easyFilterConstructor.somethingChanged

            anchors.left: parent.left
            anchors.right: parent.right
            anchors.bottom: parent.bottom
            anchors.rightMargin: 5

            onClicked: easyFilterConstructor.checkAndApplyFilter()

            toolTip: showApplyNotApplied ? "Click to add hypothesis" : "Hypothesis is already added"
        }
    }
}
