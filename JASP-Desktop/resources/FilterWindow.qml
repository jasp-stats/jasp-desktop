import QtQuick 2.7
import QtQuick.Controls 2.3 as New
import QtQuick.Controls 1.4
import QtQuick.Layouts 1.3

FocusScope
{
    height: filterWindow.minimumHeightTextBoxes * 3
    Layout.minimumHeight: filterWindow.minimumHeightTextBoxes + applyFilter.implicitHeight + (filterError.visible ? filterError.contentHeight : 0 ) + filterGeneratedBox.height

    visible: opened

    property bool opened: false
    property int minimumHeightTextBoxes: 50
    property string lastAppliedFilter: defaultFilter
    readonly property string defaultFilter: "genFilter"


    function toggle()
    {
        opened = !opened
        filterEdit.text = engineSync.getFilter()
    }

    function open()
    {
        if(!opened)
            toggle();
    }

    function sendFilter()
    {
        engineSync.sendFilter(generatedFilter, lastAppliedFilter)
    }

    function applyAndSendFilter(newFilter)
    {
        lastAppliedFilter = newFilter
        sendFilter()
    }

    SplitView
    {
        anchors.fill: parent

        orientation: Qt.Vertical

        Rectangle {
            id: filterEditPlusButton
            color: systemPalette.base

            border.width: 1
            border.color: systemPalette.mid
            Layout.fillHeight: true
            Layout.minimumHeight: applyFilter.height + filterWindow.minimumHeightTextBoxes + filterGeneratedBox.contentHeight

            TextArea
            {
                id: filterGeneratedBox
                anchors.top: parent.top
                anchors.left: parent.left
                anchors.right: parent.right
                //anchors.bottom: filterEdit.top
                text: generatedFilter
                height: contentHeight
                readOnly: true
                textColor: "gray"
            }

            TextArea
            {

                id: filterEdit
                anchors.top: filterGeneratedBox.bottom
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.bottom: applyFilter.top
            }

            Button
            {
                id: applyFilter
                text: "Apply Filter"
                anchors.left: parent.left
                anchors.right: closeFilterButton.left
                anchors.bottom: parent.bottom

                onClicked: filterWindow.applyAndSendFilter(filterEdit.text)

            }

            Button
            {
                id: closeFilterButton
                iconSource: "../images/cross.png"
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                onClicked: filterWindow.toggle()
            }
        }

        TextArea
        {
            id: filterError
            textColor: "red"
            readOnly: true
            text: filterErrorText
            Layout.minimumHeight: filterWindow.minimumHeightTextBoxes//filterError.contentHeight//Math.min(filterError.contentHeight, filterWindow.minimumHeightTextBoxes)

            states: [
                State {
                    name: "closed"
                    PropertyChanges { target: filterError; visible: false; height: 0 }
                    when: filterError.text.length == 0
                },
                State {
                    name: "opened"
                    PropertyChanges { target: filterError; visible: true; height: filterError.contentHeight} //Math.min( , filterWindow.minimumHeightTextBoxes)

                    when: filterError.text.length > 0
                }
            ]
        }


    }

}
