import QtQuick.Controls 2.1
import QtQuick.Controls.Styles 1.4
import QtQuick.Controls 1.4
import QtQuick 2.7


Rectangle {
    id: rootDataset
    color: "red"

    Rectangle {
        id: variablesWindow
        color: "blue"
        height: 0
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right

        property bool opened: false

        states: [
            State {
                name: "closed"
                PropertyChanges { target: variablesWindow; height: 0 }
                when: !variablesWindow.opened
            },
            State {
                name: "opened"
                PropertyChanges { target: variablesWindow; height: 200 }
                when: variablesWindow.opened
            }
        ]

        transitions: Transition { NumberAnimation { properties: "height"; easing.type: Easing.InOutQuad } }
    }

    TableViewJasp {
        headerDelegate: Item
        {
            id: colHeader

            height: headerText.implicitHeight * 1.1
            width: headerText.implicitWidth * 1



            Image
            {
                id: colIcon
                anchors.top: colHeader.top
                anchors.bottom: colHeader.bottom
                anchors.left: colHeader.left
                source: dataSetModel.columnIcon(styleData.column)
                width: height


                MouseArea
                {
                    anchors.fill: parent

                    onPressed:
                    {
                        popup.open()
                    }
                }


                Popup {
                    id: popup
                    x: parent.x
                    y: parent.y + parent.height
                    width: 100
                    height: 100
                    modal: true
                    focus: true
                    closePolicy: Popup.CloseOnReleaseOutside | Popup.CloseOnPressOutside
                }
            }


            Text
            {
                id: headerText
                anchors.top: colHeader.top
                anchors.bottom: colHeader.bottom
                anchors.left: colIcon.right
                anchors.right: colHeader.right
                text: dataSetModel.columnTitle(styleData.column);
                leftPadding: 10

                MouseArea
                {
                    anchors.fill: parent
                    onClicked:
                    {
                        console.log("HeaderText pressed!")
                        variablesWindow.opened = !variablesWindow.opened
                    }
                }
            }

        }


        id: dataSetTableView
        objectName: "dataSetTableView"
        anchors.top: variablesWindow.bottom
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right
        anchors.bottom: rootDataset.bottom
        //anchors.fill: parent


        model: dataSetModel

        function reloadColumns()
        {
            var roleList = dataSetModel.userRoleNames();
            //var roleNums = dataSetModel.userRoles();

            console.log("reloadColumns is called and rolelist: " + roleList);

            var temp = []
            for(var i=0; i<roleList.length; i++)
            {
                dataSetTableView.addColumn(columnComponent.createObject(dataSetTableView, { "role": roleList[i], "title": roleList[i]}))
            }

            return temp
        }



        Component
        {
            id: columnComponent
            TableViewColumn
            {
                width: 220
                movable: false


            }
        }
    }
}
