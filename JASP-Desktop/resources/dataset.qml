import QtQuick.Controls 1.4
import QtQuick.Controls.Styles 1.4
import QtQuick 2.7

Rectangle {
    id: rootRectangle
    color: "red"

    Rectangle {
        id: blauwRectangle
        color: "blue"
        height: 100
        anchors.left: rootRectangle.left
        anchors.right: rootRectangle.right
    }

    TableView {

        headerDelegate: Rectangle
        {
            Gradient {
                id: headerGradient
                GradientStop { position: 0.6; color: "#F0F0F0" }
                GradientStop { position: 0.7; color: "#E0E0E0" }
            }

            id: colHeader
            height: headerText.implicitHeight * 1.1
            width: headerText.implicitWidth * 1
            gradient: headerGradient

            Button
            {
                id: colIcon

                anchors.top: colHeader.top
                anchors.bottom: colHeader.bottom
                anchors.left: colHeader.left

                iconSource: dataSetModel.columnIcon(styleData.column)
                width: height
                style: ButtonStyle
                {
                    background: Rectangle { anchors.fill: parent; gradient: headerGradient}
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
            }
        }

        id: dataSetTableView
        objectName: "dataSetTableView"
        anchors.top: blauwRectangle.bottom
        anchors.left: rootRectangle.left
        anchors.right: rootRectangle.right
        anchors.bottom: rootRectangle.bottom
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
            }
        }

    }
}
