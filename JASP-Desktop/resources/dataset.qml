import QtQuick.Controls 1.4
import QtQuick 2.0

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

      /*  TableViewColumn
        {
            width: 220
            role: "column_0"
            title: "column_0"
        }

        TableViewColumn
        {
            width: 220
            role: "column_1"
            title: "column_1"
        }
*/
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
                console.log("Column rol " + i+ " name: " + roleList[i] + " added!")
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
