import QtQuick.Controls 1.4
import QtQuick 2.0

Rectangle {
    id: rootRectangle
    color: "red"

    TableView {
        TableViewColumn
        {
            id: haha
            width: 220
            title: "title"
            role: "title"
        }

        TableViewColumn
        {
            id: hahaha
            width: 220
            title: "author"
            role: "author"
        }


        id: dataSetTableView
        objectName: "dataSetTableView"
        anchors.top: rootRectangle.top
        anchors.left: rootRectangle.left
        anchors.right: rootRectangle.right
        anchors.bottom: rootRectangle.bottom
        //anchors.fill: parent
        //model: dataSetModel

        model: sourceModel

        ListModel {
                    id: sourceModel
                    ListElement {
                        title: "Moby-Dick"
                        author: "Herman Melville"
                    }
                    ListElement {
                        title: "The Adventures of Tom Sawyer"
                        author: "Mark Twain"
                    }
                    ListElement {
                        title: "Cat’s Cradle"
                        author: "Kurt Vonnegut"
                    }
                    ListElement {
                        title: "Farenheit 451"
                        author: "Ray Bradbury"
                    }
                    ListElement {
                        title: "It"
                        author: "Stephen King"
                    }
                    ListElement {
                        title: "On the Road"
                        author: "Jack Kerouac"
                    }
                    ListElement {
                        title: "Of Mice and Men"
                        author: "John Steinbeck"
                    }
        }

        function reloadColumns()
        {
            var roleList = dataSetModel.userRoleNames()
            console.log("reloadColumns is called and rolelist: " + roleList);

            var temp = []
            for(var i=0; i<roleList.length; i++)
            {
                var role  = roleList[i]
                temp.push(columnComponent.createObject(dataSetTableView, { "role": role, "title": role}))
                console.log("Column " + role + " added!")
            }

            resources = temp
        }

        Component
        {
            id: columnComponent
            TableViewColumn{width: 220 }
        }

    }
}
