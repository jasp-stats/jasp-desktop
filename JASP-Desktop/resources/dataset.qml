import QtQuick.Controls 1.4
import QtQuick 2.0
import JASP.DataSetTableModel 1.0

ApplicationWindow
{
    id: root
    DataSetTableModel
    {
        id: dataSetModel
    }

    TableView {
        id: dataSetTableView

        model: dataSetModel

        resources:
        {

            var roleList = dataSetModel.customRoleNames
            console.log("resources is called and rolelist: " + roleList);

            var temp = []
            for(var i=0; i<roleList.length; i++)
            {
                var role  = roleList[i]
                temp.push(columnComponent.createObject(dataSetTableView, { "role": role, "title": role}))
            }
            return temp
        }


        Component
        {
            id: columnComponent
            TableViewColumn{width: 220 }
        }
    }
}
