import QtQuick 2.0
import QtQuick.Layouts 1.11
import JASP.Controls 1.0

Item
{
    id: chi2TestTableView
    width: parent.width
    implicitWidth: width
    height: 200
    implicitHeight: height

    property alias name: tableView.name
    property alias source: tableView.source
    property alias tableView: tableView
	property var showAddButton: true
	property var showDeleteButton: true

    RowLayout
	{
        id: layout
        TableView
		{
            id: tableView
            implicitWidth: chi2TestTableView.width * 3/4 - layout.spacing
            implicitHeight: chi2TestTableView.height
            modelType: "MultinomialChi2Model"
            itemType: "double"
        }

        Group
		{
            implicitWidth: chi2TestTableView.width * 1/4
            implicitHeight: chi2TestTableView.height
            Button
			{
				visible: chi2TestTableView.showAddButton
                text: qsTr("Add Column") ; name: "addButton"
                control.width: chi2TestTableView.width * 1/4
                onClicked: tableView.addColumn()
                enabled: tableView.columnCount > 0
            }
            Button
			{
				visible: chi2TestTableView.showDeleteButton
                text: qsTr("Delete Column") ; name: "deleteButton"
                control.width: chi2TestTableView.width * 1/4
                onClicked: tableView.removeAColumn()
                enabled: tableView.columnCount > 1
            }
            Button
			{
                text: qsTr("Reset") ; name: "resetButton"
                control.width: chi2TestTableView.width * 1/4
                onClicked: tableView.reset()
                enabled: tableView.columnCount > 0
            }
        }
    }

}
