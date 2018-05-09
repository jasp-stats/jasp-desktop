import QtQuick 2.10
import QtQuick.Controls 1.4
import QtQuick.Controls 2.2
import QtQuick.Layouts 1.0

Rectangle {
    SystemPalette { id: systemPalette; colorGroup: SystemPalette.Active }
    id: rootDataset
    color: systemPalette.window

    //Fancy pants curvy gradient for the columnheaders and rownumbers.




    ProgressBarHolder
    {
        id: progressBarHolder
        objectName: "progressBarHolder"
    }


    SplitView
    {
        id: splitViewData
        //anchors.fill: parent
        anchors.top: progressBarHolder.bottom
        anchors.left: rootDataset.left
        anchors.right: rootDataset.right
        anchors.bottom: rootDataset.bottom

        orientation: Qt.Vertical
        visible: false

        VariablesWindow
        {
            id: variablesWindow
			headersGradient: Gradient{
				GradientStop { position: 0.0;	color: "#EEEEEE" }
				GradientStop { position: 0.75;	color: "#EEEEEE" }
				GradientStop { position: 0.77;	color: "#DDDDDD" }
				GradientStop { position: 1.0;	color: "#DDDDDD" }
			}
        }

        FilterWindow
        {
            id: filterWindow
            objectName: "filterWindow"

			Layout.maximumHeight: rootDataset.height * 0.8
        }

		DataTableView
		{
			objectName: "dataSetTableView"
			Layout.fillHeight: true

			signal dataTableDoubleClicked()

			onDoubleClicked: dataTableDoubleClicked()
        }
	}
}
