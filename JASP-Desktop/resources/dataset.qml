//import QtQuick.Controls 2.3
//import QtQuick.Controls.Styles 1.4
import QtQuick.Controls 1.4
import QtQuick 2.7
import QtQuick.Layouts 1.0

Rectangle {
    SystemPalette { id: systemPalette; colorGroup: SystemPalette.Active }
    id: rootDataset
    color: systemPalette.window

    //Fancy pants curvy gradient for the columnheaders and rownumbers.
    Gradient{
        id: myHeadersGradient
        GradientStop { position: 0.2; color: systemPalette.midlight }
        GradientStop { position: 0.5; color: systemPalette.light }
        GradientStop { position: 0.9; color: systemPalette.midlight }
        GradientStop { position: 1.0; color: systemPalette.mid }
    }


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
            headersGradient: myHeadersGradient
        }

        FilterWindow
        {
            id: filterWindow
            objectName: "filterWindow"
        }

        DataTableView
        {
            id: dataTableView
            Layout.fillHeight: true
            headersGradient: myHeadersGradient
        }

	}



}
