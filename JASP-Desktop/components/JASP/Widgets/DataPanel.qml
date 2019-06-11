import QtQuick 2.9
import QtQuick.Controls 1.4
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.0
import JASP.Theme 1.0

Rectangle
{
	id:				rootDataset
	color:			Theme.uiBackground

	SplitView
    {
		id:				splitViewData
		anchors.fill:	parent

		orientation:	Qt.Vertical

		handleDelegate: Rectangle { color: Theme.uiBorder; }

        VariablesWindow
        {
			id:						variablesWindow
			Layout.minimumHeight:	calculatedMinimumHeight
        }

        FilterWindow
        {
			id:						filterWindow
			objectName:				"filterWindow"

			Layout.maximumHeight:	rootDataset.height * 0.8
        }

		ComputeColumnWindow
		{
			id:						computeColumnWindow
			objectName:				"computeColumnWindow"
			Layout.maximumHeight:	rootDataset.height * 0.8
		}

		DataTableView
		{
			objectName:			"dataSetTableView"
			Layout.fillHeight:	true
			onDoubleClicked:	mainWindow.startDataEditorHandler()
        }
	}
}
