import QtQuick			2.12
import QtQuick.Controls 6.0
import QtQuick.Layouts	1.0


Rectangle
{
	id:				rootDataset
	color:			jaspTheme.uiBackground

    SplitView
    {
		id:				splitViewData
		anchors.fill:	parent
		orientation:	Qt.Vertical
		handle:			Rectangle
		{
			implicitHeight:	jaspTheme.splitHandleWidth * 0.8;
			color:			SplitHandle.hovered || SplitHandle.pressed ? jaspTheme.grayLighter : jaspTheme.uiBackground

			Item
			{
				id:							threeDots
				width:						height * 4
				height:						jaspTheme.splitHandleWidth * 0.3
				anchors.centerIn:			parent
				property color	kleur:		jaspTheme.grayDarker

				Rectangle
				{
					color:		threeDots.kleur
					width:		height
					radius:		width

					anchors
					{
						top:	parent.top
						left:	parent.left
						bottom:	parent.bottom
					}
				}

				Rectangle
				{
					color:		threeDots.kleur
					width:		height
					radius:		width
					anchors
					{
						top:				parent.top
						bottom:				parent.bottom
						horizontalCenter:	parent.horizontalCenter
					}
				}

				Rectangle
				{
					color:		threeDots.kleur
					width:		height
					radius:		width

					anchors
					{
						top:	parent.top
						right:	parent.right
						bottom:	parent.bottom
					}
				}
			}
		}

		FilterWindow
		{
			id:							filterWindow
			objectName:					"filterWindow"
			Layout.minimumHeight:		desiredMinimumHeight
			Layout.preferredHeight:		rootDataset.height * 0.25
			Layout.maximumHeight:		rootDataset.height * 0.8

		}

		ComputeColumnWindow
		{
			id:							computeColumnWindow
			objectName:					"computeColumnWindow"
			Layout.minimumHeight:		desiredMinimumHeight
			Layout.preferredHeight:		rootDataset.height * 0.25
			Layout.maximumHeight:		rootDataset.height * 0.8
		}

        VariablesWindow
        {
			id:							variablesWindow
			Layout.minimumHeight:		calculatedMinimumHeight
			Layout.preferredHeight:		rootDataset.height * 0.25
			Layout.maximumHeight:		rootDataset.height * 0.8
        }

		DataTableView
		{
			objectName:				"dataSetTableView"
			Layout.fillHeight:		true
			onDoubleClicked:		mainWindow.startDataEditorHandler()
        }
	}
}
