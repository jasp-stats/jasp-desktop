import QtQuick.Controls
import QtQuick.Layouts
import QtQuick


Rectangle
{
	id:				rootDataset

	Accessible.role:	Accessible.Pane
	Accessible.name:	qsTr("Data Panel")

	function focusDataTableView()
	{
		dataTableView.focusAndEdit();
	}

	color:			jaspTheme.uiBackground

	property int leftHandSpace: 0 //Used to allow splithandler to move out of the screen on the left a bit.

    SplitView
    {
		id:					splitViewData
		anchors.fill:		parent
		anchors.leftMargin: rootDataset.leftHandSpace
		orientation:		Qt.Vertical
		
		Loader
		{
			SplitView.minimumHeight:	preferencesModel.uiScale * 200
			SplitView.preferredHeight:	Screen.desktopAvailableHeight / 3
			SplitView.maximumHeight:	splitViewData.height
			
			sourceComponent:			filterComponent
			active:						filterModel.filterVisible
			visible:					filterModel.filterVisible
		}
		
		Component
		{
			id:	filterComponent
			
			FilterWindow
			{
				id:							filterWindow
				objectName:					"filterWindow"
	
			}
		}

		VariablesWindow
		{
			id:							variablesWindow
			SplitView.minimumHeight:	calculatedMinimumHeight
			SplitView.preferredHeight:	calculatedPreferredHeight
			SplitView.maximumHeight:	splitViewData.height
		}

		DataTableView
		{
			id:						dataTableView
			objectName:				"dataSetTableView"
			SplitView.fillHeight:	true
			onDoubleClicked:		ribbonModel.showData()
			isMainDataViewer:		true
        }

		handle: Rectangle
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
	}
}
