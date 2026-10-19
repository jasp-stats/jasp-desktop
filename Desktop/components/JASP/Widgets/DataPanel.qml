import QtQuick.Controls
import QtQuick.Layouts
import QtQuick


Rectangle
{
	id:				rootDataset
	color:			jaspTheme.uiBackground

	property int leftHandSpace: 0 //Used to allow splithandler to move out of the screen on the left a bit.

	// FilterWindow and VariablesWindow are mutually exclusive: opening one closes the other
	// through its usual apply/discard route. The confirm dialogs are modal (blocking), so the
	// outcome is known as soon as requestClose() returns: if closing one was cancelled we also
	// abort the open of the other we just triggered, so the two are never both visible.
	Connections
	{
		target: filterModel
		function onFilterVisibleChanged()
		{
			if(!filterModel.filterVisible)
				return
			if(columnModel.visible && variablesWindow && !variablesWindow.requestClose())
				filterModel.filterVisible = false	// the user cancelled, so abort opening the FilterWindow
		}
	}

	Connections
	{
		target: columnModel
		function onVisibleChanged()
		{
			if(!columnModel.visible)
				return
			if(filterModel.filterVisible && filterLoader.item)
			{
				filterLoader.item.requestClose()
				if(filterModel.filterVisible)			// still open => the user cancelled
					columnModel.visible = false			// so abort opening the VariablesWindow
			}
		}
	}

    SplitView
    {
		id:					splitViewData
		anchors.fill:		parent
		anchors.leftMargin: rootDataset.leftHandSpace
		orientation:		Qt.Vertical
		
		Loader
		{
			id:							filterLoader
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
			objectName:					"dataSetTableView"
			SplitView.fillHeight:		true
			onDoubleClicked:			ribbonModel.showData()
			isMainDataViewer:			true
			SplitView.minimumHeight:	calculatedMinimumHeight
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
