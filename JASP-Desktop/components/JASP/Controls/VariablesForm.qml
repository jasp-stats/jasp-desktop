//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick 2.11
import JASP.Theme 1.0
import QtQuick.Layouts	1.3


Item
{
	id:				variablesForm
	implicitWidth:	form.width
	height:			Theme.defaultListHeight
    implicitHeight: height
	Layout.columnSpan: parent.columns
	visible:		!debug || DEBUG_MODE

	default property alias	content:						items.children
			property int	listWidth:						implicitWidth * 2 / 5
			property var	availableVariablesList
			property var	allAssignedVariablesList:		[]
			property var	allJASPControls:				[]
			property bool	debug:							false
			property int	marginBetweenVariablesLists:	8 * preferencesModel.uiScale
			property bool	formInitialized:				false
			property double uiScale:						preferencesModel.uiScale
			property double previousUiScale:				-1
	

	Item { id: items }
	
	onUiScaleChanged: {
		if (formInitialized)
		{
			if (previousUiScale !== uiScale)
			{
				previousUiScale = uiScale
				setControlsSize()
			}
		}
	}
	
	Repeater
	{
		id: assigneButtonRepeater
		model: 0
		AssignButton
		{
            id:             assignButton
            leftSource:     availableVariablesList
            rightSource:    allAssignedVariablesList[index];
            x:				(allAssignedVariablesList[index].x + availableVariablesList.width - 40 * preferencesModel.uiScale) / 2
            y:              allAssignedVariablesList[index].y + allAssignedVariablesList[index].rectangleY
			
			Component.onCompleted:
			{
				allAssignedVariablesList[index].activeFocusChanged.connect(assignButton.setIconToLeft);
				availableVariablesList.activeFocusChanged.connect(assignButton.setIconToRight);
				allAssignedVariablesList[index].hasSelectedItemsChanged.connect(assignButton.setState);
				availableVariablesList.hasSelectedItemsChanged.connect(assignButton.setState);
			}
		}
	}
	
	Component.onCompleted:
	{
		allJASPControls = []
		allAssignedVariablesList = []
		
        for (var i = 0; i < items.children.length; ++i) {
            var child = items.children[i];
			if (variablesForm.debug)
				child.debug = true
			if (child.name && (DEBUG_MODE || !child.debug)) {
                allJASPControls.push(child);
            }
        }

		var availableVariablesListIndex = -1;
		
        for (i = 0; i < allJASPControls.length; i++) {
            var control = allJASPControls[i];
			if (control instanceof VariablesList)
			{
				if (control.listViewType === "AvailableVariables" || control.listViewType === "AvailableInteraction")
				{
					if (availableVariablesList)
						form.addError("Only 1 Available Variables list can be set in a VariablesForm");
					availableVariablesList = control;
					availableVariablesListIndex = i;
				}
				else
					allAssignedVariablesList.push(control);
			}

            // Do not set the parent in the previous loop: this removes it from the items children.
            control.parent = variablesForm;
        }
		
		if (!availableVariablesList)
		{
			form.addError("There is no Available List in the VariablesForm");
			return;
		}
		else
			allJASPControls.splice(availableVariablesListIndex, 1);
				
		availableVariablesList.parent = variablesForm
		availableVariablesList.anchors.top = variablesForm.top
		availableVariablesList.anchors.left = variablesForm.left

		
		var anchorTop = variablesForm.top;
        for (i = 0; i < allJASPControls.length; ++i) {
            allJASPControls[i].anchors.top = anchorTop;
            allJASPControls[i].anchors.topMargin = i === 0 ? 0 : marginBetweenVariablesLists;
            allJASPControls[i].anchors.right = variablesForm.right;
			anchorTop = allJASPControls[i].bottom;
        }
		
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (availableVariablesList.width === variablesForm.width)
			availableVariablesList.setWidthInForm = true
		
		for (i = 0; i < allJASPControls.length; i++)
		{
			control = allJASPControls[i];
			if ((control instanceof VariablesList) || (control instanceof RepeatedMeasuresFactorsList))
			{
				if (control.width === variablesForm.width)
					control.setWidthInForm = true
				if (control.height === Theme.defaultListHeight)
					control.setHeightInForm = true
			}
		}
		
        for (i = 0; i < allAssignedVariablesList.length; i++)
		{
			var assignedList = allAssignedVariablesList[i]
            availableVariablesList.dropKeys.push(assignedList.name);
            assignedList.dropKeys.push(availableVariablesList.name);
            for (var j = 0; j < allAssignedVariablesList.length; ++j) {
                assignedList.dropKeys.push(allAssignedVariablesList[j].name);
            }			
        }
		
		setControlsSize()
		
		assigneButtonRepeater.model = allAssignedVariablesList.length;
		formInitialized = true
    }
	
	function setControlsSize()
	{
		availableVariablesList.height = variablesForm.height
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (availableVariablesList.setWidthInForm) 
			availableVariablesList.width = variablesForm.listWidth
		
		var firstControl = true;
		var minHeightOfAssignedControls = 0;
		var	changeableHeightControls = [];
		
		for (var i = 0; i < allJASPControls.length; ++i)
		{
			var control = allJASPControls[i]
			var isControlList = ((control instanceof VariablesList) || (control instanceof RepeatedMeasuresFactorsList))
			if (isControlList && control.setWidthInForm)
				// Change the width of the VariablesList only if was not set explicitely
				control.width = variablesForm.listWidth
			if (!firstControl)
				minHeightOfAssignedControls += marginBetweenVariablesLists;
			firstControl = false;
			if (!isControlList)
				minHeightOfAssignedControls += control.height;
			else if (control.singleVariable || !control.setHeightInForm)
				minHeightOfAssignedControls += control.height;
			else
			{
				changeableHeightControls.push(control);
				if (control.title)
					minHeightOfAssignedControls += Theme.variablesListTitle;
			}			
		}
		
		// Set the height of controls (that have not singleVariable set or where the height is already specifically set)
        // so that the AssignedVariablesList column is as long as the AvailableVariablesList column.
        if (changeableHeightControls.length > 0) {
            var controlHeight = (availableVariablesList.height - minHeightOfAssignedControls) / changeableHeightControls.length;
            if (controlHeight < 25)
                controlHeight = 25; // Set a minimum height
            for (i = 0; i < changeableHeightControls.length; i++) {
                changeableHeightControls[i].height = changeableHeightControls[i].title ? (Theme.variablesListTitle + controlHeight) : controlHeight;
            }
        }
		
	}

}
