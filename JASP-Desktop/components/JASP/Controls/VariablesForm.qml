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
			property int	nbOfAssignedVariablesList:		0
			property bool	debug:							false
			property int	marginBetweenVariablesLists:	8 * preferencesModel.uiScale
			property int    uiScale:						preferencesModel.uiScale


	Item { id: items }
	
	onUiScaleChanged: setup()
	
	Repeater
	{
		model: nbOfAssignedVariablesList
		AssignButton
		{
			leftSource: availableVariablesList
            rightSource: allAssignedVariablesList[index];
		}
	}
	
	Component.onCompleted: setup()
    
	function setup()
	{
        var titleHeight = Theme.variablesListTitle;

        var allJASPControls = []

        for (var i = 0; i < items.children.length; ++i) {
            var child = items.children[i];
			if (variablesForm.debug)
				child.debug = true
			if (child.name && (DEBUG_MODE || !child.debug)) {
                allJASPControls.push(child);
            }
        }

        var minHeight = 0;
        var changeableHeightControls = [];
		var firstRightControl = true;
		var availableVariablesListIndex = -1;
		
        for (i = 0; i < allJASPControls.length; i++) {
            var control = allJASPControls[i];
			var rightControl = true;
			if (control instanceof VariablesList)
			{
				if (control.listViewType === "AvailableVariables" || control.listViewType === "AvailableInteracton")
				{
					if (availableVariablesList)
						form.addError("Only 1 Available Variables list can be set in a VariablesForm");
					availableVariablesList = control;
					availableVariablesListIndex = i;
					rightControl = false;
				}
				else
					allAssignedVariablesList.push(control);
			}

			if (rightControl)
			{
				if (!firstRightControl)
					minHeight += marginBetweenVariablesLists;
				firstRightControl = false;
				if (control.singleVariable)
					minHeight += control.height;
				else if (control.height !== Theme.defaultListHeight)
					// If the height of this List item was changed, don't change it
					minHeight += control.height
				else {
					changeableHeightControls.push(control);
					if (control.title)
						minHeight += titleHeight;
				}
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
		
		var setWidth = allJASPControls.length > 0 ? availableVariablesList.width === allJASPControls[0].width : true
		
		availableVariablesList.parent = variablesForm
		availableVariablesList.width = setWidth ? variablesForm.listWidth : availableVariablesList.width;
		availableVariablesList.height	= variablesForm.height
		availableVariablesList.anchors.top = variablesForm.top
		availableVariablesList.anchors.left = variablesForm.left

        // Set the height of controls (that have not singleVariable set or where the height is already specifically set)
        // so that the AssignedVariablesList column is as long as the AvailableVariablesList column.
		// To Do: Should be a binding of some kind to avoid problems with resizing after changing preferencesModel.uiScale
        if (changeableHeightControls.length > 0) {
            var controlHeight = (availableVariablesList.height - minHeight) / changeableHeightControls.length;
            if (controlHeight < 25)
                controlHeight = 25; // Set a minimum height
            for (i = 0; i < changeableHeightControls.length; i++) {
                changeableHeightControls[i].height = changeableHeightControls[i].title ? (titleHeight + controlHeight) : controlHeight;
            }
        }

		var anchorTop = variablesForm.top;
        for (i = 0; i < allJASPControls.length; ++i) {
			allJASPControls[i].width = setWidth ? variablesForm.listWidth : allJASPControls[i].width
            allJASPControls[i].anchors.top = anchorTop;
            allJASPControls[i].anchors.topMargin = i === 0 ? 0 : marginBetweenVariablesLists;
            allJASPControls[i].anchors.right = variablesForm.right;
			anchorTop = allJASPControls[i].bottom;
        }

        for (i = 0; i < allAssignedVariablesList.length; i++) {
            var assignedList = allAssignedVariablesList[i];
            availableVariablesList.dropKeys.push(assignedList.name);
            assignedList.dropKeys.push(availableVariablesList.name);
            for (var j = 0; j < allAssignedVariablesList.length; ++j) {
                assignedList.dropKeys.push(allAssignedVariablesList[j].name);
            }
        }
		
		nbOfAssignedVariablesList = allAssignedVariablesList.length;
    }

}
