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

import QtQuick			2.12
import QtQuick.Layouts	1.12
import JASP				1.0

Item
{
	id					: variablesForm
	implicitHeight		: jaspTheme.defaultVariablesFormHeight
	implicitWidth		: form.width
	height				: implicitHeight
	width				: implicitWidth
	Layout.columnSpan	: parent.columns
	visible				: !debug || DEBUG_MODE

	default property alias	content:						items.children
			property int	listWidth:						width * 2 / 5
			property var	availableVariablesList
			property var	allAssignedVariablesList:		[]
			property var	allJASPControls:				[]
			property bool	debug:							false
			property int	marginBetweenVariablesLists:	8 * preferencesModel.uiScale
			property int	minimumHeightVariablesLists:	25 * preferencesModel.uiScale
			property bool	formInitialized:				false

			property double	_lastListWidth:					0
	
	property int preferredHeight:	implicitHeight
	property int preferredWidth:	implicitWidth

	Layout.preferredWidth:	preferredWidth
	Layout.preferredHeight:	preferredHeight

	Item { id: items }

	Connections
	{
		target:					preferencesModel
		onLanguageCodeChanged:
		{
			// Apparently a Qt bug: the height is not always recalculated by the GridLayout when the language is changed.
			// Force this by changing temporarily the Layout.preferredHeight
			// This fixes jasp-stats/jasp-test-release#731, but with Qt5.14.2, if preferredHeight is temporarly set to 0, it does not set it back to the original value afterwards.
			// So set it to slightly higher value, and then it works....
			variablesForm.Layout.preferredHeight = variablesForm.preferredHeight + .1
			variablesForm.Layout.preferredHeight = Qt.binding(function() { return variablesForm.preferredHeight; })
		}
	}

	onListWidthChanged: if (formInitialized && listWidth > 0 && listWidth != _lastListWidth) _lastListWidth = listWidth;

	onHeightChanged:	if (formInitialized )	setControlsSize();

	Repeater
	{
		id: assignButtonRepeater
		model: 0
		
		Loader
		{
			property var myLeftSource:			availableVariablesList
			property var myRightSource:			allAssignedVariablesList[index];
			property bool interactionAssign:	allAssignedVariablesList[index].addInteractionOptions
			z:	10
			
			
			sourceComponent: interactionAssign ? assignInteractionButtonComponent : assignButtonComponent
			x:		(allAssignedVariablesList[index].x + availableVariablesList.width - 40 * preferencesModel.uiScale) / 2
			y:      allAssignedVariablesList[index].y  + allAssignedVariablesList[index].rectangleY
		
			onLoaded:
			{
				allAssignedVariablesList[index]	.activeFocusChanged.connect(		item.setIconToLeft);
				availableVariablesList			.activeFocusChanged.connect(		item.setIconToRight);
				allAssignedVariablesList[index]	.selectedItemsChanged.connect(	item.setState);
				availableVariablesList			.selectedItemsChanged.connect(	item.setState);
				
				if (interactionAssign)
				{
					allAssignedVariablesList[index].interactionControl = item.interactionControl
					item.interactionControl.resetWidth(["Main Effects"])
					item.interactionControl.activated.connect(item.setState)
				}
			}
		}

	}
	
	Component
	{
		id: assignButtonComponent
		AssignButton 
		{
			z:				30
			leftSource:		myLeftSource
			rightSource:	myRightSource
		}		
	}
	
	Component
	{
		id: assignInteractionButtonComponent
		Item
		{
			z:	10

			property alias assignButton: assignButton
			property alias interactionControl: interactionControl
			
			function setIconToLeft()	{ assignButton.setIconToLeft() }
			function setIconToRight()	{ assignButton.setIconToRight() }
			function setState()			{ assignButton.setState() }

			
			AssignButton
			{
				id:					assignButton
				leftSource:			myLeftSource
				rightSource:		myRightSource
				interactionControl: interactionControl
				z:					3
			}
			
			DropDown
			{
				id:					interactionControl
				anchors.left:		assignButton.left
				anchors.leftMargin: (assignButton.width - width - 4) / 2
				anchors.top:		assignButton.bottom
				anchors.topMargin:	2
				currentIndex:		0
				values: ListModel 
				{
					ListElement { label: qsTr("Main Effects");	value: "MainEffects" }
					ListElement { label: qsTr("Only 2 way");		value: "All2Way" }
					ListElement { label: qsTr("Only 3 way");		value: "All3Way" }
					ListElement { label: qsTr("Only 4 way");		value: "All4Way" }
					ListElement { label: qsTr("Only 5 way");		value: "All5Way" }
					ListElement { label: qsTr("All");				value: "Cross" }
				}
			}			
		}
	}
	
	Component.onCompleted: setup()
	
	function setup()
	{
		allJASPControls = []
		allAssignedVariablesList = []
		availableVariablesList = null
		
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
				if (control.listViewType === JASP.AvailableVariables || control.listViewType === JASP.AvailableInteraction)
				{
					if (availableVariablesList)
						control.addControlError(qsTr("Only 1 Available Variables list can be set in a VariablesForm"));

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
			form.addFormError(qsTr("There is no Available List in the Variables Form"));
			return;
		}
		else
			allJASPControls.splice(availableVariablesListIndex, 1);
				
		availableVariablesList.parent		= variablesForm
		availableVariablesList.anchors.top	= variablesForm.top
		availableVariablesList.anchors.left = variablesForm.left

		
		var anchorTop = variablesForm.top;
        for (i = 0; i < allJASPControls.length; ++i) {
			allJASPControls[i].anchors.top			= anchorTop;
			allJASPControls[i].anchors.topMargin	= i === 0 ? 0 : marginBetweenVariablesLists;
			allJASPControls[i].anchors.right		= variablesForm.right;
			anchorTop								= allJASPControls[i].bottom;
        }
		
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (availableVariablesList.width === variablesForm.width)
			availableVariablesList.setWidthInForm = true
		
		for (i = 0; i < allJASPControls.length; i++)
		{
			control = allJASPControls[i];
			if ((control instanceof VariablesList) || (control instanceof RepeatedMeasuresFactorsList) || (control instanceof InputListView))
			{
				if (control.width === variablesForm.width)
					control.setWidthInForm = true

				if (control.height === jaspTheme.defaultVariablesFormHeight)
					control.setHeightInForm = true
			}
			else if (control instanceof ComboBox)
				control.heightChanged.connect(setControlsSize);

		}
		
        for (i = 0; i < allAssignedVariablesList.length; i++)
		{
			var assignedList = allAssignedVariablesList[i]
            availableVariablesList.dropKeys.push(assignedList.name);
			availableVariablesList.draggingChanged.connect(assignedList.setEnabledState);
            assignedList.dropKeys.push(availableVariablesList.name);

			for (var j = 0; j < allAssignedVariablesList.length; ++j)
			{
				assignedList.dropKeys.push(allAssignedVariablesList[j].name);
				if (assignedList != allAssignedVariablesList[j])
					assignedList.draggingChanged.connect(allAssignedVariablesList[j].setEnabledState);
			}
        }
		
		setControlsSize()
		
		assignButtonRepeater.model = allAssignedVariablesList.length;
		formInitialized = true
    }
	
	function setControlsSize()
	{
		availableVariablesList.height = Qt.binding(function() { return variablesForm.height; })
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (availableVariablesList.setWidthInForm) 
			availableVariablesList.width = Qt.binding(function() { return variablesForm.listWidth; })
		
		var firstControl				= true;
		var minHeightOfAssignedControls = 0;
		var	changeableHeightControls	= [];
		
		for (var i = 0; i < allJASPControls.length; ++i)
		{
			var control				= allJASPControls[i]
			var isControlList		= ((control instanceof VariablesList) || (control instanceof RepeatedMeasuresFactorsList) || (control instanceof InputListView))
			var isControlComboBox	= (control instanceof ComboBox)

			if (isControlList && control.setWidthInForm)
				// Change the width of the VariablesList only if was not set explicitely
				control.width = Qt.binding(function() {return variablesForm.listWidth; })
			else if (isControlComboBox && control.setWidthInForm)
			{
				control.setLabelAbove	= true
				control.controlMinWidth = variablesForm.listWidth
			}

			if (!firstControl)
				minHeightOfAssignedControls += marginBetweenVariablesLists;

			firstControl = false;

			if (!isControlList)
				minHeightOfAssignedControls += control.height;
			else if (control.maxRows === 1 || !control.setHeightInForm)
				minHeightOfAssignedControls += control.height;
			else
			{
				changeableHeightControls.push(control);
				if (control.title)
					minHeightOfAssignedControls += jaspTheme.variablesListTitle;
			}
		}
		
		// Set the height of controls (that have not singleVariable set or where the height is already specifically set)
        // so that the AssignedVariablesList column is as long as the AvailableVariablesList column.
		if (changeableHeightControls.length > 0)
		{
            var controlHeight = (availableVariablesList.height - minHeightOfAssignedControls) / changeableHeightControls.length;

			if (controlHeight < minimumHeightVariablesLists)
				controlHeight = minimumHeightVariablesLists; // Set a minimum height

			for (i = 0; i < changeableHeightControls.length; i++)
                changeableHeightControls[i].height = changeableHeightControls[i].title ? (jaspTheme.variablesListTitle + controlHeight) : controlHeight;
        }
		
	}

}
