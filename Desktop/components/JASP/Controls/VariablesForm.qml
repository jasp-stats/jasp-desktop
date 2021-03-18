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

VariablesFormBase
{
	id							: variablesForm
	implicitHeight				: jaspTheme.defaultVariablesFormHeight
	implicitWidth				: form.width
	Layout.columnSpan			: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1
	isBound						: false
	shouldStealHover			: false
	marginBetweenVariablesLists	: 8 * preferencesModel.uiScale
	minimumHeightVariablesLists	: 25 * preferencesModel.uiScale
	preferredHeight				: implicitHeight
	preferredWidth				: implicitWidth
	Layout.preferredHeight		: preferredHeight // Cannot set Layout attached property in c++

	default property alias	content				: items.children
			property int	listWidth			: width * 2 / 5
			property alias	contentItems		: items

			property double	_lastListWidth		: 0

	Item { id: items }

	Connections
	{
		target:					preferencesModel
		function onLanguageCodeChanged()
		{
			// Apparently a Qt bug: the height is not always recalculated by the GridLayout when the language is changed.
			// Force this by changing temporarily the Layout.preferredHeight
			// This fixes jasp-stats/jasp-test-release#731, but with Qt5.14.2, if preferredHeight is temporarly set to 0, it does not set it back to the original value afterwards.
			// So set it to slightly higher value, and then it works....
			variablesForm.Layout.preferredHeight = variablesForm.preferredHeight + .1
			variablesForm.Layout.preferredHeight = Qt.binding(function() { return variablesForm.preferredHeight; })
		}
	}

	onListWidthChanged: if (initialized && listWidth > 0 && listWidth != _lastListWidth) _lastListWidth = listWidth;

	onHeightChanged:	if (initialized )	setControlsSize();

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
				allAssignedVariablesList[index]	.activeFocusChanged		.connect(item.setIconToLeft	);
				availableVariablesList			.activeFocusChanged		.connect(item.setIconToRight);
				allAssignedVariablesList[index]	.selectedItemsChanged	.connect(item.setState		);
				availableVariablesList			.selectedItemsChanged	.connect(item.setState		);
				
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

	function init()
	{
		var first = true
		var anchorTop = variablesForm.top;
		for (var i in allJASPControls)
		{
			allJASPControls[i].anchors.top			= anchorTop;
			allJASPControls[i].anchors.topMargin	= first ? 0 : marginBetweenVariablesLists;
			allJASPControls[i].anchors.right		= variablesForm.right;
			anchorTop								= allJASPControls[i].bottom;
			first = false
		}

		var countAssignedList = 0
		for (var key in allAssignedVariablesList)
		{
			countAssignedList++;
			var assignedList = allAssignedVariablesList[key]
			availableVariablesList.dropKeys.push(assignedList.name);
			availableVariablesList.draggingChanged.connect(assignedList.setEnabledState);
			assignedList.dropKeys.push(availableVariablesList.name);

			for (var key2 in allAssignedVariablesList)
			{
				assignedList.dropKeys.push(allAssignedVariablesList[key2].name);
				if (assignedList !== allAssignedVariablesList[key2])
					assignedList.draggingChanged.connect(allAssignedVariablesList[key2].setEnabledState);
			}
		}

		setControlsSize()

		assignButtonRepeater.model = countAssignedList;
	}

	function setControlsSize()
	{
		availableVariablesList.height = Qt.binding(function() { return variablesForm.height; })
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (widthSetByForm(availableVariablesList))
			availableVariablesList.width = Qt.binding(function() { return variablesForm.listWidth; })
		
		var firstControl				= true;
		var minHeightOfAssignedControls = 0;
		var	changeableHeightControls	= [];
		
		for (var key in allJASPControls)
		{
			var control				= allJASPControls[key]
			var isControlList		= ((control instanceof VariablesList) || (control instanceof FactorLevelList) || (control instanceof InputListView))
			var isControlComboBox	= (control instanceof ComboBox)

			if (isControlList && widthSetByForm(control))
				// Change the width of the VariablesList only if was not set explicitely
				control.width = Qt.binding(function() {return variablesForm.listWidth; })
			else if (isControlComboBox && widthSetByForm(control))
			{
				control.setLabelAbove	= true
				control.controlMinWidth = Qt.binding(function() {return variablesForm.listWidth; })
			}

			if (!firstControl)
				minHeightOfAssignedControls += marginBetweenVariablesLists;

			firstControl = false;

			if (!isControlList)
				minHeightOfAssignedControls += control.height;
			else if (control.maxRows === 1 || !heightSetByForm(control))
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

			for (var i = 0; i < changeableHeightControls.length; i++)
                changeableHeightControls[i].height = changeableHeightControls[i].title ? (jaspTheme.variablesListTitle + controlHeight) : controlHeight;
        }
		
	}

}
