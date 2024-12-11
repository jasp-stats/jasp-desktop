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
	implicitWidth				: jaspForm.width
	Layout.columnSpan			: (parent && parent.hasOwnProperty('columns')) ? parent.columns : 1
	isBound						: false
	shouldStealHover			: false
	marginBetweenVariablesLists	: 8 * preferencesModel.uiScale
	minimumHeightVariablesLists	: 25 * preferencesModel.uiScale
	preferredHeight				: implicitHeight
	preferredWidth				: implicitWidth
	Layout.preferredHeight		: preferredHeight // Cannot set Layout attached property in c++

	onActiveFocusChanged		:
	{
		if (activeFocus)
		{
			availableVariablesList.forceActiveFocus();
			availableVariablesList.KeyNavigation.backtab = variablesForm.nextItemInFocusChain(false);
		}
	}

	default property alias	content				: items.children
			property int	listWidth			: width * 2 / 5
			property alias	contentItems		: items
			property bool	removeInvisibles	: false

			property double	_lastListWidth		: 0
			property double _comboBoxHeight		: 0

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

	onHeightChanged:			if (initialized)	setControlsSize()
	onRemoveInvisiblesChanged:	if (initialized)	setControlsSize()

	Repeater
	{
		id: assignButtonRepeater
		model: 0
		
		AssignButton
		{
			x:				(allAssignedVariablesList[index].x + availableVariablesList.width - 40 * preferencesModel.uiScale) / 2
			y:				allAssignedVariablesList[index].y  + allAssignedVariablesList[index].rectangleY
			z:				10
			leftSource:		availableVariablesList
			rightSource:	allAssignedVariablesList[index]
			enabled:		allAssignedVariablesList[index].enabled

			Component.onCompleted:
			{
				allAssignedVariablesList[index]	.activeFocusChanged		.connect(setIconToLeft	);
				availableVariablesList			.activeFocusChanged		.connect(setIconToRight	);
			}
		}
	}
	
	function init()
	{
		for (var i in allJASPControls)
		{
			var control					= allJASPControls[i]
			control.anchors.right		= variablesForm.right;
			control.visibleChanged.connect(setControlsSize)

			var isControlList		= ((control.controlType === JASPControl.VariablesListView) || (control.controlType === JASPControl.FactorLevelList) || (control.controlType === JASPControl.InputListView))
			var isControlComboBox	= (control.controlType === JASPControl.ComboBox)

			if (isControlList && widthSetByForm(control))
				// Change the width of the VariablesList only if was not set explicitely
				control.width = Qt.binding(function() {return variablesForm.listWidth; })
			else if (isControlComboBox && widthSetByForm(control))
			{
				control.setLabelAbove	= true
				control.fieldWidth = Qt.binding(function() {return variablesForm.listWidth; })
			}
		}

		var countAssignedList = 0
		var availableDropKeys = []
		for (var key in allAssignedVariablesList)
		{
			countAssignedList++;
			var assignedList = allAssignedVariablesList[key]
			var assignedDropKeys = [];
			availableDropKeys.push(assignedList.name);
			assignedDropKeys.push(availableVariablesList.name);

			for (var key2 in allAssignedVariablesList)
				assignedDropKeys.push(allAssignedVariablesList[key2].name);

			assignedList.dropKeys = assignedDropKeys;
		}

		availableVariablesList.dropKeys = availableDropKeys
		setControlsSize()
		assignButtonRepeater.model = countAssignedList;
		setTabOrder();

		availableVariablesList.height = Qt.binding(function() { return variablesForm.height; })
		// Set the width of the VariablesList to listWidth only if it is not set explicitely
		// Implicitely, the width is set to the parent width.
		if (widthSetByForm(availableVariablesList))
			availableVariablesList.width = Qt.binding(function() { return variablesForm.listWidth; })

	}

	function setControlsSize()
	{
		var firstControl				= true;
		var minHeightOfAssignedControls = 0;
		var	changeableHeightControls	= [];
		var anchorTop					= variablesForm.top
		
		for (var key in allJASPControls)
		{
			var control				= allJASPControls[key]

			if (removeInvisibles && !control.visible)
				control.anchors.top			= variablesForm.top;
			else
			{
				control.anchors.top			= anchorTop;
				control.anchors.topMargin	= firstControl ? 0 : marginBetweenVariablesLists;
				anchorTop					= control.bottom;

				if (!firstControl)
					minHeightOfAssignedControls += marginBetweenVariablesLists;

				firstControl = false;

				var isControlList = ((control.controlType === JASPControl.VariablesListView) || (control.controlType === JASPControl.FactorLevelList) || (control.controlType === JASPControl.InputListView))

				if (!isControlList)
					minHeightOfAssignedControls += control.height;
				else if (control.maxRows === 1 || !heightSetByForm(control))
				{
					// console.log("Control " + control.name + " has height " + control.height)
					minHeightOfAssignedControls += control.height;
				}
				else
				{
					changeableHeightControls.push(control);
					if (control.title)
						minHeightOfAssignedControls += jaspTheme.variablesListTitle;
				}
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

	function setTabOrder()
	{
		availableVariablesList.KeyNavigation.tab = assignButtonRepeater.itemAt(0);
		for (var i = 0; i < allAssignedVariablesList.length - 1; i++)
			assignButtonRepeater.itemAt(i).KeyNavigation.tab = assignButtonRepeater.itemAt(i + 1);

		if(allAssignedVariablesList.length > 0)
			assignButtonRepeater.itemAt(allAssignedVariablesList.length - 1).KeyNavigation.tab = allAssignedVariablesList[0]

		for (var j = 0; j < allAssignedVariablesList.length - 1; j++)
			allAssignedVariablesList[j].KeyNavigation.tab = allAssignedVariablesList[j + 1];
	}
}
