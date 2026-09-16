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

import QtQuick
import QtQuick.Layouts
import JASP.Controls

/*!
    \qmltype VariablesForm
    \inqmlmodule JASP.Controls 1.0
    \brief A two-column layout with available variables on the left and assigned lists on the right.

	Creates a form where an AvailableVariablesList and one or more AssignedVariablesList are connected.
	It creates automatically the Arrow buttons for each AssignedVariablesList and sets their height
	to fill the form.

    \note VariablesForm does not bind directly to R options. The child
    VariablesList controls each have their own R binding.

    \section1 Properties

    \list
    \li \b listWidth (int) - Width of each variable list. Default: width * 2 / 5.
    \li \b removeInvisibles (bool) - Remove invisible controls from the layout. Default: false.
    \endlist

    \section1 Example

    \qml
    VariablesForm {
        AvailableVariablesList { name: "allVariables" }
        AssignedVariablesList { name: "dependent"; title: qsTr("Dependent Variable"); singleVariable: true }
        AssignedVariablesList { name: "fixedFactors"; title: qsTr("Fixed Factors"); allowedColumns: ["nominal"] }
    }
    \endqml
*/
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
	readonly property var	_activeAssignedListNames	: allAssignedVariablesList.filter((list) => list.visible && list.enabled).map((list) => list.name)
	readonly property var	_layoutControls				: allJASPControls.filter((control) => !removeInvisibles || control.visible)
	readonly property real	_changeableHeight			: _computeChangeableHeight(_layoutControls)
	readonly property var	_tabItems					: _computeTabItems()

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

	// Assigned lists that are invisible or disabled are removed from the drop keys:
	// the first drop key is the target of a double click, and should not be such a list.
	Binding
	{
		target:			variablesForm.availableVariablesList
		property:		"dropKeys"
		value:			variablesForm._activeAssignedListNames
		restoreMode:	Binding.RestoreNone
	}

	Instantiator
	{
		model: variablesForm.allAssignedVariablesList.length

		Binding
		{
			// The available list must stay the first key: it is the related list of the assigned list (see VariablesListBase::getRelatedModel)
			target:			variablesForm.allAssignedVariablesList[index]
			property:		"dropKeys"
			value:			[variablesForm.availableVariablesList.name].concat(variablesForm._activeAssignedListNames)
			restoreMode:	Binding.RestoreNone
		}
	}

	// The available list takes the height of the form, and the list width if its width is not set explicitly
	Binding
	{
		target:			variablesForm.availableVariablesList
		property:		"height"
		value:			variablesForm.height
		restoreMode:	Binding.RestoreNone
	}

	Binding
	{
		target:			variablesForm.availableVariablesList
		property:		"width"
		when:			variablesForm.widthSetByForm(variablesForm.availableVariablesList)
		value:			variablesForm.listWidth
		restoreMode:	Binding.RestoreNone
	}

	// The other controls are placed one below the other on the right side of the form:
	// the lists with a changeable height share the height left, so that this column is as long as the available list.
	Instantiator
	{
		model: variablesForm.allJASPControls.length

		Item
		{
			id: controlLayout

			readonly property var	control:			variablesForm.allJASPControls[index]
			readonly property int	position:			variablesForm._layoutControls.indexOf(control) // -1 when the control is removed from the layout
			readonly property bool	hasWidthSetByForm:	variablesForm.widthSetByForm(control)

			Binding
			{
				target:			controlLayout.control
				property:		"anchors.right"
				value:			variablesForm.right
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				target:			controlLayout.control
				property:		"anchors.top"
				value:			controlLayout.position > 0 ? variablesForm._layoutControls[controlLayout.position - 1].bottom : variablesForm.top
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				target:			controlLayout.control
				property:		"anchors.topMargin"
				value:			controlLayout.position > 0 ? variablesForm.marginBetweenVariablesLists : 0
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				// A list removed from the layout already gets the height it will have when it is back in the layout
				target:			controlLayout.control
				property:		"height"
				when:			variablesForm._hasChangeableHeight(controlLayout.control)
				value:			variablesForm._changeableHeightOf(controlLayout.control)
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				// Change the width of a list only if it was not set explicitly
				target:			controlLayout.control
				property:		"width"
				when:			controlLayout.hasWidthSetByForm && variablesForm._isList(controlLayout.control)
				value:			variablesForm.listWidth
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				target:			controlLayout.control
				property:		"setLabelAbove"
				when:			controlLayout.hasWidthSetByForm && controlLayout.control.controlType === JASPControl.ComboBox
				value:			true
				restoreMode:	Binding.RestoreNone
			}

			Binding
			{
				target:			controlLayout.control
				property:		"fieldWidth"
				when:			controlLayout.hasWidthSetByForm && controlLayout.control.controlType === JASPControl.ComboBox
				value:			variablesForm.listWidth
				restoreMode:	Binding.RestoreNone
			}
		}
	}

	// Tab order: the available list, the assign buttons and then the controls of the form, skipping the invisible or disabled ones.
	// When KeyNavigation.tab is null (no next control, or a control like a Group whose children take the focus), Qt's own Tab handling takes over.
	Binding
	{
		target:			variablesForm.availableVariablesList
		property:		"KeyNavigation.tab"
		value:			variablesForm._nextTabItem(variablesForm.availableVariablesList)
		restoreMode:	Binding.RestoreNone
	}

	Instantiator
	{
		model: variablesForm.allJASPControls.length

		Binding
		{
			target:			variablesForm._tabKeyItem(variablesForm.allJASPControls[index])
			property:		"KeyNavigation.tab"
			value:			variablesForm._nextTabItem(variablesForm.allJASPControls[index])
			restoreMode:	Binding.RestoreNone
		}
	}

	Repeater
	{
		id: assignButtonRepeater
		model: variablesForm.allAssignedVariablesList.length
		
		AssignButton
		{
			id:				assignButton
			x:				(allAssignedVariablesList[index].x + availableVariablesList.width - 40 * preferencesModel.uiScale) / 2
			y:				allAssignedVariablesList[index].y  + allAssignedVariablesList[index].rectangleY
			z:				10
			leftSource:		availableVariablesList
			rightSource:	allAssignedVariablesList[index]
			enabled:		allAssignedVariablesList[index].enabled
			KeyNavigation.tab:	variablesForm._nextTabItem(assignButton)

			Component.onCompleted:
			{
				allAssignedVariablesList[index]	.activeFocusChanged		.connect(setIconToLeft	);
				availableVariablesList			.activeFocusChanged		.connect(setIconToRight	);
			}
		}
	}
	
	function _isList(control)
	{
		return (control.controlType === JASPControl.VariablesListView) || (control.controlType === JASPControl.FactorLevelList) || (control.controlType === JASPControl.InputListView)
	}

	function _hasChangeableHeight(control)
	{
		// The height of a list is changeable if the list has more than one row and if its height is not set explicitly
		return _isList(control) && control.maxRows !== 1 && heightSetByForm(control)
	}

	// Height of the lists with a changeable height when these controls are placed in the column
	function _computeChangeableHeight(controls)
	{
		var count		= 0;
		var fixedHeight	= Math.max(controls.length - 1, 0) * marginBetweenVariablesLists;

		for (var control of controls)
		{
			if (!_hasChangeableHeight(control))
				fixedHeight += control.height;
			else
			{
				count++;
				if (control.title)
					fixedHeight += jaspTheme.variablesListTitle;
			}
		}

		return count > 0 ? Math.max((availableVariablesList.height - fixedHeight) / count, minimumHeightVariablesLists) : 0;
	}

	function _changeableHeightOf(control)
	{
		var height = _layoutControls.includes(control) ? _changeableHeight : _computeChangeableHeight(_layoutControls.concat(control));
		return control.title ? jaspTheme.variablesListTitle + height : height;
	}

	// Items reached with the Tab key, in this order: the available list, the assign buttons and the controls of the form
	function _computeTabItems()
	{
		var items = [availableVariablesList];

		for (var i = 0; i < assignButtonRepeater.count; i++)
			items.push(assignButtonRepeater.itemAt(i));

		for (var j = 0; j < allJASPControls.length; j++)
			items.push(allJASPControls[j]);

		return items;
	}

	// Next item that gets the focus with the Tab key: invisible or disabled items are skipped.
	// A control that does not take the focus itself (e.g. a Group) gives null, so that Qt moves the focus to its children.
	function _nextTabItem(item)
	{
		var position = _tabItems.indexOf(item);
		if (position < 0)
			return null;

		for (var i = position + 1; i < _tabItems.length; i++)
		{
			var next = _tabItems[i];
			if (next && next.visible && next.enabled)
				return next.activeFocusOnTab ? next : null;
		}

		return null;
	}

	// Item receiving the Tab key of a control: its inner control if this one takes the Tab focus itself (e.g. the ComboBox of a DropDown)
	function _tabKeyItem(control)
	{
		return control.innerControl && control.innerControl.activeFocusOnTab ? control.innerControl : control;
	}
}
