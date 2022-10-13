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

import QtQuick			2.11
import QtQuick.Controls 2.4
import QtQuick.Layouts	1.3 as L
import JASP				1.0


JASPControl
{
	id						: groupBox
	implicitWidth			: Math.max(label.realWidth, jaspTheme.groupContentPadding + contentArea.implicitWidth)
	implicitHeight			: label.realHeight + jaspTheme.titleBottomMargin + contentArea.implicitHeight
	L.Layout.leftMargin		: indent ? jaspTheme.indentationLength : 0
	controlType				: JASPControl.GroupBox
	isBound					: false
	childControlsArea		: contentArea
	focusOnTab				: false

	default property alias	content:			contentArea.children
			property int	rowSpacing:			jaspTheme.rowGroupSpacing
			property int	columnSpacing:		jaspTheme.columnGroupSpacing
			property int	columns:			1
			property bool	indent:				false
			property bool	alignTextFields:	true
			property alias	label:				label

			property var	_allTextFields:		[]
			property bool	_childrenConnected:	false

	Label
	{
		id:				label
		anchors.top:	groupBox.top
		anchors.left:	groupBox.left
		text:			groupBox.title
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		font:			jaspTheme.font
		visible:		groupBox.title ? true : false
		
		property int	realHeight: visible ? implicitHeight : 0
		property int	realWidth:  visible ? implicitWidth  : 0
		
    }
    
	GridLayout
	{
		id:					contentArea
		columns:			groupBox.columns
		anchors.top:		groupBox.title ? label.bottom : groupBox.top
		anchors.topMargin:	groupBox.title ? jaspTheme.titleBottomMargin : 0
		anchors.left:		groupBox.left
		anchors.leftMargin: groupBox.title ? jaspTheme.groupContentPadding : 0
		rowSpacing:			groupBox.rowSpacing
		columnSpacing:		groupBox.columnSpacing

		checkFormOverflowWhenLanguageChanged: false // the checkFormOverflow is done with the alignment.
    }

	Connections
	{
		target:	preferencesModel
		function onUiScaleChanged(scale)	{ alignTextFieldTimer.restart(); }
	}

	Connections
	{
		target:	preferencesModel
		function onLanguageCodeChanged()	{ checkFormOverflowAndAlignTimer.restart(); }
	}

	Timer
	{
		// The alignment should be done when the scaling of the TextField's are done
		id: alignTextFieldTimer
		interval: 50
		onTriggered: _alignTextFields()
	}

	Timer
	{
		id: checkFormOverflowAndAlignTimer
		interval: 50
		onTriggered: _checkFormOverflowAndAlign()
	}

	Component.onCompleted:
	{
		for (var i = 0; i < contentArea.children.length; i++)
		{
			var child = contentArea.children[i];
			if (child.hasOwnProperty('controlType') && child.controlType === JASPControl.TextField)
				_allTextFields.push(child)
		}

		checkFormOverflowAndAlignTimer.start()
	}

	function _alignTextFields()
	{
		if (!alignTextFields || _allTextFields.length < 1) return;

		var allTextFieldsPerColumn = {}
		var columns = [];
		var i, j, textField;

		for (i = 0; i < _allTextFields.length; i++)
		{
			textField = _allTextFields[i];
			if (!_childrenConnected)
				// Do not connect visible when component is just completed: the visible value is aparently not yet set for all children.
				// So do it with the first time it is aligned.
				textField.visibleChanged.connect(_alignTextFields);

			if (textField.visible)
			{
				if (!allTextFieldsPerColumn.hasOwnProperty(textField.x))
				{
					// Cannot use Layout.column to know in which column is the textField.
					// Then its x value is used.
					allTextFieldsPerColumn[textField.x] = []
					columns.push(textField.x)
				}
				allTextFieldsPerColumn[textField.x].push(textField)
			}
		}

		for (i = 0; i < columns.length; i++)
		{
			var textFields = allTextFieldsPerColumn[columns[i]]

			// To align all the textfields on one column:
			// . First search for the Textfield with the longest label (its innerControl x position).
			// . Then add an offset (the controlXOffset) to all other textfields so that they are aligned with the longest TextField
			if (textFields.length >= 1)
			{
				textField = textFields[0];
				textField.controlXOffset = 0;
				var xMax = textField.innerControl.x;
				var longestControl = textField.innerControl;

				for (j = 1; j < textFields.length; j++)
				{
					textField = textFields[j];
					textField.controlXOffset = 0;
					if (xMax < textField.innerControl.x)
					{
						longestControl = textField.innerControl;
						xMax = textField.innerControl.x;
					}
				}

				for (j = 0; j < textFields.length; j++)
				{
					textField = textFields[j];
					if (textField.innerControl !== longestControl)
						// Cannot use binding here, since innerControl.x depends on the controlXOffset,
						// that would generate a binding loop
						textField.controlXOffset = (xMax - textField.innerControl.x);

				}
			}
		}

		_childrenConnected = true;
    }

	function _checkFormOverflowAndAlign()
	{
		_alignTextFields();
		if (contentArea.checkFormOverflow())
			_alignTextFields();
	}
}
