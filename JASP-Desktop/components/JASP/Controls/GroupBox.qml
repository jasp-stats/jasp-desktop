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
	id:					groupBox
	implicitWidth:		Math.max(label.realWidth, jaspTheme.groupContentPadding + contentArea.implicitWidth)
	implicitHeight:		label.realHeight + jaspTheme.titleBottomMargin + contentArea.implicitHeight	
	L.Layout.leftMargin:	indent ? jaspTheme.indentationLength : 0
	controlType				: JASPControlBase.GroupBox
	isBound					: false
	childControlsArea		: contentArea
    
	default property alias	content:			contentArea.children
			property int	rowSpacing:			jaspTheme.rowGroupSpacing
			property int	columnSpacing:		jaspTheme.columnGroupSpacing
			property int	columns:			1
			property bool	indent:				false
			property bool	alignTextFields:	true
			property alias	label:				label

			property var	_allTextFields:		[]

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
		property int	realWidth: visible ? implicitWidth : 0
		
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
    }

	Connections
	{
		target:				preferencesModel
		onUiScaleChanged:	alignTextFieldTimer.restart()
	}

	Connections
	{
		target:					preferencesModel
		onLanguageCodeChanged:	alignTextFieldTimer.restart()
	}

	Timer
	{
		// The alignment should be done when the scaling of the TextField's are done
		id: alignTextFieldTimer
		interval: 50
		onTriggered: _alignTextField()
	}
    
	Component.onCompleted:
	{
		for (var i = 0; i < contentArea.children.length; i++)
		{
			var child = contentArea.children[i];
			if (child.hasOwnProperty('controlType') && child.controlType === JASPControlBase.TextField)
			{
				child.visibleChanged.connect(_alignTextField);
				_allTextFields.push(child)
			}
		}

		alignTextFieldTimer.start()
	}

	function _alignTextField()
	{
		if (alignTextFields && _allTextFields.length > 1)
		{
			var i;
			_allTextFields[0].controlXOffset = 0;
			var xMax = _allTextFields[0].innerControl.x;
			var longestControl = _allTextFields[0].innerControl;
			for (i = 1; i < _allTextFields.length; i++)
			{
				_allTextFields[i].controlXOffset = 0;
				if (xMax < _allTextFields[i].innerControl.x)
				{
					longestControl = _allTextFields[i].innerControl;
					xMax = _allTextFields[i].innerControl.x;
				}
            }
            
			for (i = 0; i < _allTextFields.length; i++)
			{
				if (_allTextFields[i].innerControl !== longestControl)
					// Cannot use binding here, since innerControl.x depends on the controlXOffset,
					// that would generate a binding loop
					_allTextFields[i].controlXOffset = (xMax - _allTextFields[i].innerControl.x);

			}
		}
    }
}
