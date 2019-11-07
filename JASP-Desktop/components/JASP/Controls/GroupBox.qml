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
import QtQuick.Controls 2.4
import QtQuick.Layouts 1.3 as L


Rectangle {
	id:					control
	
	implicitWidth:		Math.max(label.realWidth, jaspTheme.groupContentPadding + contentArea.implicitWidth)
	implicitHeight:		label.realHeight + jaspTheme.titleBottomMargin + contentArea.implicitHeight	
	color:				jaspTheme.analysisBackgroundColor // transparent generates sometimes temporary black blocks
	L.Layout.leftMargin:	indent ? jaspTheme.indentationLength : 0
	visible:			!debug || DEBUG_MODE
    
	default property alias	content:			contentArea.children
			property alias	contentArea:		contentArea
			property int	rowSpacing:			jaspTheme.rowGroupSpacing
			property int	columnSpacing:		jaspTheme.columnGroupSpacing
			property int	columns:			1
			property string title:				""
			property bool	debug:				false
			property bool	indent:				false
			property bool	alignTextFields:	true
			property var	childControls:		[]
			property alias	alignChildrenTopLeft: contentArea.alignChildrenTopLeft
			property alias	label:				label

			property var	_allTextFields:		[]


	Label
	{
		id:				label
		anchors.top:	control.top
		anchors.left:	control.left
		text:			control.title
		color:			enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
		font:			jaspTheme.font
		visible:		control.title ? true : false
		
		property int	realHeight: visible ? implicitHeight : 0
		property int	realWidth: visible ? implicitWidth : 0
		
    }
    
	GridLayout
	{
		id:					contentArea
		columns:			control.columns
		anchors.top:		control.title ? label.bottom : control.top
		anchors.topMargin:	control.title ? jaspTheme.titleBottomMargin : 0
		anchors.left:		control.left
        anchors.leftMargin: control.title ? jaspTheme.groupContentPadding : 0
		rowSpacing:			control.rowSpacing
		columnSpacing:		control.columnSpacing
    }

	Connections
	{
		id: alignTextFieldSignal
		target: null
		enabled: false
		onXChanged: alignTextFieldTimer.restart()
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
		var i;
        form.getJASPControls(childControls, contentArea, false)
		for (i = 0; i < childControls.length; i++)
		{
            if (control.debug)
				childControls[i].setDebugState();
		}

		for (i = 0; i < contentArea.children.length; i++)
		{
			var child = contentArea.children[i];
			if (child.hasOwnProperty('controlType') && child.controlType === 'TextField')
				_allTextFields.push(child)
		}

		_alignTextField()
	}

	function _alignTextField()
	{
		if (alignTextFields && _allTextFields.length > 1)
		{
			var i;
			var xMax = 0;
			var longestControl;
			for (i = 0; i < _allTextFields.length; i++)
			{
				_allTextFields[i].controlXOffset = 0;
				if (xMax < _allTextFields[i].control.x)
				{
					longestControl = _allTextFields[i].control;
					xMax = _allTextFields[i].control.x;
				}
            }
            
			if (!alignTextFieldSignal.target)
				alignTextFieldSignal.target = longestControl;
			for (i = 0; i < _allTextFields.length; i++)
			{
				if (_allTextFields[i].control !== longestControl)
					// Cannot use binding here, since control.x depends on the controlXOffset,
					// that would generate a binding loop
					_allTextFields[i].controlXOffset = (xMax - _allTextFields[i].control.x);

			}
			alignTextFieldSignal.enabled = true;
		}
    }
}
