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

import QtQuick				2.11
import JASP.Theme			1.0
import QtQuick.Controls		2.4

FocusScope
{
	id: jaspControl

	property string	controlType:			"JASPControl"
	property string name:					""
	property bool	hasTabFocus:			true
	property bool	isBound:				true
	property bool	debug:					false
	property var	background:				null
	property var	focusIndicator:			background
	property bool   indent:                 false
	property alias	cursorShape:			controlMouseArea.cursorShape
	property alias	hovered:				controlMouseArea.containsMouse
	property bool	useControlMouseArea:	true
	property var	childControlsArea:		null
	property var	childControls:			[]
	property bool	childControlHasFocus:	false
	
	function setDebugState()
	{
		debug = true
		if (typeof(DEBUG_MODE) === "undefined" || !DEBUG_MODE)
			visible = false;
		else if (background)
			background.color = Theme.debugBackgroundColor
		
		for (var i = 0; i < childControls.length; i++)
			childControls[i].setDebugState();
	}
	
	Component.onCompleted:
	{
		if (typeof(control) !== "undefined")
		{
			if (!background)
				background = control.background
		}
		
		if (childControlsArea && childControlsArea.children.length > 0)
		{
			getJASPControls(childControls, childControlsArea, false)
			if (controlType != "Expander")
				childControlHasFocus = Qt.binding(function() {
					var result = false;
					for (var i = 0; i < childControls.length; i++)
						if (childControls[i].activeFocus)
							result = true;
					return result;
				});
		}
		
		if (debug)
			setDebugState();
	}
	
	states: [
		State
		{
			when: jaspControl.activeFocus && jaspControl.hasTabFocus && !jaspControl.childControlHasFocus
			PropertyChanges
			{
				target:			focusIndicator
				border.color:	Theme.focusBorderColor
				border.width:	Theme.jaspControlHighlightWidth
			}
		}
	]

	transitions: [
		Transition
		{
			ParallelAnimation
			{
				NumberAnimation
				{
					target:				focusIndicator
					properties:			"border.width";
					duration:			800;
					easing.type:		Easing.OutElastic;
					easing.amplitude:	1.5;
				}
				ColorAnimation
				{
					target:		focusIndicator
					duration:	100
				}
			}
		}
	]

	property string	toolTip:				""

	ToolTip.text:				toolTip
	ToolTip.timeout:			Theme.toolTipTimeout
	ToolTip.delay:				Theme.toolTipDelay
	ToolTip.toolTip.font:		Theme.font
	ToolTip.visible:			toolTip !== "" && controlMouseArea.containsMouse
	ToolTip.toolTip.background: Rectangle { color:	Theme.tooltipBackgroundColor } //This does set it for ALL tooltips ever after

	MouseArea
	{
		z:					5
		anchors.fill:		useControlMouseArea ? parent : undefined
		id:					controlMouseArea
		hoverEnabled:		true
		acceptedButtons:	Qt.NoButton
		cursorShape:		Qt.PointingHandCursor
	}
}
