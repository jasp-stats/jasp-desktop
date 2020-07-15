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

import QtQuick				2.12
import JASP					1.0
import QtQuick.Controls		2.12
import QtQuick.Layouts		1.12 as L
//import QtQml 2.14


JASPControlBase
{
	id: jaspControl

	property var	focusIndicator			: background
	property bool   indent					: false
	property alias	cursorShape				: controlMouseArea.cursorShape
	property alias	hovered					: controlMouseArea.containsMouse
	property bool	useControlMouseArea		: true
	property bool	isDependency			: false
	property var	dependencyMustContain	: [] //Will be filled with QStringList when necessary
	property bool	shouldShowFocus			: activeFocus && focusOnTab && (innerControl === null || innerControl.activeFocus) && !hasError && !hasWarning
	property bool	shouldStealHover		: toolTip !== ""
	property string	toolTip					: info //By default the toolTip is exactly the same as whatever info was set on the option

	width:						implicitWidth
	height:						implicitHeight

	property int preferredHeight:	implicitHeight
	property int preferredWidth:	implicitWidth

	L.Layout.preferredWidth:	preferredWidth
	L.Layout.preferredHeight:	preferredHeight
	L.Layout.leftMargin:		indent ? jaspTheme.indentationLength : 0
	L.Layout.alignment:			Qt.AlignTop | Qt.AlignLeft

	visible: DEBUG_MODE || (!debug && !parentDebug)

	function setBackgroundColor()
	{
		if (background !== null)
			background.color = (debug || parentDebug) ? jaspTheme.debugBackgroundColor : "transparent"
	}

	onDebugChanged:			setBackgroundColor()
	onParentDebugChanged:	setBackgroundColor()

	states: [
		State
		{
			name: "hasError"
			when: focusIndicator !== null  && jaspControl.hasError
			PropertyChanges
			{
				target:			focusIndicator
				border.color:	jaspTheme.controlErrorTextColor
				border.width:	jaspTheme.jaspControlHighlightWidth
			}
		},
		State
		{
			name: "hasWarning"
			when: focusIndicator !== null  && jaspControl.hasWarning && !jaspControl.hasError
			PropertyChanges
			{
				target:			focusIndicator
				border.color:	jaspTheme.controlWarningTextColor
				border.width:	jaspTheme.jaspControlHighlightWidth
			}
		},
		State
		{
			name: "hasFocus"
			when: focusIndicator !== null  && jaspControl.shouldShowFocus
			PropertyChanges
			{
				target:			focusIndicator
				border.color:	jaspTheme.focusBorderColor
				border.width:	jaspTheme.jaspControlHighlightWidth
			}
		},
		State
		{
			name: "isDependency"
			when: focusIndicator !== null && jaspControl.isDependency && !jaspControl.shouldShowFocus
			PropertyChanges
			{
				target:			focusIndicator
				border.color:	jaspTheme.dependencyBorderColor
				border.width:	jaspTheme.jaspControlHighlightWidth
			}
		}
	]

	transitions: [
		Transition
		{
			to: "hasFocus"
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

	ToolTip.text:				toolTip
	ToolTip.timeout:			jaspTheme.toolTipTimeout
	ToolTip.delay:				jaspTheme.toolTipDelay
	ToolTip.toolTip.font:		jaspTheme.font
	ToolTip.visible:			toolTip !== "" && controlMouseArea.containsMouse

	MouseArea
	{
		z:					5
		anchors.fill:		useControlMouseArea ? parent : undefined
		id:					controlMouseArea
		hoverEnabled:		jaspControl.shouldStealHover
		acceptedButtons:	Qt.NoButton
		cursorShape:		Qt.PointingHandCursor
	}
}
