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
import QtQuick.Layouts	1.3 as L
import JASP				1.0

FocusScope
{
	id					: expanderWrapper
	implicitHeight		: expanderButton.visible ? expanderButton.height : 0
	implicitWidth		: expanderButton.visible ? parent.width : 0
	anchors.topMargin	: expanderButton.visible ? 15 * preferencesModel.uiScale : 0
	clip				: true
	L.Layout.columnSpan	: form ? form.columns : 1
	objectName			: "Section"

	default		property alias	content					: expanderArea.children
				property alias	button					: expanderButton
				property alias	childControlsArea		: expanderArea
				property alias	spacing					: expanderArea.rowSpacing
				property alias	text					: expanderButton.title
				property alias	info					: expanderButton.info
				property alias  title					: expanderButton.title
				property bool	expanded				: false
				property alias	debug					: expanderButton.debug
	readonly	property string iconsFolder				: jaspTheme.iconPath
	readonly	property string	expanderButtonIcon		: "expander-arrow-up.png"
				property alias	columns					: expanderArea.columns
	
	states: [
		State
		{
			name: "expanded";	when: expanderWrapper.expanded
			PropertyChanges {	target: expanderWrapper;	implicitHeight: expanderButton.height + expanderArea.anchors.topMargin + expanderArea.height + 2 }
			PropertyChanges {	target: expanderIcon;		rotation: 90;											}
		},
		State
		{
			name: "imploded";	when: !expanderWrapper.expanded
			PropertyChanges {	target: expanderWrapper;	implicitHeight: expanderButton.visible ? expanderButton.height : 0 }
			PropertyChanges {	target: expanderIcon;		rotation: 0;											}
		}
	]

	transitions: Transition
	{
		enabled:	preferencesModel.animationsOn
		
		NumberAnimation		{ property: "implicitHeight";	duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
		RotationAnimation	{								duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 }
	}
	
	ExpanderButtonBase
	{
		id						: expanderButton
		isBound					: false
		background				: expanderRectangle
		childControlsArea		: expanderArea
		width					: parent.width
		height					: 22 * preferencesModel.uiScale
		shouldStealHover		: false //Because sometimes maybe something *inside* an expanderButton might want to get hovered
		Keys.onSpacePressed		: toggleExpander()
		Keys.onReturnPressed	: toggleExpander()

		function toggleExpander() { expanderWrapper.expanded = !expanderWrapper.expanded; }
        
		MouseArea
		{
            anchors.fill: parent
			onClicked:
			{
                expanderButton.toggleExpander();
                expanderButton.forceActiveFocus();
            }
        }
        
		Rectangle
		{
			id				: expanderRectangle
			anchors.fill	: parent
			border.width	: 1
			border.color	: jaspTheme.borderColor
			radius			: jaspTheme.borderRadius
			color			: debug ? jaspTheme.debugBackgroundColor : jaspTheme.white

			Image
			{
				id					: expanderIcon
				anchors
				{
					left			: parent.left
					leftMargin		: 6 * preferencesModel.uiScale
					verticalCenter	: parent.verticalCenter
				}
				height				: 15 * preferencesModel.uiScale
				width				: 15 * preferencesModel.uiScale
				source				: jaspTheme.iconPath + "/large-arrow-right.png"
				sourceSize
				{
					width			: expanderIcon.width * 2
					height			: expanderIcon.height * 2
				}
			}
            
			Text
			{
				id						: label
				text					: expanderButton.title
				anchors.left			: expanderIcon.right
				anchors.leftMargin		: 5 * preferencesModel.uiScale
				anchors.verticalCenter	: parent.verticalCenter
				font					: jaspTheme.font
				color					: enabled ? jaspTheme.textEnabled : jaspTheme.textDisabled
            }
        }
    }

	GridLayout
	{
		id						: expanderArea
		rowSpacing				: jaspTheme.rowGridSpacing
		columnSpacing			: jaspTheme.columnGridSpacing
		anchors.leftMargin		: 5  * preferencesModel.uiScale
		anchors.top				: expanderButton.bottom
		anchors.topMargin		: 15 * preferencesModel.uiScale
		width					: parent.width
		columns					: 2
		visible					: expanderWrapper.implicitHeight > expanderButton.height //set to that focus chain does not enter closed expanders
//		anchors.bottomMargin	: 20 * preferencesModel.uiScale
    }

	Rectangle
	{
		z				: -1
		anchors.fill	: parent
		color			: jaspTheme.analysisBackgroundColor
	}    
}
