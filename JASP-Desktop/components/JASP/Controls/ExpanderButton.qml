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
import QtQuick.Layouts 1.3 as L
import JASP.Theme 1.0

FocusScope
{
	id:					expanderWrapper
	implicitHeight:		expanderButton.height + (expanded ? expanderArea.anchors.topMargin + expanderArea.height : 0)
	implicitWidth:		parent.width
	anchors.topMargin:	15 * preferencesModel.uiScale
	clip:				true
	L.Layout.columnSpan: form.columns
	objectName:			"Section"

	default		property alias	content:		expanderArea.children
				property alias	button:			expanderButton
				property alias	childControlsArea: expanderArea
				property alias	spacing:		expanderArea.rowSpacing
				property alias	text:			label.text
                property alias  title:          label.text
				property bool	expanded:		false
				property alias	debug:			expanderButton.debug
	readonly	property string iconsFolder:	"qrc:/images/"
	readonly	property string	expandedIcon:	"expander-arrow-down.png"
	readonly	property string	contractedIcon: "expander-arrow-up.png"
				property alias	columns:		expanderArea.columns
				property alias	alignChildrenTopLeft: expanderArea.alignChildrenTopLeft

	Behavior on implicitHeight { PropertyAnimation { duration: 250; easing.type: Easing.OutQuad; easing.amplitude: 3 } }
  

	JASPControl
	{
		id:						expanderButton
		controlType:			"Expander"
		isBound:				false
		background:				expanderRectangle
		childControlsArea:		expanderArea
		width:					parent.width
		height:					22 * preferencesModel.uiScale
		Keys.onSpacePressed:	toggleExpander()
		Keys.onReturnPressed:   toggleExpander()
		KeyNavigation.tab:		expanderWrapper.expanded && childControls.length > 0 ? childControls[0] : nextExpander
        
        property var nextExpander: null
        
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
			id:				expanderRectangle
			anchors.fill:	parent
			border.width:	1
			border.color:	Theme.borderColor
			radius:			Theme.borderRadius
			color:			debug ? Theme.debugBackgroundColor : Theme.white
            
			Image
			{
				id:						icon
				height:					15 * preferencesModel.uiScale
				width:					15 * preferencesModel.uiScale
				anchors.left:			parent.left
				anchors.leftMargin:		6 * preferencesModel.uiScale
                anchors.verticalCenter: parent.verticalCenter
				source:					iconsFolder + (expanded ? expandedIcon : contractedIcon)
            }

			Text
			{
				id:						label
				anchors.left:			icon.right
				anchors.leftMargin:		5 * preferencesModel.uiScale
                anchors.verticalCenter: parent.verticalCenter
				font:					Theme.font
				color:					enabled ? Theme.textEnabled : Theme.textDisabled
            }
        }
    }

	GridLayout
	{
		id:						expanderArea
		rowSpacing:				Theme.rowGridSpacing
		columnSpacing:			Theme.columnGridSpacing
		anchors.leftMargin:		5  * preferencesModel.uiScale
		anchors.top:			expanderButton.bottom
		anchors.topMargin:		15 * preferencesModel.uiScale
		anchors.bottomMargin:	20 * preferencesModel.uiScale
		width:					parent.width
		columns:				2
    }

	Rectangle
	{
		z:				-1
		anchors.fill:	parent
		color:			Theme.analysisBackgroundColor
	}    
}
