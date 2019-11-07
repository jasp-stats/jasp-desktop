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
import JASP.Controls	1.0

import JASP.Widgets		1.0
import JASP				1.0

AnalysisForm
{
	id:				form
	width:			jaspTheme.formWidth - ( 2 * jaspTheme.formMargin )
	height:			formContent.height + (jaspTheme.formMargin * 2)
	
	default property alias	content:	contentArea.children
	property alias	form:				form
	property bool	usesJaspResults:	true
	property int	majorVersion:		1
	property int	minorVersion:		0
	property bool	usesVariablesModel: false
	property int	availableWidth:		form.width - 2 * jaspTheme.formMargin
	property var	jaspControls:		[]
	property var    analysis:           myAnalysis
	property var	backgroundForms:	backgroundFlickable
	property alias	columns:			contentArea.columns
	
	property int    plotHeight:         320
	property int    plotWidth:          480
	
	function getJASPControls(controls, item, deep)
	{
		for (var i = 0; i < item.children.length; ++i)
		{
			var child = item.children[i];
			
			if (child.objectName === "Section")
			{
				controls.push(child.button);
				getJASPControls(controls, child.childControlsArea, deep);
			}
			else if (child instanceof JASPControl)
			{
				if (child.activeFocusOnTab)
				{
					controls.push(child);
					if (child.childControlsArea && deep)
						getJASPControls(controls, child.childControlsArea, deep);
				}
				else
					getJASPControls(controls, child, deep);
				
			}
			else
				getJASPControls(controls, child, deep);
		}
	}
	
	function showControlError(control, message, temporary)
	{
		controlErrorMessage.showMessage(control, message)
		if (temporary)
			controlErrorMessage.startTimeout()
	}

	function clearControlError()
	{
		if (controlErrorMessage.visible)
			controlErrorMessage.closeMessage()
	}
	
	Rectangle
	{
		id:				controlErrorMessage
		color:			jaspTheme.controlErrorBackgroundColor
		visible:		false
		opacity:		0
		width:			messageText.width + 20
		height:			messageText.height + 12
		z:				10
		radius:			4
		border.color:	jaspTheme.controlErrorTextColor
		border.width:	1

		function startTimeout()
		{
			messageTimer.start()
		}

		function closeMessage()
		{
			controlErrorMessage.opacity = 0
		}

		Timer
		{
			id:				messageTimer
			running:		false
			repeat:			false
			interval:		4000
			onTriggered:	controlErrorMessage.closeMessage()
		}

		Behavior on opacity
		{
			enabled: !preferencesModel.safeGraphics;

			NumberAnimation
			{
				duration: 300
				easing.type: Easing.InOutQuad
				onStopped: { controlErrorMessage.visible = (controlErrorMessage.opacity == 1) }
			}
		}

		function showMessage(control, message)
		{
			messageText.wrapMode = Text.NoWrap
			messageText.width = undefined
			messageText.text = message
			var controlPoint = control.mapToItem(form, control.width/2, 0)
			var x = controlPoint.x - (controlErrorMessage.width / 2)
			if (x < 0) x = 0
			if (x + controlErrorMessage.width > form.width)
			{
				if (controlErrorMessage.width < form.width)
					x = form.width - controlErrorMessage.width
				else
				{
					x = 0
					messageText.width = form.width - 10
					messageText.wrapMode = Text.Wrap
				}
			}

			var y = controlPoint.y - controlErrorMessage.height - 5
			if (y < 0) y = controlPoint.y + control.height + 5

			controlErrorMessage.x = x
			controlErrorMessage.y = y

			visible = true
			opacity = 1
		}

		Rectangle
		{
			id:				crossRectangle
			width:			12
			height:			12
			anchors.top:	parent.top
			anchors.right:	parent.right
			color:			"transparent"

			property int crossThickness: 2
			property int crossLengthOffset: -4

			Rectangle
			{
				anchors.centerIn:	parent
				height:				crossRectangle.crossThickness
				width:				parent.width + crossRectangle.crossLengthOffset
				rotation:			45
				color:				jaspTheme.controlErrorTextColor
			}

			Rectangle
			{
				anchors.centerIn:	parent
				height:				crossRectangle.crossThickness
				width:				parent.width + crossRectangle.crossLengthOffset
				rotation:			-45
				color:				jaspTheme.controlErrorTextColor
			}

			states:
			[
				State
				{
					when: crossArea.containsMouse
					PropertyChanges
					{
						target:				crossRectangle
						crossThickness:		3
						crossLengthOffset:	-2
					}
				}
			]

			MouseArea
			{
				id:				crossArea
				anchors.fill:	parent
				onClicked:		controlErrorMessage.closeMessage()
				hoverEnabled:	true
				cursorShape:	Qt.PointingHandCursor
			}
		}

		Text
		{
			id:						messageText
			font:					jaspTheme.font
			color:					jaspTheme.controlErrorTextColor
			anchors.verticalCenter: parent.verticalCenter
			anchors.left:			parent.left
			anchors.leftMargin:		5
			textFormat:				Text.RichText
		}
	}

	MouseArea
	{
		z:				-5
		anchors.fill:	parent
		onClicked:		form.forceActiveFocus()
	}

	IntegerField { visible: false; name: "plotWidth";  value: plotWidth }
	IntegerField { visible: false; name: "plotHeight"; value: plotHeight }
	
	
	FocusScope
	{
		id:				formContent
		width:			parent.width
		height:			errorMessagesBox.height + contentArea.implicitHeight
		anchors
		{
			top:		form.top
			left:		form.left
		}
				
		Rectangle
		{
			property alias text:	errorMessagesText.text
			
			id:				errorMessagesBox
			objectName:		"errorMessagesBox"
			visible:		false
			color:			jaspTheme.errorMessagesBackgroundColor
			width:			parent.width
			height:			visible ? errorMessagesText.height : 0

			Text
			{
				id:					errorMessagesText
				anchors.centerIn:	parent
				padding:			5
				wrapMode:			Text.Wrap
				width:				parent.width - 10
				verticalAlignment:	Text.AlignVCenter
			}
		}
		
		GridLayout
		{
			id:				contentArea
			anchors.top:	errorMessagesBox.bottom
			width:			parent.width
		}
	}
	
	Timer
	{
		id:				bindingTimer
		running:		false
		repeat:			false
		interval:		0
		onTriggered:
		{
			var previousExpander = null;
			getJASPControls(jaspControls, contentArea, true);

			for (var i = 0; i < jaspControls.length; i++)
				if (jaspControls[i].controlType !== "Expander")
					jaspControls[i].KeyNavigation.tab = jaspControls[(i + 1) % jaspControls.length];
				else
				{
					if (previousExpander)
						previousExpander.nextExpander = jaspControls[i];
					previousExpander = jaspControls[i];
				}
			
			if (previousExpander)
				previousExpander.nextExpander = jaspControls[0];
			
			for (i = 0; i < jaspControls.length; i++)
				if (jaspControls[i].indent)
					jaspControls[i].L.Layout.leftMargin = jaspTheme.indentationLength

			
			formCompleted();
		}
	}
	
	Component.onCompleted:	bindingTimer.start()
}
