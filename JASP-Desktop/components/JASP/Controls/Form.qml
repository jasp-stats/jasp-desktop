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
	id					: form
	implicitWidth		: jaspTheme.formWidth - ( 2 * jaspTheme.formMargin )
	implicitHeight		: formContent.height + (jaspTheme.formMargin * 2)
	width				: implicitWidth
	height				: implicitHeight
	errorMessagesItem	: errorMessagesBox
	
	default property alias	content		: contentArea.children
	property alias	form				: form
	property bool	usesJaspResults		: true
	property int	majorVersion		: 1
	property int	minorVersion		: 0
	property int	availableWidth		: form.width - 2 * jaspTheme.formMargin
	property var    analysis			: myAnalysis
	property var	backgroundForms		: backgroundFlickable
	property alias	columns				: contentArea.columns
	property bool	runAnalysisWhenOptionChange : true

	property int    plotHeight			: 320
	property int    plotWidth			: 480

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
		height:			oldFileMessagesBox.height + errorMessagesBox.height + contentArea.implicitHeight
		anchors
		{
			top:		form.top
			left:		form.left
		}

		Rectangle
		{
			id:				oldFileMessagesBox
			visible:		myAnalysis !== null && myAnalysis.needsRefresh && myAnalysis.hasVolatileNotes //Ill leave the text as is for now to avoid having to go back to the po again
			color:			jaspTheme.controlWarningBackgroundColor
			width:			form.implicitWidth
			height:			visible ? oldAnalysisText.height : 0
			anchors.top:	parent.top

			Text
			{
				id:					oldAnalysisText
				color:				jaspTheme.controlWarningTextColor
				anchors.centerIn:	parent
				padding:			5 * jaspTheme.uiScale
				wrapMode:			Text.Wrap
				width:				parent.width - 10 * jaspTheme.uiScale
				verticalAlignment:	Text.AlignVCenter
				text:				qsTr("This analysis was created with an older version of JASP (or a dynamic module)") + //I do not want to bother with formatting strings here to be honest
									( myAnalysis !== null && !myAnalysis.hasVolatileNotes ?
										qsTr(", refreshing could give a slightly different result.") :
										qsTr(", to keep your notes where they are it is highly recommended to first refresh your analyses!"))

			}
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
			anchors.top:	oldFileMessagesBox.bottom

			Text
			{
				id:					errorMessagesText
				anchors.centerIn:	parent
				padding:			5 * jaspTheme.uiScale
				wrapMode:			Text.Wrap
				width:				parent.width - 10 * jaspTheme.uiScale
				verticalAlignment:	Text.AlignVCenter
				//Should we maybe set a color here?
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
		onTriggered:	formCompleted();
	}
	
	Component.onCompleted:	bindingTimer.start()
}
