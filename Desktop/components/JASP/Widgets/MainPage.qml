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
import JASP
import QtQuick.Controls
import JASP.Controls		as JC

SplitView
{
	id: splitViewContainer

	property bool hasData:		mainWindow.dataAvailable
	property bool hasAnalysis:	mainWindow.analysesAvailable && !ribbonModel.dataMode

	function minimizeDataPanel()
	{
		dataPanel.width = 0
	}

	function maximizeDataPanel()
	{
		dataPanel.width = splitViewContainer.width
	}

	Connections
	{
		target:		analysesModel
		function onAnalysisAdded()
		{
			// When adding an analysis, if the analyses pane is cut or the results pane has not enough space, hide the data panel
			if (resultsPane.width < jaspTheme.resultWidth) //|| analysesPane.x < 0)
				minimizeDataPanel()
		}
	}
	
	handle: JASPSplitHandle
	{
		id:					handle
		
		onArrowClicked:
		{
			if (pointingLeft) minimizeDataPanel()
			else maximizeDataPanel()
		}
		pointingLeft:		x > 0
		toolTipArrow:		pointingLeft ? qsTr("Hide data")  : qsTr("Show data")
		toolTipDrag:		pointingLeft ? qsTr("Resize data/results") : qsTr("Drag to show data")
		//onXChanged:			checkPosition(false)
		//onDraggingChanged:	checkPosition(true)

		JC.ALTNavigation.enabled:				true
		JC.ALTNavigation.onTagMatch:			{ arrowClicked(); }
		JC.ALTNavigation.requestedPostfix:		"D"
		JC.ALTNavigation.y:					height / 2 - 25 * jaspTheme.uiScale

	}
	
	EnginesPanel
	{
		id:				enginesPanel
		visible:		engineSync.showEngines
	}

	DataPanel
	{
		id:						dataPanel
		visible:				mainWindow.showData
		
		SplitView.fillWidth:	true
	}
	

	AnalysesPanel
	{
		id:				analysesPane
		visible:		analysesModel.visible  && !ribbonModel.dataMode
		
		SplitView.minimumWidth:		0
		SplitView.preferredWidth:	implicitWidth
		SplitView.maximumWidth:		implicitWidth
	}
	
	HelpPanel
	{
		id:				helpPanel
		visible:		helpModel.visible
		
		SplitView.minimumWidth:		200 * preferencesModel.uiScale
		SplitView.maximumWidth:		jaspTheme.resultWidth
	}

	ResultsPanel
	{
		id:				resultsPane
		visible:		hasAnalysis && !ribbonModel.dataMode//mainWindow.showResults
	}
}
