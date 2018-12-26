import QtQuick 2.11
import QtWebEngine 1.7
import QtWebChannel 1.0
import QtQuick.Controls 2.4
import QtQuick.Controls 1.4 as OLD
import QtQuick.Layouts 1.3
import JASP.Theme 1.0
import JASP.Widgets 1.0

OLD.SplitView
{
	id:				panelSplit
	orientation:	Qt.Horizontal

	Item
	{
		id:						dataAndAnalyses
		Layout.fillWidth:		mainWindow.dataPanelVisible
		Layout.minimumWidth:	analyses.width
		Layout.maximumWidth:	mainWindow.dataPanelVisible ? panelSplit.width - Theme.minPanelWidth : analyses.width
		visible:				mainWindow.dataPanelVisible || mainWindow.analysesVisible
		width:					implicitWidth
		implicitWidth:			visible ? panelSplit.width / 2 : 0

		/*Connections
		{
			target:				mainWindow
			onDataPanelVisible:	if(mainWindow.dataPanelVisible) { dataAndAnalyses.width = panelSplit.width / 2 }
		}*/

		z: 1

		DataPanel
		{
			id:						data
			visible:				mainWindow.dataPanelVisible
			anchors.fill:			parent
			anchors.rightMargin:	analyses.extraSpace
			z:						1
		}

		MouseArea
		{
			visible:	mainWindow.analysesVisible
			z:	6
			anchors
			{
				top:	parent.top
				bottom:	parent.bottom
				left:	parent.left
				right:	analyses.left
			}

			onClicked:
			{
				if(mainWindow.analysesVisible)
					mainWindow.analysesVisible = false
				mouse.accepted = false
			}
		}

		AnalysisForms
		{
			id:				analyses
			z:				2

			anchors
			{
				top:		parent.top
				right:		parent.right
				bottom:		parent.bottom
			}
		}
	}


	WebEngineView
	{
		z:						3
		id:						resultsView
		url:					resultJsInterface.resultsPageUrl
		implicitWidth:			Theme.resultWidth
		Layout.minimumWidth:	Theme.minPanelWidth
	//	Layout.maximumWidth:	dataAndAnalyses.visible ? panelSplit.width / 2 : panelSplit.width

		webChannel:				resultJsInterface.channel

		Connections {
			target: resultJsInterface

			onRunJavaScript:			{ resultsView.runJavaScript(js)	}
			onRunJavaScriptCallback:	{
				var res = undefined;
				resultsView.runJavaScript(js, function(result) { console.log(result); res = result; })
				return res;
			}
		}


	}
}
