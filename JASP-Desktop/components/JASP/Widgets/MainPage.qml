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

	DataPanel
	{
		id:						data
		Layout.fillWidth:		visible
		visible:				mainWindow.dataPanelVisible
		Layout.minimumWidth:	!visible ? 0 : Theme.minPanelWidth
		implicitWidth:			!visible ? 0 : Math.max(parent.width - Theme.resultWidth, Theme.formWidth)
	}

	AnalysisForms
	{
		id:						analyses
		visible:				mainWindow.dataPanelVisible
		Layout.minimumWidth:	!visible ? 0 : Theme.formWidth
		implicitWidth:			!visible ? 0 : Theme.formWidth
		Layout.maximumWidth:	!visible ? 0 : Theme.formWidth


	}


	WebEngineView
	{
		id:						resultsView
		url:					resultJsInterface.resultsPageUrl
		implicitWidth:			Theme.resultWidth
		Layout.minimumWidth:	Theme.minPanelWidth

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
