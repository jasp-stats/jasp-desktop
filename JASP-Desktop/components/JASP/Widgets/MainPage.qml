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
	orientation:	Qt.Horizontal

	DataPanel
	{
		id:						data
		width:					visible ? 300 : 0
		Layout.fillWidth:		visible
		Layout.minimumWidth:	visible ? 200 : 0
		visible:				mainWindow.dataPanelVisible
	}

	WebEngineView
	{
		id:						resultsView
		url:					resultJsInterface.resultsPageUrl
		Layout.minimumWidth:	100
		width:					600

		webChannel:				resultJsInterface.channel
		//Component.onCompleted:	resultJsInterface.channel = webChannel

		//onUrlChanged:			clear

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
