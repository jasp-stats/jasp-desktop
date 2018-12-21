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
		Layout.fillWidth:		true
		//Layout.minimumWidth:	!visible ? 0 : Theme.formWidth
		visible:				true //data.visible || analyses.visible
		width:					implicitWidth
		implicitWidth:			visible ? panelSplit.width / 2 : 0
			//analyses.visible ? (!data.visible ? Theme.formWidth :)

		//color:	"green" //Theme.uiBackground*/
		z: 1

	/*	onVisibleChanged:
		{
			if(visible)
				width = panelSplit.width / 2
		}*/

		DataPanel
		{
			id:						data
			visible:				mainWindow.dataPanelVisible
			anchors.fill:			parent
			anchors.rightMargin:	analyses.extraSpaceRight
			z:						1
		}

		AnalysisForms
		{
			id:				analyses
			visible:		true
			z:				2
			//color:			"purple"
			width:			mainWindow.analysesVisible ? Math.min(parent.width, Theme.formWidth) : extraSpaceRight

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
