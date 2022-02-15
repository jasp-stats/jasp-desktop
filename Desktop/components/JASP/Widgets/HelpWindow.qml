import QtQuick			2.12
import QtQuick.Window	2.12
import QtWebEngine		1.8
import JASP.Widgets		1.0
import JASP.Controls	1.0

Window
{
	id:					helpWindowRoot
	width:				400
	height:				700
	minimumWidth:		200 * preferencesModel.uiScale
	minimumHeight:		minimumWidth
	visible:			helpModel.visible
	onVisibleChanged:	helpModel.visible = visible
	title:				qsTr("JASP Help")
	color:				jaspTheme.uiBackground

	Shortcut { onActivated: mainWindow.zoomInKeyPressed();		sequences: [Qt.Key_ZoomIn, "Ctrl+Plus", "Ctrl+\+", "Ctrl+\="];	}
	Shortcut { onActivated: mainWindow.zoomOutKeyPressed();		sequences: [Qt.Key_ZoomOut, "Ctrl+Minus", "Ctrl+\-"];			}
	Shortcut { onActivated: mainWindow.zoomResetKeyPressed();	sequences: ["Ctrl+0"];											}
	Shortcut { onActivated: helpWindowRoot.close();				sequences: ["Ctrl+Q", "Ctrl+W", Qt.Key_Close];					}
	Shortcut { onActivated: helpWindowRoot.toggleFullScreen();	sequences: ["Ctrl+M"];											}
	Shortcut { onActivated: searchBar.startSearching();			sequences: ["Ctrl+F", Qt.Key_Search];							}


	Connections
	{
		target:			mainWindow
		function onCloseWindows() { helpWindowRoot.close(); }
	}

	UIScaleNotifier { anchors.centerIn:	parent }

	WebEngineView
	{
		id:						helpView
		url:					helpModel.indexURL()
		anchors.fill:			parent
		anchors.bottomMargin:	searchBar.height + (jaspTheme.generalAnchorMargin * 2)
		zoomFactor:				preferencesModel.uiScale
		backgroundColor:		jaspTheme.uiBackground
		onLoadingChanged:	(loadRequest)=>
		{
			if(loadRequest.status === WebEngineView.LoadSucceededStatus)
				helpModel.loadingSucceeded()
			searchBar.search();
		}

		onNavigationRequested: (request)=>
		{
			if(request.navigationType === WebEngineNavigationRequest.LinkClickedNavigation)
			{
				Qt.openUrlExternally(request.url);
				request.reject();
			}
		}

		Connections
		{
			target:				helpModel
			function onRunJavaScriptSignal(helpJS)
			{
				helpView.runJavaScript(helpJS);
				searchBar.search();
			}
		}
	}


	TextField
	{
		id:						searchBar
		label:					qsTr("Search for:")
		value:					""
		onValueChanged:			search()
		Keys.onEscapePressed:	value = ""
		fieldWidth:				parent.width - (controlLabel.width + control.anchors.leftMargin + jaspTheme.generalAnchorMargin * 2)

		function startSearching()
		{
			forceActiveFocus();
			control.selectAll()
		}

		function search()
		{
			helpView.findText(value)
		}

		anchors
		{
			left:			parent.left
			right:			parent.right
			bottom:			parent.bottom
			margins:		jaspTheme.generalAnchorMargin
		}

	}
}
