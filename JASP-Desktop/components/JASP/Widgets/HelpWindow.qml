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
	title:				"JASP Help"
	color:				jaspTheme.uiBackground

	Shortcut { onActivated: mainWindow.zoomInKeyPressed();		sequences: [Qt.Key_ZoomIn, "Ctrl+Plus", "Ctrl+\+", "Ctrl+\="];	}
	Shortcut { onActivated: mainWindow.zoomOutKeyPressed();		sequences: [Qt.Key_ZoomOut, "Ctrl+Minus", "Ctrl+\-"];			}
	Shortcut { onActivated: mainWindow.zoomResetKeyPressed();	sequences: ["Ctrl+0"];											}
	Shortcut { onActivated: helpWindowRoot.close();				sequences: ["Ctrl+Q", Qt.Key_Close];							}
	Shortcut { onActivated: helpWindowRoot.toggleFullScreen();	sequences: ["Ctrl+M"];											}
	Shortcut { onActivated: searchBar.startSearching();			sequences: ["Ctrl+F", Qt.Key_Search];							}

	UIScaleNotifier { anchors.centerIn:	parent }

	WebEngineView
	{
		id:						helpView
		url:					helpModel.indexURL()
		anchors.fill:			parent
		anchors.bottomMargin:	searchBar.height
		zoomFactor:				preferencesModel.uiScale
		backgroundColor:		jaspTheme.uiBackground
		onLoadingChanged:		if(loadRequest.status === WebEngineLoadRequest.LoadSucceededStatus)
									helpView.runJavaScript("window.setTheme(\"" + preferencesModel.currentThemeName + "\")");
		onNavigationRequested:
			if(request.navigationType === WebEngineNavigationRequest.LinkClickedNavigation)
			{
				Qt.openUrlExternally(request.url);
				request.action = WebEngineNavigationRequest.IgnoreRequest;
			}

		Connections
		{
			target:				helpModel
			onRunJavaScript:	helpView.runJavaScript(helpJS)
		}
	}

	TextField
	{
		id:						searchBar
		label:					qsTr("Search for:")
		value:					""
		onValueChanged:			helpView.findText(value)
		Keys.onEscapePressed:	value = ""

		function startSearching()
		{
			forceActiveFocus();
			control.selectAll()
		}

		anchors
		{
			left:	parent.left
			right:	parent.right
			bottom:	parent.bottom
		}

	}
}
