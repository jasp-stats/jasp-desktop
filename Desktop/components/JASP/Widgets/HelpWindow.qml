import QtQuick
import QtQuick.Window
import QtWebEngine
import JASP.Widgets
import JASP.Controls

Window
{
	id:					helpWindowRoot
	width:				400 * preferencesModel.uiScale
	height:				Math.min(700 * preferencesModel.uiScale, Screen.desktopAvailableHeight) //If bigger than screenheight weird jumping behaviour observed by Rens
	minimumWidth:		200 * preferencesModel.uiScale
	minimumHeight:		minimumWidth
	visible:			helpModel.visible
	onVisibleChanged:	helpModel.visible = visible
	title:				qsTr("JASP Help")
	color:				jaspTheme.uiBackground

	Shortcut { onActivated: helpWindowRoot.close();				sequences: ["Ctrl+W", Qt.Key_Close];			}
	Shortcut { onActivated: helpWindowRoot.toggleFullScreen();	sequences: ["Ctrl+M"];							}
	Shortcut { onActivated: searchBar.startSearching();			sequences: ["Ctrl+F", Qt.Key_Search];			}

	onClosing: { helpModel.markdown = ""; } //break binding

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
