import QtQuick 2.11
import QtQuick.Window 2.11
import QtWebEngine 1.7
import JASP.Widgets 1.0

Window
{
	width:				400
	height:				700
	visible:			helpModel.visible
	onVisibleChanged:	helpModel.visible = visible
	title:				"JASP Help"

	WebEngineView
	{
		id:						helpView
		url:					helpModel.indexURL()
		anchors.fill:			parent
		onNavigationRequested:
		{
			if(request.navigationType === WebEngineNavigationRequest.LinkClickedNavigation)
			{
				Qt.openUrlExternally(request.url);
				request.action = WebEngineNavigationRequest.IgnoreRequest;
			}
			//else
			//	request.action = request.navigationType !== WebEngineNavigationRequest.BackForwardNavigation && request.navigationType !== WebEngineNavigationRequest.TypedNavigation ? WebEngineNavigationRequest.AcceptRequest : WebEngineNavigationRequest.IgnoreRequest
		}

		Connections
		{
			target:				helpModel
			onHelpJSChanged:	helpView.runJavaScript(helpModel.helpJS)
		}
	}
}
