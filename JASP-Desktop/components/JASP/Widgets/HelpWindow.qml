import QtQuick 2.11
import QtQuick.Window 2.11
import QtWebEngine 1.7
import JASP.Widgets 1.0
import JASP.Theme 1.0

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
		onNavigationRequested:	request.action = request.navigationType === WebEngineNavigationRequest.ReloadNavigation || request.url == helpModel.indexURL() ? WebEngineNavigationRequest.AcceptRequest : WebEngineNavigationRequest.IgnoreRequest

		Connections
		{
			target:				helpModel
			onHelpJSChanged:	helpView.runJavaScript(helpModel.helpJS)
		}
	}
}
