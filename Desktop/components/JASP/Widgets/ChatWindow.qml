import QtQuick
import QtWebEngine
import QtWebChannel
import JASP

Window
{
    id: chatPanel
    objectName: "chatWindow"
    title: qsTr("JASP Chat")
    width: 600 * preferencesModel.uiScale
    height: 600 * preferencesModel.uiScale

    visible: mainWindow.aiChatVisible

    flags: Qt.Window | Qt.WindowStaysOnTopHint

	Connections {
		target: mainWindow
		function onCloseWindows() { chatPanel.close() }
	}

    onVisibleChanged:   {
        mainWindow.aiChatVisible = visible
    }

    onClosing: {
        aiBridge.clearChat();
    }

    WebEngineView {
        id: chatView
        anchors.fill: parent
        backgroundColor: jaspTheme.uiBackground



        // Start blank, load real page after webChannel is configured
        url: "about:blank"

        onNavigationRequested: (request) => {
            var realUrl = "qrc:///html/chat.html";
            if (request.url.toString() === realUrl ||
                request.navigationType === WebEngineNavigationRequest.ReloadNavigation) {
                request.accept()
            } else if (request.url.toString() !== "about:blank") {
                Qt.openUrlExternally(request.url)
                request.reject()
            }
        }

        webChannel.registeredObjects: [aiBridgeInterface]

        QtObject {
            id: aiBridgeInterface
            WebChannel.id: "aiBridge"

		property string aiIconPath: {
			var avatar = preferencesModel.aiPersonaModel.activePersonaAvatarWeb
			return avatar ? avatar : jaspTheme.iconPath + "jaspAI.png"
		}

		onAiIconPathChanged: { aiBridgeInterface.personaAvatarUpdated(aiIconPath) }

		property string conversationStatsJson: ""

            signal onStreamOpen()
            signal onStreamClose()
            signal onStreamChunk(string text)
    	        signal onStreamError(string error)
    	        signal onClearChat()
	signal conversationStatsUpdated()
	signal personaAvatarUpdated(string newPath)

            function startStream(messagesJson) { aiBridge.startStream(messagesJson) }
            function stopStream() { aiBridge.stopStream() }
            function setSystemMessage(text) { aiBridge.setSystemMessage(text) }
    	        function clearChat() { aiBridge.clearChat() }
	    function conversationStats() { return aiBridge.conversationStats() }
	    function openUrl(url) { Qt.openUrlExternally(url) }

            Component.onCompleted: {
                aiBridge.onStreamOpen.connect(aiBridgeInterface.onStreamOpen)
                aiBridge.onStreamClose.connect(aiBridgeInterface.onStreamClose)
                aiBridge.onStreamChunk.connect(aiBridgeInterface.onStreamChunk)
                aiBridge.onStreamError.connect(aiBridgeInterface.onStreamError)
                aiBridge.onClearChat.connect(aiBridgeInterface.onClearChat)
                aiBridge.conversationStatsUpdated.connect(function () {
                    aiBridgeInterface.conversationStatsJson = aiBridge.conversationStats()
                })
            }
        }

        Component.onCompleted: {
            // Load real page AFTER the webChannel is fully set up
            chatView.url = "qrc:///html/chat.html";
        }
    }
}
