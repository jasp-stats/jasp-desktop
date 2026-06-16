import QtQuick
import QtQuick.Controls
import QtQuick.Dialogs
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

    function submitUserMessage(text)
    {
        // Ensure window is visible and focused
        if (!visible) visible = true;
        raise();
        requestActivate();

        // Use deep-chat's built-in submitUserMessage method to trigger the full normal flow
        chatView.runJavaScript('
            (function() {
                var chat = document.querySelector("deep-chat");
                if (chat) chat.submitUserMessage({text: ' + JSON.stringify(text) + '});
            })();
        ');
    }

    WebEngineView {
        id: chatView
        anchors.fill: parent
        anchors.bottomMargin: controlBar.height + disclaimerBar.height
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
	    function exportToMarkdownFile(path) { aiBridge.exportToMarkdownFile(path) }
	    function requestSave() { saveFileDialog.open() }

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

    // --- Control bar anchored below the chat ---
    Rectangle {
        id: controlBar
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: disclaimerBar.top
        height: 36 * preferencesModel.uiScale
        color: jaspTheme.uiBackground
        border.color: jaspTheme.borderColor

        Row {
            anchors.verticalCenter: parent.verticalCenter
            anchors.right: parent.right
            anchors.rightMargin: 8
            spacing: 6
            layoutDirection: Qt.RightToLeft

            // New conversation button
            Rectangle {
                width: newBtnText.implicitWidth + 16 * preferencesModel.uiScale
                height: 28 * preferencesModel.uiScale
                anchors.verticalCenter: parent.verticalCenter
                radius: jaspTheme.borderRadius
                color: newMouse.containsMouse ? jaspTheme.buttonColorHovered : "transparent"

                Text {
                    id: newBtnText
                    anchors.centerIn: parent
                    text: qsTr("New")
                    font: jaspTheme.font
                    color: jaspTheme.textEnabled
                }

                MouseArea {
                    id: newMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: { aiBridgeInterface.clearChat() }
                }
            }

            // Save button
            Rectangle {
                width: saveBtnText.implicitWidth + 16 * preferencesModel.uiScale
                height: 28 * preferencesModel.uiScale
                anchors.verticalCenter: parent.verticalCenter
                radius: jaspTheme.borderRadius
                color: saveMouse.containsMouse ? jaspTheme.buttonColorHovered : "transparent"

                Text {
                    id: saveBtnText
                    anchors.centerIn: parent
                    text: qsTr("Save")
                    font: jaspTheme.font
                    color: jaspTheme.textEnabled
                }

                MouseArea {
                    id: saveMouse
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor
                    onClicked: { aiBridgeInterface.requestSave() }
                }
            }

            // Token counter
            Item {
                anchors.verticalCenter: parent.verticalCenter
                width: tokenText.implicitWidth; height: tokenText.implicitHeight

                Text {
                    id: tokenText
                    anchors.verticalCenter: parent.verticalCenter
                    font: jaspTheme.font
                    color: jaspTheme.textDisabled
                    text: {
                        var stats = JSON.parse(aiBridgeInterface.conversationStatsJson || "{}");
                        var ctx = stats.lastRequestTokens || 0;
                        var total = stats.totalTokens || 0;
                        if (ctx === 0 && total === 0)
                            return "";
                        var s = formatTokens(ctx);
                        if (ctx > 0 && total > 0 && ctx !== total)
                            s += " / " + formatTokens(total);
                        else if (ctx === 0 && total > 0)
                            s = formatTokens(total);
                        return s + " " + qsTr("tokens");
                    }
                }

                MouseArea {
                    id: tokenMouseArea
                    anchors.fill: parent
                    hoverEnabled: true
                }

                ToolTip.visible: tokenMouseArea.containsMouse
                ToolTip.text: qsTr("Context window tokens / Total session tokens")
                ToolTip.delay: 500
            }
        }
    }

    function formatTokens(n) {
        if (n >= 1e6) return (n / 1e6).toFixed(1) + "M";
        if (n >= 1e3) return (n / 1e3).toFixed(1) + "K";
        return n;
    }

    // --- Disclaimer bar below the buttons ---
    Rectangle {
        id: disclaimerBar
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        height: disclaimerText.implicitHeight + 16 * preferencesModel.uiScale
        color: jaspTheme.uiBackground

        Rectangle {
            anchors.top: parent.top
            anchors.left: parent.left
            anchors.right: parent.right
            height: 1
            color: jaspTheme.borderColor
        }

        Text {
            id: disclaimerText
            anchors.centerIn: parent
            width: parent.width - 24 * preferencesModel.uiScale
            text: qsTr("This is a beta version of JASP AI. Please monitor the costs and always question AI output.\nIntended for personal use. Contact %1 for enterprise / institutional support.")
                .arg("<a href='https://www.jasp-services.com/contact/' style='color:#0091ea'>JASP Services BV</a>")
            font: jaspTheme.font
            color: jaspTheme.textEnabled
            wrapMode: Text.Wrap
            horizontalAlignment: Text.AlignHCenter
            textFormat: Text.RichText
            onLinkActivated: function(link) { Qt.openUrlExternally(link) }
        }
    }

    FileDialog {
        id: saveFileDialog
        title: qsTr("Save Conversation")
        fileMode: FileDialog.SaveFile
        nameFilters: [qsTr("Markdown files (*.md)"), qsTr("All files (*)")]
        defaultSuffix: "md"

        onAccepted: {
            aiBridgeInterface.exportToMarkdownFile(selectedFile)
        }
    }
}
