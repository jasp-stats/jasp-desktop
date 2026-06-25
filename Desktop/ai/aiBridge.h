//
// AiBridge — C++ backend for the AI chat feature.
//
// Bridges the deep-chat web component to AI providers (OpenAI, DeepSeek, etc.)
// via HTTP SSE streaming. Handles tool/function calling by dispatching through
// the existing JaspRpcDispatcher and feeding results back into the model loop.
//
// All configuration (endpoint, API key, model, system prompt, extra params)
// is read directly from PreferencesModel at request time — no cached copies.
//
// Architecture docs: Docs/development/aiBridge/
//
// Exposed to JavaScript via QWebChannel as the "aiBridge" object.
//

#ifndef AIBRIDGE_H
#define AIBRIDGE_H

#include <QObject>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QJsonArray>
#include <QJsonObject>
#include <QJsonDocument>
#include <QTimer>
#include <QMap>
#include <QVector>

class PreferencesModel;

class AiBridge : public QObject
{
	Q_OBJECT

public:
	explicit AiBridge(QObject *parent = nullptr);
	~AiBridge() override;

	static AiBridge * singleton() { return _singleton; }

	// ------------------------------------------------------------------
	// Configuration — reads PreferencesModel directly at request time.
	// No cached copies; getters are used by buildRequestBody/sendToAI.
	// ------------------------------------------------------------------

	QString endpoint() const;
	QString authToken() const;
	QString model() const;

	/// List all available persona names.
	Q_INVOKABLE QStringList personaNames() const;

	/// Switch the active persona by its ID.
	Q_INVOKABLE void setCurrentPersonaId(const QString &id);

	/// Set raw JSON extra parameters (persisted to preferences).
	Q_INVOKABLE void setExtraParams(const QString &json);

	// ------------------------------------------------------------------
	// Q_INVOKABLE — callable from JavaScript via QWebChannel
	// ------------------------------------------------------------------

	/// Called by chat-bridge.js when the user sends a message.
	Q_INVOKABLE void startStream(const QString &messagesJson);

	/// Called when the user clicks the stop button in deep-chat.
	Q_INVOKABLE void stopStream();

	/// Clear the full conversation history (start a new chat).
	Q_INVOKABLE void clearConversation();

	/// Full reset: stop any active stream, clear conversation, and
	/// notify the frontend to clear its message display.
	Q_INVOKABLE void clearChat();

	/// Return conversation stats as JSON.
	Q_INVOKABLE QString conversationStats() const;

	/// Enable writing the full request body to <tempDir>/ai-request.json when developer mode is on.
	Q_INVOKABLE void setDebugDumpEnabled(bool enabled);
	bool debugDumpEnabled() const { return m_debugDumpEnabled; }

	/// Quick connectivity test: sends a minimal request to the configured endpoint
	/// and reports success/failure via testConnectionResult signal.
	Q_INVOKABLE void testConnection();

	/// Send a hidden "Introduce yourself." message to prime the chat with a greeting.
	void sendIntroMessage();

	/// Export the full conversation to a Markdown file.
	Q_INVOKABLE void exportToMarkdownFile(const QString &filePath) const;

signals:
	void onStreamOpen();
	void onStreamClose();
	void onStreamChunk(const QString &text);
	void onStreamError(const QString &error);
	void onClearChat();
	void testConnectionResult(bool success, const QString &message);
	void conversationStatsUpdated();

private slots:
	void onReadyRead();
	void onReplyFinished();
	void onReplyError(QNetworkReply::NetworkError error);

private:
	void sendToAI(const QJsonArray &messages, bool withTools = true);
	void processSSELine(const QByteArray &line);
	void processSSEData(const QString &eventType, const QByteArray &data);
	void processToolCalls(const QJsonArray &toolCalls);
	void flushToolCalls();
	void continueWithToolResults(const QJsonArray &toolResults);
	QByteArray buildRequestBody(const QJsonArray &messages, bool withTools = true);
	void emitError(const QString &message);

	/// True when new work must not be started — a reply is being processed or
	/// an RPC dispatch is in-flight (possibly inside a nested event loop).
	bool isBusy() const;

	/// Map a QNetworkReply::NetworkError to a user-friendly string.
	/// Falls back to reply->errorString() for unrecognised codes.
	static QString networkErrorToString(QNetworkReply::NetworkError error,
										QNetworkReply *reply = nullptr);

	static int estimateTokens(const QString &text);
	static int estimateTokens(const QJsonObject &obj);
	static int estimateTokens(const QJsonArray &arr);
	static int estimateTokens(const QJsonValue &val);
	void logConversationStats(const char *context) const;
	void logToolCall(const QJsonObject &toolCall, const QString &resultText) const;
	void dumpConversationDump(const QJsonDocument &bodyDoc) const;

	/// Return the set of RPC tool names enabled for the currently active persona.
	QStringList effectiveToolsForActivePersona() const;

	// --- Members ---

	QNetworkAccessManager *m_networkManager = nullptr;
	QNetworkReply *m_activeReply = nullptr;
	QByteArray m_sseBuffer;

	QJsonArray m_conversation;
	QJsonArray m_pendingToolCalls;

	int m_totalRequestsSent = 0;
	int m_totalToolCallsDispatched = 0;
	int m_totalStreamChunks = 0;
	int m_totalInputTokens = 0;
	int m_totalOutputTokens = 0;
	int m_lastRequestTokens = 0;

	QMap<QString, QJsonObject> m_toolCallAccum;
	QVector<QString>      m_toolCallOrder;    // UUIDs in arrival order — replaces index/_seq/sort
	QString               m_lastToolCallId;   // active tool call receiving fragments
	QJsonObject m_assistantDelta;

	bool m_debugDumpEnabled = true;
	bool m_verboseLogging    = false;
	bool m_streaming = false;
	bool m_processingReply = false;   // true while inside onReplyFinished()
	bool m_deferredClearChat = false; // set when clearChat() is called during busy
	bool m_isIntroStream = false;     // true while the intro greeting is streaming

	static AiBridge *_singleton;
};

#endif // AIBRIDGE_H
