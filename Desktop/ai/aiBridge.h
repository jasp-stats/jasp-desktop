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
#include <QString>

class PreferencesModel;
class TokenProvider;
class QNetworkRequest;

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
	QString authHeaderName() const;
	QString authHeaderPrefix() const;
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
	/// Only acts on an empty conversation, so it is safe to call whenever the chat
	/// window becomes visible. Interactive: obtaining a token for it may open the
	/// sign-in browser, which is what the user expects when they open the chat
	/// window or press the reset button.
	Q_INVOKABLE void sendIntroMessage();

	/// Export the full conversation to a Markdown file.
	Q_INVOKABLE void exportToMarkdownFile(const QString &filePath) const;

	// ------------------------------------------------------------------
	// Authentication — thin passthrough to the configured TokenProvider.
	// ------------------------------------------------------------------

	/// Start an interactive sign-in for the configured provider. A no-op for
	/// API-key auth, which needs no user interaction. The outcome arrives via
	/// authStateChanged(), authInteractionRequired() or onStreamError().
	Q_INVOKABLE void signIn();

	/// Discard cached credentials for the configured provider. Local only —
	/// nothing is revoked at the identity provider.
	Q_INVOKABLE void signOut();

	/// True when the configured provider holds a usable token. For API-key auth
	/// that simply means a key is set. A property, not just a method, because
	/// QML binds visible: to it — a bare method reads as undefined there.
	Q_PROPERTY(bool isSignedIn READ isSignedIn NOTIFY authStateChanged)
	bool isSignedIn() const;

	/// Who is signed in, and until when — for the preferences sign-in card.
	/// Empty / invalid when not signed in, or when the provider is a plain API
	/// key (which has no account). Bindings refresh through authStateChanged.
	Q_PROPERTY(QString authAccountName READ authAccountName NOTIFY authStateChanged)
	Q_PROPERTY(QDateTime authExpiresAt  READ authExpiresAt  NOTIFY authStateChanged)
	QString   authAccountName() const;
	QDateTime authExpiresAt() const;

signals:
	void onStreamOpen();
	void onStreamClose();
	void onStreamChunk(const QString &text);
	void onStreamError(const QString &error);
	void onClearChat();
	void testConnectionResult(bool success, const QString &message);
	void conversationStatsUpdated();

	/// Sign-in state changed — a token arrived, or credentials were dropped.
	void authStateChanged();

	/// The auth backend needs the user to do something; the message is
	/// display-ready. Any queued request resumes once a token arrives.
	void authInteractionRequired(const QString &message);

private slots:
	void onReadyRead();
	void onReplyFinished();
	void onReplyError(QNetworkReply::NetworkError error);

private:
	void sendToAI(const QJsonArray &messages, bool withTools = true);

	/// Greeting implementation. allowSignIn=false is for the automatic paths
	/// (clearChat after a config edit): an unprompted greeting must never open
	/// the sign-in browser — the chat window's own greeting or the sign-in
	/// button does that instead. It skips the greeting when no usable token is
	/// cached; the greeting then fires on the next user-visible trigger.
	void sendIntroMessage(bool allowSignIn);

	/// React to any of the config signals. Compares the effective configuration
	/// (endpoint, key, model, extras, persona…) against what the current
	/// conversation was built on, and clears the chat only when it actually
	/// differs — the signals themselves fire in storms at startup while models
	/// load indices that never really changed.
	void onEffectiveConfigMaybeChanged();

	/// Issue the streaming request. The endpoint is known, and the provider
	/// holds a usable token or no auth header is wanted.
	void postStreamingRequest(const QJsonArray &messages, bool withTools);

	/// Issue the testConnection probe. Same preconditions as above.
	void postTestConnection();

	/// TokenProvider signal handlers. A request queued while waiting for sign-in
	/// resumes in onTokenReady().
	void onTokenReady(const QString &token);
	void onAuthInteractionRequired(const QString &message);
	void onAuthFailed(const QString &error);

	void processSSELine(const QByteArray &line);
	void processSSEData(const QString &eventType, const QByteArray &data);
	void processToolCalls(const QJsonArray &toolCalls);
	void flushToolCalls();
	void continueWithToolResults(const QJsonArray &toolResults);
	QByteArray buildRequestBody(const QJsonArray &messages, bool withTools = true);
	void emitError(const QString &message);

	/// (Re)create m_tokenProvider when the configured auth mode changes. Also
	/// records why no backend could be built, so callers can explain it.
	void configureTokenProvider();

	/// Push the current auth settings into the provider. Providers never read
	/// AIConfigModel themselves, so auth/ stays independent of this feature.
	void pushAuthConfig();

	/// Apply the provider's token to a request using the configured header name
	/// and prefix ("Authorization: Bearer <token>" by default).
	void applyAuthHeader(QNetworkRequest &request) const;

	/// Add the configured static extra headers (routing/attribution only — never
	/// credentials, which live in SecretVault; parity with Claude's
	/// inferenceCustomHeaders, including its no-credentials rule). Never touches
	/// the auth header.
	void applyExtraHeaders(QNetworkRequest &request) const;

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

	TokenProvider *m_tokenProvider     = nullptr;
	QString        m_tokenProviderMode;   // auth mode the provider was built for
	QString        m_authUnavailable;     // non-empty => why that mode has no backend

	// Work queued while an interactive sign-in is in flight. Acquisition is
	// asynchronous, so the request is parked here and resumed in onTokenReady()
	// rather than blocking the event loop the browser flow needs.
	QJsonArray m_pendingMessages;
	bool       m_pendingSend      = false;
	bool       m_pendingWithTools = true;
	bool       m_pendingTest      = false;

	/// Last observed effective configuration; see onEffectiveConfigMaybeChanged().
	QString    m_effectiveConfigSignature;

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

	bool m_debugDumpEnabled = false; // conversation dump to <tempDir>/ai-request.json, opt-in only — contains full chat incl. tool results
	bool m_verboseLogging    = false;
	bool m_streaming = false;
	bool m_processingReply = false;   // true while inside onReplyFinished()
	bool m_deferredClearChat = false; // set when clearChat() is called during busy
	bool m_isIntroStream = false;     // true while the intro greeting is streaming

	static AiBridge *_singleton;
};

#endif // AIBRIDGE_H
