//
// AiBridge implementation — handles HTTP SSE streaming to AI providers
// and orchestrates tool-call loops through the JaspRpcDispatcher.
//

#include "aiBridge.h"

#include <QJsonDocument>
#include <QJsonObject>
#include <QJsonArray>
#include <QNetworkRequest>
#include <QUrl>
#include <QFile>

#include "log.h"
#include "dirs.h"
#include "rpc/jasprpcdispatcher.h"
#include "gui/preferencesmodel.h"
#include "gui/aipersonamodel.h"

// =============================================================================
// Singleton
// =============================================================================

AiBridge *AiBridge::_singleton = nullptr;

// =============================================================================
// Constructor / Destructor
// =============================================================================

	AiBridge::AiBridge(QObject *parent)
		: QObject(parent)
		, m_networkManager(new QNetworkAccessManager(this))
	{
		assert(!_singleton);
		_singleton = this;

		m_totalRequestsSent = 0;
		m_totalToolCallsDispatched = 0;
		m_totalStreamChunks = 0;

		Log::log() << "AiBridge initialized" << std::endl;

		// Clear chat when persona changes so the new persona takes effect from a clean slate
		AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
		if (pm)
			connect(pm, &AIPersonaModel::currentPersonaIndexChanged, this, &AiBridge::clearChat);
	}

AiBridge::~AiBridge()
{
	stopStream();
	_singleton = nullptr;
}

// =============================================================================
// Configuration — reads PreferencesModel directly.
// =============================================================================

QString AiBridge::endpoint() const
{
	return PreferencesModel::prefs()->aiEndpoint();
}

QString AiBridge::authToken() const
{
	return PreferencesModel::prefs()->aiApiKey();
}

QString AiBridge::model() const
{
	return PreferencesModel::prefs()->aiModel();
}

QStringList AiBridge::personaNames() const
{
	QStringList names;
	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	if (pm) {
		for (int i = 0; i < pm->rowCount(); ++i)
			names.append(pm->data(pm->index(i, 0), AIPersonaModel::NameRole).toString());
	}
	return names;
}

void AiBridge::setCurrentPersonaId(const QString &id)
{
	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	if (!pm) return;
	for (int i = 0; i < pm->rowCount(); ++i) {
		if (pm->data(pm->index(i, 0), AIPersonaModel::IdRole).toString() == id) {
			pm->setCurrentPersonaIndex(i);
			return;
		}
	}
}

void AiBridge::setExtraParams(const QString &json)
{
	// Validate before persisting
	if (!json.trimmed().isEmpty()) {
		QJsonDocument doc = QJsonDocument::fromJson(json.toUtf8());
		if (!doc.isObject()) {
			Log::log() << "AiBridge: extraParams is not a valid JSON object, ignoring" << std::endl;
			return;
		}
	}
	PreferencesModel::prefs()->setAiExtraParams(json);
}

void AiBridge::setDebugDumpEnabled(bool enabled)
{
	m_debugDumpEnabled = enabled;
	Log::log() << "AiBridge: debug dump " << (enabled ? "enabled" : "disabled") << std::endl;
}

// =============================================================================
// Q_INVOKABLE — JavaScript entry points
// =============================================================================

void AiBridge::startStream(const QString &messagesJson)
{
	Log::log() << "AiBridge::startStream called with " << messagesJson.size() << " chars" << std::endl;

	// Re-entrancy guard: if a dispatched RPC handler is still on the
	// call stack (e.g. analysis_results with wait=true is in a nested
	// QEventLoop), reject new messages to prevent AiBridge state
	// corruption.  The guard is centralized in JaspRpcDispatcher.
	{
		auto* disp = JaspRpcDispatcher::singleton();
		if (disp && disp->inFlight()) {
			Log::log() << "AiBridge: startStream rejected — RPC dispatch in flight" << std::endl;
			emit onStreamError("A tool call is still executing. Please wait for it to finish.");
			return;
		}
	}

	if (endpoint().isEmpty()) {
		Log::log() << "AiBridge: No endpoint configured — cannot send" << std::endl;
		emitError("No AI endpoint configured. Set one with aiBridge.setEndpoint(url).");
		return;
	}

	if (m_streaming) {
		Log::log() << "AiBridge: Stream already active, stopping previous." << std::endl;
		stopStream();
	}

	QJsonParseError parseError;
	QJsonDocument doc = QJsonDocument::fromJson(messagesJson.toUtf8(), &parseError);

	if (parseError.error != QJsonParseError::NoError) {
		emitError(QStringLiteral("Failed to parse messages JSON: ") + parseError.errorString());
		return;
	}

	if (!doc.isArray()) {
		emitError(QStringLiteral("Messages must be a JSON array"));
		return;
	}

	// Append new messages to conversation (don't replace)
	int prevSize = m_conversation.size();
	for (const QJsonValue &v : doc.array())
		m_conversation.append(v);
	m_streaming = true;

	m_assistantDelta = QJsonObject();
	m_toolCallAccum.clear();

	int totalTokens = estimateTokens(m_conversation);
	Log::log() << "AiBridge: Starting stream — conversation grew from " << prevSize
	           << " to " << m_conversation.size() << " messages"
	           << " (~" << totalTokens << " tokens accumulated)" << std::endl;
	logConversationStats("startStream");

	emit onStreamOpen();
	sendToAI(m_conversation);
}

void AiBridge::stopStream()
{
	Log::log() << "AiBridge::stopStream called" << std::endl;
	m_streaming = false;
	m_assistantDelta = QJsonObject();
	m_toolCallAccum.clear();
	if (m_activeReply) {
		// Disconnect before abort — abort() may emit finished synchronously,
		// and onReplyFinished can start a new tool-call stream otherwise.
		m_activeReply->disconnect(this);
		m_activeReply->abort();
		m_activeReply->deleteLater();
		m_activeReply = nullptr;
	}
	m_sseBuffer.clear();
	emit onStreamClose();
}

void AiBridge::clearConversation()
{
	Log::log() << "AiBridge::clearConversation — clearing " << m_conversation.size()
	           << " messages (~" << estimateTokens(m_conversation) << " tokens)" << std::endl;
	m_conversation = QJsonArray();
	m_assistantDelta = QJsonObject();
	m_toolCallAccum.clear();
	m_pendingToolCalls = QJsonArray();
}

void AiBridge::clearChat()
{
	Log::log() << "AiBridge::clearChat — full reset requested" << std::endl;
	stopStream();
	clearConversation();
	emit onClearChat();
	sendIntroMessage();
}

void AiBridge::sendIntroMessage()
{
	if (endpoint().isEmpty()) return;

	QJsonObject introMsg;
	introMsg[QStringLiteral("role")] = QStringLiteral("user");
	introMsg[QStringLiteral("content")] = QStringLiteral("Give a short introduction.");
	m_conversation.append(introMsg);

	m_streaming = true;
	m_assistantDelta = QJsonObject();
	m_toolCallAccum.clear();

	Log::log() << "AiBridge: sending intro message" << std::endl;

	emit onStreamOpen();
	sendToAI(m_conversation, false);
}

QString AiBridge::conversationStats() const
{
	QJsonObject stats;
	stats[QStringLiteral("messageCount")] = m_conversation.size();
	stats[QStringLiteral("estimatedTokens")] = estimateTokens(m_conversation);
	stats[QStringLiteral("toolCallsDispatched")] = m_totalToolCallsDispatched;
	stats[QStringLiteral("requestsSent")] = m_totalRequestsSent;
	stats[QStringLiteral("streamChunks")] = m_totalStreamChunks;
	stats[QStringLiteral("totalInputTokens")] = m_totalInputTokens;
	stats[QStringLiteral("totalOutputTokens")] = m_totalOutputTokens;
	stats[QStringLiteral("totalTokens")] = m_totalInputTokens + m_totalOutputTokens;
	return QString::fromUtf8(QJsonDocument(stats).toJson(QJsonDocument::Compact));
}

// =============================================================================
// HTTP request
// =============================================================================

void AiBridge::sendToAI(const QJsonArray &messages, bool withTools)
{
	QString ep = endpoint();
	if (ep.isEmpty()) {
		m_streaming = false;
		emitError(QStringLiteral("No AI endpoint configured. Please set an endpoint URL."));
		emit onStreamClose();
		return;
	}

	QNetworkRequest request{QUrl(ep)};
	request.setHeader(QNetworkRequest::ContentTypeHeader, QStringLiteral("application/json"));
	request.setRawHeader("Accept", "text/event-stream");
	request.setTransferTimeout(120000); // 120 s — large conversations with tool results need time

	QString token = authToken();
	if (!token.isEmpty()) {
		request.setRawHeader("Authorization", ("Bearer " + token).toUtf8());
	}

	QByteArray body = buildRequestBody(messages, withTools);

	// Check token limit against the full request body (system prompt + tools + messages)
	if (PreferencesModel::prefs()->aiChatLimitActive()) {
		int limit = PreferencesModel::prefs()->aiChatLimit();
		int bodyTokens = estimateTokens(QString::fromUtf8(body));
		if (limit > 0 && bodyTokens > limit) {
			Log::log() << "AiBridge: request exceeds token limit (" << bodyTokens << " > " << limit << "), rejecting" << std::endl;
			m_streaming = false;
			emit onStreamError(QStringLiteral("Request exceeds the token limit (~%1 tokens, limit is %2). Please clear the chat or reduce your message.").arg(bodyTokens).arg(limit));
			emit onStreamClose();
			return;
		}
	}

	m_totalRequestsSent++;

	// --- Debug dump: readable structure, compact internals ---
	if (m_debugDumpEnabled && PreferencesModel::prefs()->developerMode() && !Dirs::tempDir().empty()) {
		std::string path = Dirs::tempDir() + "/ai-request.json";
		QFile file(QString::fromStdString(path));
		if (file.open(QIODevice::WriteOnly | QIODevice::Truncate)) {
			QJsonDocument bodyDoc = QJsonDocument::fromJson(body);
			if (bodyDoc.isObject()) {
				QJsonObject obj = bodyDoc.object();
				file.write("{\n");
				file.write(QStringLiteral("  \"model\": \"%1\",\n").arg(obj[QStringLiteral("model")].toString()).toUtf8());
				file.write(QStringLiteral("  \"stream\": %1,\n").arg(obj[QStringLiteral("stream")].toBool() ? QStringLiteral("true") : QStringLiteral("false")).toUtf8());

				// messages array
				file.write("  \"messages\": [\n");
				const QJsonArray msgs = obj[QStringLiteral("messages")].toArray();
				for (int i = 0; i < msgs.size(); ++i) {
					file.write("    ");
					file.write(QJsonDocument(msgs[i].toObject()).toJson(QJsonDocument::Compact));
					bool hasMore = (i < msgs.size() - 1) || obj.contains(QStringLiteral("tools"));
					if (!hasMore) {
						for (auto it = obj.begin(); it != obj.end(); ++it) {
							if (it.key() != QLatin1String("model") && it.key() != QLatin1String("stream") &&
								it.key() != QLatin1String("messages") && it.key() != QLatin1String("tools"))
								{ hasMore = true; break; }
						}
					}
					if (hasMore) file.write(",");
					file.write("\n");
				}
				file.write("  ]");

				// tools array
				if (obj.contains(QStringLiteral("tools"))) {
					file.write(",\n  \"tools\": [\n");
					const QJsonArray tools = obj[QStringLiteral("tools")].toArray();
					for (int i = 0; i < tools.size(); ++i) {
						file.write("    ");
						file.write(QJsonDocument(tools[i].toObject()).toJson(QJsonDocument::Compact));
						if (i < tools.size() - 1) file.write(",");
						file.write("\n");
					}
					file.write("  ]");
				}

				// Any extra keys merged from extraParams (max_tokens, thinking, etc.)
				for (auto it = obj.begin(); it != obj.end(); ++it) {
					const auto &k = it.key();
					if (k == QLatin1String("model") || k == QLatin1String("stream") ||
						k == QLatin1String("messages") || k == QLatin1String("tools"))
						continue;
					file.write(",\n  ");
					// Write just the key:value pair (strip the outer { } from the compact JSON)
					QByteArray pair = QJsonDocument(QJsonObject{{it.key(), it.value()}}).toJson(QJsonDocument::Compact);
					file.write(QString::fromUtf8(pair).mid(1).chopped(1).toUtf8());
				}

				file.write("\n}\n");
			} else {
				file.write(body);
			}
			file.close();
		}
	}

	// --- Token monitoring ---
	int bodyTokens = estimateTokens(QString::fromUtf8(body));
	int convTokens = estimateTokens(messages);
	int toolsTokens = 0;
	{
		QJsonDocument bodyDoc = QJsonDocument::fromJson(body);
		if (bodyDoc.isObject() && bodyDoc.object().contains(QStringLiteral("tools")))
			toolsTokens = estimateTokens(bodyDoc.object()[QStringLiteral("tools")]);
	}
	m_totalInputTokens += bodyTokens;

	Log::log() << "AiBridge: POST #" << m_totalRequestsSent << " to " << ep.toStdString()
	           << " | " << body.size() << " bytes"
	           << " | ~" << bodyTokens << " tokens in"
	           << " | ~" << m_totalInputTokens << " total in this session"
	           << " | messages: " << messages.size() << " (~" << convTokens << " tokens)"
	           << " | tools: ~" << toolsTokens << " tokens"
	<< " | system prompt (common+persona): " << estimateTokens(PreferencesModel::prefs()->aiCommonSystemPrompt() + (PreferencesModel::prefs()->aiPersonaModel() ? PreferencesModel::prefs()->aiPersonaModel()->activePersona().personaPrompt : QString())) << " tokens"
	           << std::endl;

	// Log the full request body at a debug level (can be very verbose)
	Log::log() << "AiBridge: REQUEST BODY:\n" << QJsonDocument::fromJson(body).toJson(QJsonDocument::Indented).toStdString() << std::endl;

	m_activeReply = m_networkManager->post(request, body);

	connect(m_activeReply, &QNetworkReply::readyRead,    this, &AiBridge::onReadyRead);
	connect(m_activeReply, &QNetworkReply::finished,     this, &AiBridge::onReplyFinished);
	connect(m_activeReply, &QNetworkReply::errorOccurred, this, &AiBridge::onReplyError);
}

QByteArray AiBridge::buildRequestBody(const QJsonArray &messages, bool withTools)
{
	QJsonObject body;

	body[QStringLiteral("model")] = model();
	body[QStringLiteral("stream")] = true;

	// Build the full messages array with optional system message
	QJsonArray fullMessages;

	// Build the system message: Common System Prompt first, then Persona Prompt
	QString sysContent;

	// 1. Common System Prompt
	QString commonPrompt = PreferencesModel::prefs()->aiCommonSystemPrompt();
	if (!commonPrompt.isEmpty())
		sysContent = commonPrompt.trimmed() + QStringLiteral("\n\n");

	// 2. Persona Prompt
	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	if (pm && pm->count() > 0) {
		const PersonaEntry &p = pm->activePersona();
		if (!p.personaPrompt.isEmpty()) {
			sysContent += QStringLiteral("Persona Prompt:\n");
			sysContent += QStringLiteral("  Your name is: ") + p.name + QStringLiteral("\n\n");
			sysContent += QStringLiteral("  ") + QString(p.personaPrompt).trimmed().replace(QStringLiteral("\n"), QStringLiteral("\n  ")) + QStringLiteral("\n");
		}
	} else if (sysContent.isEmpty()) {
		sysContent = QStringLiteral("You are JASP AI, a helpful assistant.");
	}

	if (!sysContent.isEmpty()) {
		QJsonObject sysMsg;
		sysMsg[QStringLiteral("role")] = QStringLiteral("system");
		sysMsg[QStringLiteral("content")] = sysContent.trimmed();
		fullMessages.append(sysMsg);
	}

	// Build full tool definitions (skipped for intro/lightweight requests)
	if (withTools)
	{
		bool useCompleteSchema = PreferencesModel::prefs()->aiUseCompleteSchema();
		QJsonArray toolDefs;
		QJsonArray toolStubs;
		{
		JaspRpcDispatcher *disp = JaspRpcDispatcher::singleton();
			if (disp) {
				QStringList enabled = effectiveToolsForActivePersona();
				QSet<QString> enabledSet(enabled.begin(), enabled.end());
				for (const auto &tname : disp->knownSpecNames()) {
					QString qname = QString::fromStdString(tname);
					if (!enabledSet.contains(qname)) continue;
					const RpcMethodSpec *spec = disp->getSpec(tname);
					if (!spec) continue;

					// Full definition
					QJsonObject tool;
					tool[QStringLiteral("type")] = QStringLiteral("function");
					QJsonObject func;
					func[QStringLiteral("name")] = QString::fromStdString(tname);
					func[QStringLiteral("description")] = QString::fromStdString(spec->summary);
					QJsonObject params;
					params[QStringLiteral("type")] = QStringLiteral("object");
					QJsonObject props;
					QJsonArray required;
					for (const auto &p : spec->params) {
						QJsonObject prop;
						prop[QStringLiteral("type")] = QString::fromStdString(p.schema.type);
						prop[QStringLiteral("description")] = QString::fromStdString(p.description);
						props[QString::fromStdString(p.name)] = prop;
						if (p.required) required.append(QString::fromStdString(p.name));
					}
					params[QStringLiteral("properties")] = props;
					if (!required.isEmpty()) params[QStringLiteral("required")] = required;
					func[QStringLiteral("parameters")] = params;
					tool[QStringLiteral("function")] = func;
					toolDefs.append(tool);

					// Name-only stub for tools array (used in compact mode)
					QJsonObject stub;
					stub[QStringLiteral("type")] = QStringLiteral("function");
					QJsonObject stubFunc;
					stubFunc[QStringLiteral("name")] = QString::fromStdString(tname);
					stub[QStringLiteral("function")] = stubFunc;
					toolStubs.append(stub);
				}
			}
	}

	if (useCompleteSchema && !toolDefs.isEmpty())
	{
		// Full schemas in the structured tools array — models see types
		body[QStringLiteral("tools")] = toolDefs;
	}
	else if (!toolStubs.isEmpty())
	{
		// System message carries the full definitions as text
		if (!toolDefs.isEmpty()) {
			QJsonObject toolSysMsg;
			toolSysMsg[QStringLiteral("role")] = QStringLiteral("system");
			toolSysMsg[QStringLiteral("content")] = QStringLiteral("Available tools:\n") + QString::fromUtf8(QJsonDocument(toolDefs).toJson(QJsonDocument::Compact));
			fullMessages.append(toolSysMsg);
		}
		// Tools array gets name-only stubs (token optimization)
		body[QStringLiteral("tools")] = toolStubs;
		}
	}

	for (const QJsonValue &val : messages) {
		QJsonObject msg = val.toObject();
		// Normalize: deep-chat uses "text", AI APIs use "content"
		if (msg.contains(QStringLiteral("text")) && !msg.contains(QStringLiteral("content"))) {
			msg[QStringLiteral("content")] = msg[QStringLiteral("text")].toString();
			msg.remove(QStringLiteral("text"));
		}
		fullMessages.append(msg);
	}

	body[QStringLiteral("messages")] = fullMessages;

	// Merge per-message extra fields (e.g. Anthropic cache_control) into every message.
	// Protected fields (role, content, text) cannot be overridden.
	QString msgExtra = PreferencesModel::prefs()->aiMessageExtra();
	if (!msgExtra.isEmpty()) {
		QJsonParseError parseError;
		QJsonDocument msgExtraDoc = QJsonDocument::fromJson(msgExtra.toUtf8(), &parseError);
		if (msgExtraDoc.isObject()) {
			const QJsonObject msgExtraObj = msgExtraDoc.object();
			for (int i = 0; i < fullMessages.size(); ++i) {
				QJsonObject msg = fullMessages[i].toObject();
				for (auto it = msgExtraObj.begin(); it != msgExtraObj.end(); ++it) {
					if (it.key() == QLatin1String("role") ||
						it.key() == QLatin1String("content") ||
						it.key() == QLatin1String("text"))
						continue;
					msg[it.key()] = it.value();
				}
				fullMessages[i] = msg;
			}
		} else {
			Log::log() << "AiBridge: messageExtra is not valid JSON — "
			           << parseError.errorString().toStdString() << ", ignoring" << std::endl;
		}
	}

		// Merge user-specified extra parameters (e.g. max_tokens, thinking, etc.)
		QString extra = PreferencesModel::prefs()->aiExtraParams();
		if (!extra.isEmpty()) {
			QJsonDocument extraDoc = QJsonDocument::fromJson(extra.toUtf8());
			if (extraDoc.isObject()) {
				const QJsonObject extraObj = extraDoc.object();
				for (auto it = extraObj.begin(); it != extraObj.end(); ++it) {
					// Don't let extra params override critical fields
					if (it.key() == QLatin1String("model") ||
						it.key() == QLatin1String("stream") ||
						it.key() == QLatin1String("messages") ||
						it.key() == QLatin1String("tools") ||
						it.key() == QLatin1String("text"))
						continue;
					body[it.key()] = it.value();
				}
			}
		}

		return QJsonDocument(body).toJson(QJsonDocument::Compact);
}

QStringList AiBridge::effectiveToolsForActivePersona() const
{
	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	if (!pm || pm->count() == 0)
		return pm ? pm->defaultToolSet() : QStringList();
	return pm->effectiveEnabledTools(pm->currentPersonaIndex());
}

// =============================================================================
// SSE stream parsing
// =============================================================================

void AiBridge::onReadyRead()
{
	if (!m_activeReply) return;

	// Don't buffer SSE for error responses — let onReplyFinished read the body
	int httpStatus = m_activeReply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
	if (httpStatus >= 400) return;

	m_sseBuffer.append(m_activeReply->readAll());

	// Process complete SSE lines from the buffer
	while (true) {
		int newlineIdx = m_sseBuffer.indexOf('\n');
		if (newlineIdx < 0) break;

		QByteArray line = m_sseBuffer.left(newlineIdx).trimmed();
		m_sseBuffer.remove(0, newlineIdx + 1);

		processSSELine(line);
	}
}

void AiBridge::processSSELine(const QByteArray &line)
{
	if (line.isEmpty()) return;

	// SSE lines are "field:value"
	if (line.startsWith("data:")) {
		QByteArray data = line.mid(5).trimmed(); // skip "data:"

		if (data == "[DONE]") {
			// Stream completed normally
			return;
		}

		processSSEData(QStringLiteral("data"), data);
	}
	else if (line.startsWith("event:")) {
		// Some SSE implementations use event: to distinguish message types.
		// Log it so we can see if the provider sends unexpected event types.
		QByteArray eventType = line.mid(6).trimmed();
		Log::log() << "AiBridge: SSE event type: " << eventType.toStdString() << std::endl;
	}
	else if (line.contains(':')) {
		int colonIdx = line.indexOf(':');
		QByteArray field = line.left(colonIdx).trimmed();
		QByteArray value = line.mid(colonIdx + 1).trimmed();
		processSSEData(QString::fromUtf8(field), value);
	}
	else {
		// Line doesn't match any expected SSE pattern
		Log::log() << "AiBridge: DISCARDED unrecognised SSE line: " << line.toStdString() << std::endl;
	}
}

void AiBridge::processSSEData(const QString &eventType, const QByteArray &data)
{
	Q_UNUSED(eventType)

	QJsonParseError parseError;
	QJsonDocument doc = QJsonDocument::fromJson(data, &parseError);

	if (parseError.error != QJsonParseError::NoError) {
		Log::log() << "AiBridge: Failed to parse SSE data: " << parseError.errorString().toStdString() << std::endl;
		return;
	}

	QJsonObject obj = doc.object();


	// Check for error
	if (obj.contains(QStringLiteral("error"))) {
		QJsonObject err = obj[QStringLiteral("error")].toObject();
		QString errMsg = err[QStringLiteral("message")].toString(QStringLiteral("Unknown API error"));
		emitError(errMsg);
		return;
	}

	// Check for tool calls (OpenAI/DeepSeek format)
	QJsonArray choices = obj[QStringLiteral("choices")].toArray();
	if (choices.isEmpty()) {
		// Providers may send chunks without choices (usage metadata, etc.).
		// Log other keys so we can see what's being sent.
		QString keyList;
		for (auto it = obj.begin(); it != obj.end(); ++it) {
			if (!keyList.isEmpty()) keyList += QStringLiteral(", ");
			keyList += it.key();
		}
		if (!keyList.isEmpty())
			Log::log() << "AiBridge: SSE chunk with no choices — keys: " << keyList.toStdString() << std::endl;
		else
			Log::log() << "AiBridge: DISCARDED empty SSE data object (no choices, no keys)" << std::endl;
		return;
	}

	QJsonObject choice = choices.first().toObject();
	QJsonObject delta = choice[QStringLiteral("delta")].toObject();

	// --- Tool calls ---
	if (delta.contains(QStringLiteral("tool_calls"))) {
		QJsonArray toolCalls = delta[QStringLiteral("tool_calls")].toArray();
		if (!toolCalls.isEmpty()) {
			processToolCalls(toolCalls);
		}
		return;
	}

	// --- Generic delta merge: accumulate ALL fields the provider emits ---
	// String fields are concatenated (they stream in fragments);
	// non-string fields (role, etc.) overwrite. Nulls are skipped.
	for (auto it = delta.begin(); it != delta.end(); ++it) {
		const QString key = it.key();
		const QJsonValue val = it.value();

		// Emit text content chunks to the frontend
		if (key == QStringLiteral("content") && val.isString()) {
			QString text = val.toString();
			if (!text.isEmpty()) {
				m_totalStreamChunks++;
				emit conversationStatsUpdated();
				emit onStreamChunk(text);
			}
		}

		// Merge into accumulator
		if (val.isString() && m_assistantDelta.contains(key)) {
			// Concatenate streaming string fragments
			// (covers content, reasoning_content, refusal, thinking, citation, etc.)
			m_assistantDelta[key] = m_assistantDelta[key].toString() + val.toString();
		} else if (!val.isNull()) {
			m_assistantDelta[key] = val;
		}
	}
}

// =============================================================================
// Tool-call handling — delta accumulation
// =============================================================================

void AiBridge::processToolCalls(const QJsonArray &toolCalls)
{
	// Merge each delta into the accumulator keyed by index.
	//
	// NOTE: Gemini's OpenAI-compatible endpoint does NOT include an "index"
	// field inside streaming tool_call delta chunks (confirmed bug as of
	// Nov 2025).  We therefore fall back: first try to match an existing
	// accumulator entry by "id", then fall back to the next available slot.
	int nextAutoIdx = m_toolCallAccum.isEmpty() ? 0 : (m_toolCallAccum.lastKey() + 1);

	for (const QJsonValue &val : toolCalls) {
		QJsonObject delta = val.toObject();
		int idx = delta[QStringLiteral("index")].toInt(-1);

		// --- Fallback for providers that omit "index" (Gemini, etc.) ---
		if (idx < 0) {
			QString fallbackId = delta[QStringLiteral("id")].toString();
			if (!fallbackId.isEmpty()) {
				// Try to find an existing accumulator entry with this id
				bool found = false;
				for (auto it = m_toolCallAccum.begin(); it != m_toolCallAccum.end(); ++it) {
					if (it.value()[QStringLiteral("id")].toString() == fallbackId) {
						idx = it.key();
						found = true;
						break;
					}
				}
				if (!found) {
					idx = nextAutoIdx++;
					Log::log() << "AiBridge: tool call missing 'index' — assigned #" << idx
					           << " (id=" << fallbackId.toStdString() << ")" << std::endl;
				}
			} else {
				// No id either — skip this fragment (shouldn't happen)
				Log::log() << "AiBridge: DISCARDED tool call fragment missing both 'index' and 'id': "
				           << QJsonDocument(delta).toJson(QJsonDocument::Compact).toStdString() << std::endl;
				continue;
			}
		}

		QJsonObject &acc = m_toolCallAccum[idx];

		bool firstFragment = !acc.contains(QStringLiteral("id"));

		// Shallow-merge: copy all keys from delta into accumulator
		for (auto it = delta.begin(); it != delta.end(); ++it) {
			if (it.key() == QStringLiteral("function")) {
				// Deep-merge function object — concatenate arguments fragments
				QJsonObject funcAcc = acc[QStringLiteral("function")].toObject();
				QJsonObject funcDelta = it.value().toObject();
				for (auto fit = funcDelta.begin(); fit != funcDelta.end(); ++fit) {
					if (fit.key() == QStringLiteral("arguments"))
						funcAcc[fit.key()] = funcAcc[fit.key()].toString() + fit.value().toString();
					else
						funcAcc[fit.key()] = fit.value();
				}
				acc[QStringLiteral("function")] = funcAcc;
			} else {
				acc[it.key()] = it.value();
			}
		}

		// One-line log per tool call on first fragment only
		if (firstFragment) {
			QString funcName = acc[QStringLiteral("function")].toObject()[QStringLiteral("name")].toString();
			Log::log() << "AiBridge: tool call #" << idx
			           << (funcName.isEmpty() ? "" : " — " + funcName.toStdString())
			           << std::endl;
		}
	}
}

// Called from onReplyFinished when the stream ends with tool calls.
//
// Re-entrancy is now enforced by JaspRpcDispatcher::dispatch() via its
// m_inFlight flag.  See jasprpcdispatcher.h for details.
void AiBridge::flushToolCalls()
{
	if (m_toolCallAccum.isEmpty()) return;

	QJsonArray tcArray;  // all tool calls for the assistant message

	for (auto it = m_toolCallAccum.begin(); it != m_toolCallAccum.end(); ++it) {
		QJsonObject tc = it.value();

		// Normalize arguments
		QJsonObject funcObj = tc[QStringLiteral("function")].toObject();
		QString argumentsStr = funcObj[QStringLiteral("arguments")].toString();
		if (argumentsStr.isEmpty() || argumentsStr == QStringLiteral(""""))
			argumentsStr = QStringLiteral("{}");
		funcObj[QStringLiteral("arguments")] = argumentsStr;
		tc[QStringLiteral("function")] = funcObj;

		QString functionName = funcObj[QStringLiteral("name")].toString();
		if (functionName.isEmpty()) continue;

		// Security gate: refuse dispatch if the active persona doesn't allow this tool.
		// The AI is only *shown* allowed tools via buildRequestBody(), but we check
		// here as a defence-in-depth measure in case a model hallucinates or a provider
		// misbehaves.
		QString toolResultText;
		const QStringList allowedTools = effectiveToolsForActivePersona();
		const QSet<QString> allowedSet(allowedTools.begin(), allowedTools.end());
		if (!allowedSet.contains(functionName))
		{
			Log::log() << "AiBridge: BLOCKED tool [" << functionName.toStdString()
					   << "] — not in active persona's allowed-tool set" << std::endl;
			toolResultText = QStringLiteral("Error: Tool '") + functionName
				+ QStringLiteral("' is not available for the current persona.");
		}
		else
		{
			Log::log() << "AiBridge: Dispatching tool [" << functionName.toStdString()
						<< "] with args: " << argumentsStr.toStdString() << std::endl;

			// Dispatch through JASP RPC
			JaspRpcDispatcher *disp = JaspRpcDispatcher::singleton();
			if (!disp) { Log::log() << "AiBridge: No RPC dispatcher!" << std::endl; emitError("RPC dispatcher not available"); return; }

			QJsonParseError argParseError;
			QJsonDocument argDoc = QJsonDocument::fromJson(argumentsStr.toUtf8(), &argParseError);
			Json::Value params;
			if (argParseError.error == QJsonParseError::NoError && argDoc.isObject()) {
				std::string s = QJsonDocument(argDoc.object()).toJson(QJsonDocument::Compact).toStdString();
				Json::Reader r; r.parse(s, params);
			}

			std::string requestJson = R"({"jsonrpc":"2.0","method":")" + functionName.toStdString() + R"(","params":)" +
									  (params.isObject() ? params.toStyledString() : "{}") + "}";
			std::string resultJson = disp->dispatch(requestJson);
			Json::Value result; Json::Reader r; r.parse(resultJson, result);

			if (result.isMember("result")) { Json::FastWriter w; toolResultText = QString::fromStdString(w.write(result["result"])); }
			else if (result.isMember("error")) { toolResultText = QStringLiteral("Error: ") + QString::fromStdString(result["error"].toStyledString()); }
			else { toolResultText = QString::fromStdString(resultJson); }

			m_totalToolCallsDispatched++;
			logToolCall(tc, toolResultText);
		}

		// Build the tool_call entry — copy ALL accumulated fields (reasoning_content etc)
		QJsonObject tcObj = tc;
		tcObj.remove(QStringLiteral("index"));
		tcArray.append(tcObj);

		// Append the tool result message
		QJsonObject toolMsg;
		toolMsg[QStringLiteral("role")] = QStringLiteral("tool");
		toolMsg[QStringLiteral("tool_call_id")] = tc[QStringLiteral("id")];
		toolMsg[QStringLiteral("content")] = toolResultText;
		m_pendingToolCalls.append(tc);
		m_conversation.append(toolMsg);
	}

	// Prepend ONE assistant message with all tool_calls
	if (!tcArray.isEmpty()) {
		QJsonObject assistantMsg = m_assistantDelta;
		assistantMsg[QStringLiteral("role")] = QStringLiteral("assistant");
		assistantMsg[QStringLiteral("tool_calls")] = tcArray;
		m_assistantDelta = QJsonObject();
		int insertPos = m_conversation.size() - tcArray.size();
		m_conversation.insert(insertPos, assistantMsg);
	}

	m_toolCallAccum.clear();
}

void AiBridge::continueWithToolResults(const QJsonArray &toolResults)
{
	Q_UNUSED(toolResults)
	// This is a hook for future use — e.g., batching tool results before continuing
}

// =============================================================================
// Reply handlers
// =============================================================================

void AiBridge::onReplyFinished()
{
	if (!m_activeReply) return;

	int httpStatus = m_activeReply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
	QByteArray body = m_activeReply->readAll();

	// Network-level errors (connection refused, timeout, host not found, etc.)
	// are already handled in onReplyError — just clean up here.
	if (m_activeReply->error() != QNetworkReply::NoError) {
		m_activeReply->deleteLater();
		m_activeReply = nullptr;
		m_streaming = false;
		emit onStreamClose();
		return;
	}

	// Handle HTTP errors
	if (httpStatus >= 400) {
		Log::log() << "AiBridge: HTTP " << httpStatus << " error body: " << body.toStdString() << std::endl;
		emitError(QStringLiteral("HTTP ") + QString::number(httpStatus) + QStringLiteral(": ") + QString::fromUtf8(body));
		m_activeReply->deleteLater();
		m_activeReply = nullptr;
		m_streaming = false;
		emit onStreamClose();
		return;
	}

	// Process any remaining SSE data from the buffer + just-read body
	m_sseBuffer.append(body);
	while (m_sseBuffer.contains('\n')) {
		int idx = m_sseBuffer.indexOf('\n');
		QByteArray line = m_sseBuffer.left(idx).trimmed();
		m_sseBuffer.remove(0, idx + 1);
		if (!line.isEmpty()) processSSELine(line);
	}

	m_activeReply->deleteLater();
	m_activeReply = nullptr;

	// Flush accumulated tool call deltas before continuing
	flushToolCalls();

	// If tool calls arrived during this stream, continue the loop
	if (!m_pendingToolCalls.isEmpty()) {
		int loopTokens = estimateTokens(m_conversation);
		Log::log() << "AiBridge: Stream ended with " << m_pendingToolCalls.size()
				<< " tool call(s), continuing loop — conversation now "
				<< m_conversation.size() << " messages (~" << loopTokens << " tokens)"
				<< std::endl;
		m_pendingToolCalls = QJsonArray();
		m_streaming = true;
		emit onStreamOpen();
		sendToAI(m_conversation);
		return;
	}

	// Save assistant response into conversation history
	if (!m_assistantDelta.isEmpty()) {
		int outTokens = estimateTokens(m_assistantDelta);
		m_totalOutputTokens += outTokens;

		QJsonObject asst = m_assistantDelta;
		asst[QStringLiteral("role")] = QStringLiteral("assistant");
		m_conversation.append(asst);
		m_assistantDelta = QJsonObject();
	}
	m_streaming = false;
	emit onStreamClose();

	Log::log() << "AiBridge: Stream finished — " << m_totalStreamChunks << " chunks this session" << std::endl;
	logConversationStats("streamEnd");
}

void AiBridge::onReplyError(QNetworkReply::NetworkError error)
{
	Log::log() << "AiBridge::onReplyError code=" << (int)error << std::endl;

	if (error == QNetworkReply::NoError) return;

	// onReplyFinished will handle the cleanup after this.
	emitError(networkErrorToString(error, m_activeReply));
}

// =============================================================================
// Helpers
// =============================================================================

QString AiBridge::networkErrorToString(QNetworkReply::NetworkError error, QNetworkReply *reply)
{
	switch (error) {
	case QNetworkReply::ConnectionRefusedError:
		return QStringLiteral("Connection refused — the AI service may be unavailable.");
	case QNetworkReply::RemoteHostClosedError:
		return QStringLiteral("Connection closed unexpectedly by the AI service.");
	case QNetworkReply::HostNotFoundError:
		return QStringLiteral("AI service host not found — check your endpoint URL.");
	case QNetworkReply::TimeoutError:
		return QStringLiteral("Request timed out — the AI service did not respond in time.");
	case QNetworkReply::SslHandshakeFailedError:
		return QStringLiteral("SSL/TLS handshake failed — check your certificate or endpoint URL.");
	case QNetworkReply::ProxyConnectionRefusedError:
		return QStringLiteral("Proxy connection refused — check your proxy settings.");
	case QNetworkReply::ProxyConnectionClosedError:
		return QStringLiteral("Proxy connection closed unexpectedly.");
	case QNetworkReply::ProxyNotFoundError:
		return QStringLiteral("Proxy not found — check your proxy settings.");
	case QNetworkReply::ProxyAuthenticationRequiredError:
		return QStringLiteral("Proxy authentication required — check your proxy credentials.");
	case QNetworkReply::ContentAccessDenied:
		return QStringLiteral("Access to the AI service was denied (HTTP 403).");
	case QNetworkReply::ContentNotFoundError:
		return QStringLiteral("AI service endpoint not found (HTTP 404) — check your endpoint URL.");
	case QNetworkReply::AuthenticationRequiredError:
		return QStringLiteral("Authentication required — check your API key.");
	default:
		return reply ? reply->errorString() : QStringLiteral("An unknown network error occurred.");
	}
}

void AiBridge::emitError(const QString &message)
{
	Log::log() << "AiBridge ERROR: " << message.toStdString() << std::endl;
	emit onStreamError(message);
}

void AiBridge::testConnection()
{
	QString ep = endpoint();
	if (ep.isEmpty()) {
		emit testConnectionResult(false, QStringLiteral("No endpoint configured."));
		return;
	}

	Log::log() << "AiBridge::testConnection — probing " << ep.toStdString() << std::endl;

	QNetworkRequest request{QUrl(ep)};
	request.setHeader(QNetworkRequest::ContentTypeHeader, QStringLiteral("application/json"));
	request.setTransferTimeout(10000); // 10 s for a test

	QString token = authToken();
	if (!token.isEmpty())
		request.setRawHeader("Authorization", ("Bearer " + token).toUtf8());

	// Build a minimal valid body — just enough to provoke a meaningful response
	QJsonObject body;
	body[QStringLiteral("model")] = model().isEmpty() ? QStringLiteral("gpt-3.5-turbo") : model();
	body[QStringLiteral("stream")] = false;
	body[QStringLiteral("max_tokens")] = 1;
	QJsonArray msgs;
	QJsonObject msg;
	msg[QStringLiteral("role")] = QStringLiteral("user");
	msg[QStringLiteral("content")] = QStringLiteral("Hi");
	msgs.append(msg);
	body[QStringLiteral("messages")] = msgs;

	QNetworkReply *reply = m_networkManager->post(request, QJsonDocument(body).toJson(QJsonDocument::Compact));

	connect(reply, &QNetworkReply::finished, this, [this, reply]() {
		reply->deleteLater();

		int httpStatus = reply->attribute(QNetworkRequest::HttpStatusCodeAttribute).toInt();
		QNetworkReply::NetworkError err = reply->error();

		if (err != QNetworkReply::NoError) {
			QString detail = networkErrorToString(err, reply);
			Log::log() << "AiBridge::testConnection FAILED: " << detail.toStdString() << std::endl;
			emit testConnectionResult(false, detail);
			return;
		}

		// We got a response — even 4xx/5xx means the service is reachable
		QByteArray responseBody = reply->readAll();
		Log::log() << "AiBridge::testConnection OK  HTTP " << httpStatus << std::endl;

		QString msg;
		if (httpStatus == 200 || httpStatus == 201) {
			msg = QStringLiteral("Connection successful (HTTP ") + QString::number(httpStatus) + QStringLiteral(").");
		} else if (httpStatus >= 400 && httpStatus < 500) {
			msg = QStringLiteral("Service reachable (HTTP ") + QString::number(httpStatus)
				+ QStringLiteral(") — check your API key or model name.");
		} else if (httpStatus >= 500) {
			msg = QStringLiteral("Service reachable but returned server error (HTTP ")
				+ QString::number(httpStatus) + QStringLiteral(").");
		} else {
			msg = QStringLiteral("Connection successful (HTTP ") + QString::number(httpStatus) + QStringLiteral(").");
		}

		emit testConnectionResult(true, msg);
	});
}

// =============================================================================
// Token estimation (approximate — ~1 token per 4 chars for English text)
// =============================================================================

int AiBridge::estimateTokens(const QString &text)
{
	// Rough heuristic: 1 token ≈ 4 characters for English text.
	// GPT tokenizers average ~1.3 tokens/word, ~4 chars/token.
	if (text.isEmpty()) return 0;
	return qMax(1, text.length() / 4);
}

int AiBridge::estimateTokens(const QJsonObject &obj)
{
	return estimateTokens(QString::fromUtf8(QJsonDocument(obj).toJson(QJsonDocument::Compact)));
}

int AiBridge::estimateTokens(const QJsonArray &arr)
{
	return estimateTokens(QString::fromUtf8(QJsonDocument(arr).toJson(QJsonDocument::Compact)));
}

int AiBridge::estimateTokens(const QJsonValue &val)
{
	if (val.isString())
		return estimateTokens(val.toString());
	if (val.isObject())
		return estimateTokens(val.toObject());
	if (val.isArray())
		return estimateTokens(val.toArray());
	// Numbers, bools, null → ~1 token
	return val.isUndefined() || val.isNull() ? 0 : 1;
}

void AiBridge::logConversationStats(const char *context) const
{
	int totalTokens = estimateTokens(m_conversation);
	AIPersonaModel *pm = PreferencesModel::prefs()->aiPersonaModel();
	int sysTokens = pm ? estimateTokens(pm->activePersona().personaPrompt) : 0;

	// Count messages by role
	QMap<QString, int> roleCounts;
	for (const QJsonValue &v : m_conversation) {
		QJsonObject msg = v.toObject();
		QString role = msg[QStringLiteral("role")].toString(QStringLiteral("unknown"));
		roleCounts[role]++;
	}

	QString roleBreakdown;
	for (auto it = roleCounts.begin(); it != roleCounts.end(); ++it) {
		if (!roleBreakdown.isEmpty()) roleBreakdown += QStringLiteral(", ");
		roleBreakdown += it.key() + QStringLiteral(":") + QString::number(it.value());
	}

	Log::log() << "AiBridge CONVERSATION STATS [" << context << "]"
	           << " | messages: " << m_conversation.size()
	           << " | roles: {" << roleBreakdown.toStdString() << "}"
	           << " | ~" << totalTokens << " tokens in conversation"
	           << " | +~" << sysTokens << " tokens system prompt"
	           << " | session: ~" << m_totalInputTokens << " in + ~" << m_totalOutputTokens << " out = ~" << (m_totalInputTokens + m_totalOutputTokens) << " total"
	           << " | requests: " << m_totalRequestsSent
	           << " | tool calls: " << m_totalToolCallsDispatched
	           << " | stream chunks: " << m_totalStreamChunks
	           << std::endl;
}

void AiBridge::logToolCall(const QJsonObject &toolCall, const QString &resultText) const
{
	QJsonObject funcObj = toolCall[QStringLiteral("function")].toObject();
	QString funcName = funcObj[QStringLiteral("name")].toString();
	QString callId = toolCall[QStringLiteral("id")].toString();

	// Truncate result for logging (max 500 chars)
	QString truncated = resultText;
	if (truncated.length() > 500)
		truncated = truncated.left(500) + QStringLiteral("... [truncated, total ") + QString::number(resultText.length()) + QStringLiteral(" chars]");

	Log::log() << "AiBridge TOOL RESULT [" << m_totalToolCallsDispatched << "]"
	           << " | id: " << callId.toStdString()
	           << " | function: " << funcName.toStdString()
	           << " | result (~" << estimateTokens(resultText) << " tokens): "
	           << truncated.toStdString()
	           << std::endl;
}
