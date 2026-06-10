#include "aipersonamodel.h"
#include "dirs.h"
#include "jasptheme.h"
#include "utilities/settings.h"
#include "utilities/appdirs.h"
#include "utilities/qutils.h"
#include "rpc/jasprpcdispatcher.h"
#include "log.h"

#include <QFile>
#include <QDir>
#include <QMap>
#include <QSet>
#include <QJsonDocument>
#include <QJsonArray>
#include <QJsonObject>
#include <QUrl>
#include <QMimeDatabase>

// Forward declarations — static helpers defined later in this file
static QStringList resolveCapabilitiesToTools(const QJsonArray &capsArr);
static QStringList resolveCaps(const QStringList &tools);
static QJsonArray toJsonArr(const QStringList &ids);

// ============================================================================
// Construction
// ============================================================================

AIPersonaModel::AIPersonaModel(QObject *parent)
	: QAbstractListModel(parent)
{
	loadFromSettings();
}

// ============================================================================
// QAbstractListModel interface
// ============================================================================

int AIPersonaModel::rowCount(const QModelIndex &) const
{
	return m_personas.size();
}

QVariant AIPersonaModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid() || index.row() < 0 || index.row() >= m_personas.size())
		return {};

	const PersonaEntry &p = m_personas.at(index.row());
	switch (role) {
	case IdRole:             return p.id;
	case NameRole:           return p.name;
	case PersonaPromptRole:   return p.personaPrompt;
	case ImagePathRole:      return p.imagePath;
	case IsSystemRole:       return p.isSystem;
	case EnabledToolsRole:   return p.enabledTools;
	case EnabledCapabilitiesRole: return p.enabledCapabilities;
	default:                 return {};
	}
}

bool AIPersonaModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (!index.isValid() || index.row() < 0 || index.row() >= m_personas.size())
		return false;

	PersonaEntry &merged = m_personas[index.row()];

	// Find or create the user-side entry (all edits go through m_userPersonas)
	auto it = std::find_if(m_userPersonas.begin(), m_userPersonas.end(),
		[&](const PersonaEntry &u) { return u.id == merged.id; });
	if (it == m_userPersonas.end()) {
		PersonaEntry entry = merged;
		entry.isSystem = false;
		m_userPersonas.append(entry);
		it = m_userPersonas.end() - 1;
	}

	switch (role) {
	case NameRole:
		if (value.toString() == it->name) return true;
		it->name = value.toString();
		merged.name = it->name;
		break;
	case PersonaPromptRole:
		if (value.toString() == it->personaPrompt) return true;
		it->personaPrompt = value.toString();
		merged.personaPrompt = it->personaPrompt;
		break;
	case ImagePathRole:
		if (value.toString() == it->imagePath) return true;
		it->imagePath = value.toString();
		merged.imagePath = it->imagePath;
		if (index.row() == m_currentPersonaIndex)
			emit activePersonaAvatarChanged();
		break;
	case EnabledToolsRole:
	{
		QStringList tools = value.toStringList();
		tools.sort();
		if (tools == it->enabledTools) return true;

		// Store tools and derive caps
		it->enabledTools = tools;
		merged.enabledTools = tools;
		it->enabledCapabilities = resolveCaps(tools);
		merged.enabledCapabilities = it->enabledCapabilities;
		break;
	}
	case EnabledCapabilitiesRole:
	{
		QStringList caps = value.toStringList();
		caps.sort();
		if (caps == it->enabledCapabilities) return true;
		QStringList tools = resolveCapabilitiesToTools(toJsonArr(caps));
		tools.sort();
		it->enabledTools = tools;
		it->enabledCapabilities = caps;
		merged.enabledTools = tools;
		merged.enabledCapabilities = caps;
		break;
	}
	default:
		return false;
	}

	saveToSettings();

	emit dataChanged(index, index, {role});
	switch (role) {
	case NameRole:           emit personaNameChanged(index.row(), it->name); break;
	case PersonaPromptRole:   emit personaPromptChanged(index.row(), it->personaPrompt); break;
	case ImagePathRole:      emit personaImagePathChanged(index.row(), it->imagePath); break;
	case EnabledToolsRole:   emit personaEnabledToolsChanged(index.row(), it->enabledTools);
							emit personaEnabledCapabilitiesChanged(index.row(), it->enabledCapabilities); break;
	default: break;
	}
	return true;
}

QHash<int, QByteArray> AIPersonaModel::roleNames() const
{
	return {
		{ IdRole,             "personaId" },
		{ NameRole,           "personaName" },
		{ PersonaPromptRole,   "personaPrompt" },
		{ ImagePathRole,      "personaImagePath" },
		{ IsSystemRole,       "personaIsSystem" },
		{ EnabledToolsRole,   "personaEnabledTools" },
		{ EnabledCapabilitiesRole, "personaEnabledCapabilities" }
	};
}

// ============================================================================
// Persona CRUD
// ============================================================================

int AIPersonaModel::addPersona()
{
	PersonaEntry p;
	p.id          = QUuid::createUuid().toString(QUuid::WithoutBraces);
	p.name        = QStringLiteral("New Persona");
	p.personaPrompt = QStringLiteral("");
	p.imagePath   = QStringLiteral("");
	p.isSystem    = false;

	m_userPersonas.append(p);
	mergeLists();
	saveToSettings();

	return indexOfId(p.id);
}

void AIPersonaModel::removePersona(int index)
{
	if (index < 0 || index >= m_personas.size()) return;
	const PersonaEntry &p = m_personas.at(index);
	if (p.isSystem) return; // cannot delete system personas

	m_userPersonas.erase(
		std::remove_if(m_userPersonas.begin(), m_userPersonas.end(),
			[&](const PersonaEntry &u) { return u.id == p.id; }),
		m_userPersonas.end());

	mergeLists();

	if (m_currentPersonaIndex >= m_personas.size())
		setCurrentPersonaIndex(m_personas.size() - 1);

	saveToSettings();
}

void AIPersonaModel::duplicatePersona(int index)
{
	if (index < 0 || index >= m_personas.size()) return;

	PersonaEntry p = m_personas.at(index);
	p.id          = QUuid::createUuid().toString(QUuid::WithoutBraces);
	p.name        = p.name + QStringLiteral(" (copy)");
	p.isSystem    = false;

	m_userPersonas.append(p);
	mergeLists();

	int newIdx = indexOfId(p.id);
	setCurrentPersonaIndex(newIdx);
	saveToSettings();
}

void AIPersonaModel::resetSystemPersona(int index)
{
	if (index < 0 || index >= m_personas.size()) return;
	PersonaEntry &merged = m_personas[index];
	if (!merged.isSystem) return;
	QString personaId = merged.id;

	// Remove any user-side override with matching ID
	m_userPersonas.erase(
		std::remove_if(m_userPersonas.begin(), m_userPersonas.end(),
			[&](const PersonaEntry &u) { return u.id == personaId; }),
		m_userPersonas.end());

	// Restore from original system persona (no model reset)
	auto it = std::find_if(m_systemPersonas.begin(), m_systemPersonas.end(),
		[&](const PersonaEntry &s) { return s.id == personaId; });
	if (it != m_systemPersonas.end()) {
		merged.name = it->name;
		merged.personaPrompt = it->personaPrompt;
		merged.imagePath = it->imagePath;
		merged.enabledTools = it->enabledTools;
		merged.enabledCapabilities = it->enabledCapabilities;
	} else {
		merged.enabledTools.clear();
		merged.enabledCapabilities.clear();
	}

	saveToSettings();
	emit dataChanged(this->index(index, 0), this->index(index, 0));
	emit personaNameChanged(index, merged.name);
	emit personaPromptChanged(index, merged.personaPrompt);
	emit personaImagePathChanged(index, merged.imagePath);
	emit personaEnabledToolsChanged(index, merged.enabledTools);
	emit personaEnabledCapabilitiesChanged(index, merged.enabledCapabilities);
}

// ============================================================================
// Image management
// ============================================================================

QString AIPersonaModel::personaImagesDir() const
{
	QString dir = AppDirs::appData() + "/personas";
	QDir().mkpath(dir);
	return dir;
}

QString AIPersonaModel::copyImageToPersonasDir(const QUrl &sourceUrl)
{
	if (!sourceUrl.isLocalFile()) return {};

	const QString srcPath = sourceUrl.toLocalFile();
	QFileInfo fi(srcPath);
	if (!fi.exists() || !fi.isFile()) return {};

	// Generate a unique filename in the personas directory
	const QString suffix = fi.suffix().isEmpty() ? QStringLiteral("png") : fi.suffix();
	const QString uniqueName = QUuid::createUuid().toString(QUuid::WithoutBraces) + "." + suffix;
	const QString destPath = personaImagesDir() + "/" + uniqueName;

	if (QFile::exists(destPath))
		QFile::remove(destPath);

	if (QFile::copy(srcPath, destPath))
		return QDir::toNativeSeparators(destPath);

	Log::log() << "AIPersonaModel: failed to copy image " << srcPath << " to " << destPath << std::endl;
	return {};
}

QString AIPersonaModel::defaultPersonaImagePath() const
{
	return JaspTheme::currentIconPath() + "jaspAI.png";
}

QUrl AIPersonaModel::resolvedImageUrl(const QString &imagePath) const
{
	if (imagePath.isEmpty())
		return QUrl(defaultPersonaImagePath());

	if (QFile::exists(imagePath))
		return QUrl::fromLocalFile(imagePath);

	return QUrl(defaultPersonaImagePath());
}

// ============================================================================
// Accessors
// ============================================================================

const PersonaEntry &AIPersonaModel::activePersona() const
{
	static const PersonaEntry emptyEntry;
	if (m_currentPersonaIndex >= 0 && m_currentPersonaIndex < m_personas.size())
		return m_personas.at(m_currentPersonaIndex);
	return emptyEntry;
}

int AIPersonaModel::count() const
{
	return m_personas.size();
}


int AIPersonaModel::currentPersonaIndex() const
{
	return m_currentPersonaIndex;
}

QString AIPersonaModel::activePersonaAvatar() const
{
	const PersonaEntry &p = activePersona();
	if (p.imagePath.isEmpty())
		return {};

	QFileInfo fi(p.imagePath);
	if (!fi.exists())
		return {};

	return QUrl::fromLocalFile(p.imagePath).toString();
}

QString AIPersonaModel::activePersonaAvatarWeb() const
{
	const PersonaEntry &p = activePersona();
	if (p.imagePath.isEmpty())
		return {};

	QFileInfo fi(p.imagePath);
	if (!fi.exists())
		return {};

	return QStringLiteral("jaspPersona:///") + fi.fileName();
}

void AIPersonaModel::setCurrentPersonaIndex(int index)
{
	if (index < 0) index = 0;
	if (index >= m_personas.size()) index = m_personas.size() - 1;
	if (index == m_currentPersonaIndex) return;

	m_currentPersonaIndex = index;
	emit currentPersonaIndexChanged();
	emit activePersonaAvatarChanged();

	// Persist the active persona by UUID
	if (index >= 0 && index < m_personas.size())
		Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, m_personas.at(index).id);
	else
		Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, QString());
}

// ============================================================================
// Helper — resolve capability IDs to tool names
// ============================================================================

static QStringList resolveCapabilitiesToTools(const QJsonArray &capsArr)
{
	// "*" means all capabilities → all tools
	for (const QJsonValue &v : capsArr)
		if (v.isString() && v.toString() == QStringLiteral("*"))
			return {QStringLiteral("*")};

	// Load JASP_Capabilities.json to map cap IDs → methods
	std::string path = Dirs::resourcesDir() + "JASP_Capabilities.json";
	QFile file(tq(path));
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
		return {};

	QJsonParseError err;
	QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
	file.close();
	if (err.error != QJsonParseError::NoError || !doc.isObject())
		return {};

	// Build map: capability id → methods
	QMap<QString, QStringList> capMap;
	QJsonArray caps = doc.object().value(QStringLiteral("capabilities")).toArray();
	for (const QJsonValue &cv : caps)
	{
		if (!cv.isObject()) continue;
		QJsonObject co = cv.toObject();
		QStringList methods;
		for (const QJsonValue &mv : co.value(QStringLiteral("methods")).toArray())
			if (mv.isString()) methods.append(mv.toString());
		capMap[co.value(QStringLiteral("id")).toString()] = methods;
	}

	// Collect methods for the requested capabilities
	QSet<QString> resolved;
	for (const QJsonValue &v : capsArr)
	{
		if (!v.isString()) continue;
		auto it = capMap.find(v.toString());
		if (it != capMap.end())
			for (const QString &m : *it)
				resolved.insert(m);
	}

	return QStringList(resolved.begin(), resolved.end());
}

/// Convert QStringList to QJsonArray
static QJsonArray toJsonArr(const QStringList &ids)
{
	QJsonArray arr;
	for (const QString &id : ids)
		arr.append(id);
	return arr;
}

/// Given tools, compute which caps are fully covered (reads JASP_Capabilities.json)
static QStringList resolveCaps(const QStringList &tools)
{
	if (tools.isEmpty()) return {};

	std::string path = Dirs::resourcesDir() + "JASP_Capabilities.json";
	QFile file(tq(path));
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
		return {};

	QJsonParseError err;
	QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
	file.close();
	if (err.error != QJsonParseError::NoError || !doc.isObject())
		return {};

	QSet<QString> toolSet(tools.begin(), tools.end());
	QStringList covered;
	QJsonArray caps = doc.object().value(QStringLiteral("capabilities")).toArray();
	for (const QJsonValue &cv : caps)
	{
		if (!cv.isObject()) continue;
		QJsonObject co = cv.toObject();
		QJsonArray methods = co.value(QStringLiteral("methods")).toArray();
		if (methods.isEmpty()) continue;

		bool allPresent = true;
		for (const QJsonValue &mv : methods)
		{
			if (!mv.isString() || !toolSet.contains(mv.toString())) {
				allPresent = false;
				break;
			}
		}
		if (allPresent)
			covered.append(co.value(QStringLiteral("id")).toString());
	}
	covered.sort();
	return covered;
}

// ============================================================================
// Persistence
// ============================================================================

void AIPersonaModel::loadFromSettings()
{
	if (m_loaded) return;
	m_loaded = true;

	// 1. Load system personas from the shipped JSON file.
	std::string sysFilePath = Dirs::resourcesDir() + "defaultPersonas.json";
	QFile sysFile(tq(sysFilePath));
	if (sysFile.open(QIODevice::ReadOnly | QIODevice::Text))
	{
		QJsonParseError parseError;
		QJsonDocument doc = QJsonDocument::fromJson(sysFile.readAll(), &parseError);
		sysFile.close();

		if (doc.isArray())
		{
			for (const QJsonValue &val : doc.array())
			{
				QJsonObject obj = val.toObject();
				PersonaEntry entry;
				entry.id           = obj.value(QStringLiteral("id")).toString();
				entry.name         = obj.value(QStringLiteral("name")).toString();
				entry.personaPrompt = obj.value(QStringLiteral("personaPrompt")).toString();
				// Backward compat: fall back to old key name
				if (entry.personaPrompt.isEmpty())
					entry.personaPrompt = obj.value(QStringLiteral("systemPrompt")).toString();
				entry.imagePath    = obj.value(QStringLiteral("image")).toString();
				// Resolve relative paths for system persona images relative to resources dir
				if (!entry.imagePath.isEmpty() && !entry.imagePath.startsWith(QStringLiteral("/")) && !entry.imagePath.startsWith(QStringLiteral("qrc"))
					&& !entry.imagePath.startsWith(QStringLiteral("http")))
					entry.imagePath = tq(Dirs::resourcesDir()) + entry.imagePath;
				entry.isSystem     = true;

				// Parse enabledCapabilities (explicit cap IDs take priority)
				QJsonArray capsArr = obj.value(QStringLiteral("enabledCapabilities")).toArray();
				if (!capsArr.isEmpty()) {
					for (const QJsonValue &cv : capsArr)
						if (cv.isString()) entry.enabledCapabilities.append(cv.toString());
					entry.enabledCapabilities.sort();
				}

				// Parse enabledTools (or fall back to enabledTools / enabledCapabilities)
				QJsonArray toolsArr = obj.value(QStringLiteral("enabledTools")).toArray();
				if (toolsArr.isEmpty())
					toolsArr = capsArr; // backward compat: if no tools, try caps
				if (!toolsArr.isEmpty()) {
					entry.enabledTools = resolveCapabilitiesToTools(toolsArr);
					entry.enabledTools.sort();
				}

				// If no caps were stored but tools exist, derive caps from tools
				if (entry.enabledCapabilities.isEmpty() && !entry.enabledTools.isEmpty())
					entry.enabledCapabilities = resolveCaps(entry.enabledTools);

				if (entry.id.isEmpty())
					entry.id = QUuid::createUuid().toString(QUuid::WithoutBraces);
				if (entry.imagePath.isEmpty())
					entry.imagePath = resolveDefaultImage();

				m_systemPersonas.append(entry);
				}
			}
			else
		{
			Log::log() << "AIPersonaModel: defaultPersonas.json is not a valid JSON array." << std::endl;
		}
	}
	else
	{
		Log::log() << "AIPersonaModel: Could not open defaultPersonas.json" << std::endl;
	}

	// 2. If we couldn't load any system personas, inject a hardcoded fallback.
	if (m_systemPersonas.isEmpty())
	{
		PersonaEntry fallback;
		fallback.id           = QUuid::createUuid().toString(QUuid::WithoutBraces);
		fallback.name         = QStringLiteral("General Assistant");
		fallback.personaPrompt = QStringLiteral(
			"You are JASP AI, a helpful assistant integrated into JASP statistical software. "
			"You can help users with statistical analysis, data interpretation, and using JASP. "
			"Be concise and helpful. You are a statistical expert; run analyses with lots of options "
			"so you can give the user the best interpretations and advice.");
		fallback.imagePath    = resolveDefaultImage();
		fallback.isSystem     = true;
		m_systemPersonas.append(fallback);
	}

	// 3. Load user personas from QSettings.
	const QString userJson = Settings::value(Settings::AI_USER_PERSONAS).toString();
	if (!userJson.isEmpty())
	{
		QJsonParseError parseError;
		QJsonDocument doc = QJsonDocument::fromJson(userJson.toUtf8(), &parseError);
		if (doc.isArray())
		{
			for (const QJsonValue &val : doc.array())
			{
				QJsonObject obj = val.toObject();
				PersonaEntry entry;
				entry.id           = obj.value(QStringLiteral("id")).toString();
				entry.name         = obj.value(QStringLiteral("name")).toString();
				entry.personaPrompt = obj.value(QStringLiteral("personaPrompt")).toString();
				if (entry.personaPrompt.isEmpty())
					entry.personaPrompt = obj.value(QStringLiteral("systemPrompt")).toString();
				entry.imagePath    = obj.value(QStringLiteral("imagePath")).toString();
				entry.isSystem     = false;

				QJsonArray toolsArr = obj.value(QStringLiteral("enabledTools")).toArray();
				if (!toolsArr.isEmpty()) {
					for (const QJsonValue &tv : toolsArr)
						if (tv.isString()) entry.enabledTools.append(tv.toString());
					entry.enabledTools.sort();
				}

				// Parse enabledCapabilities (new field)
				QJsonArray capsArr = obj.value(QStringLiteral("enabledCapabilities")).toArray();
				if (!capsArr.isEmpty()) {
					for (const QJsonValue &cv : capsArr)
						if (cv.isString()) entry.enabledCapabilities.append(cv.toString());
					entry.enabledCapabilities.sort();
				}

				// Migration: if caps not stored but tools are, derive caps from tools
				if (entry.enabledCapabilities.isEmpty() && !entry.enabledTools.isEmpty())
					entry.enabledCapabilities = resolveCaps(entry.enabledTools);

				if (entry.id.isEmpty())
					entry.id = QUuid::createUuid().toString(QUuid::WithoutBraces);

				m_userPersonas.append(entry);
			}
		}
	}

	// 4. Merge into flat list.
	mergeLists();

	// 5. Restore the active persona.
	QString activeId = Settings::value(Settings::AI_CURRENT_PERSONA_ID).toString();
	int idx = indexOfId(activeId);
	if (idx < 0 && !m_personas.isEmpty())
		idx = 0;

	if (idx >= 0)
	{
		m_currentPersonaIndex = idx;
		emit currentPersonaIndexChanged();

		// Emit signals so QML refreshes even if TabBar index didn't change
		const PersonaEntry &rp = m_personas.at(idx);
		emit personaEnabledToolsChanged(idx, rp.enabledTools);
		emit personaEnabledCapabilitiesChanged(idx, rp.enabledCapabilities);
	}

	emit countChanged();
}

void AIPersonaModel::saveToSettings()
{
	QJsonArray arr;
	for (const PersonaEntry &p : m_userPersonas)
	{
		QJsonObject obj;
		obj[QStringLiteral("id")]           = p.id;
		obj[QStringLiteral("name")]         = p.name;
		obj[QStringLiteral("personaPrompt")] = p.personaPrompt;
		obj[QStringLiteral("imagePath")]    = p.imagePath;
		if (!p.enabledTools.isEmpty()) {
			QJsonArray toolsArr;
			for (const QString &t : p.enabledTools)
				toolsArr.append(t);
			obj[QStringLiteral("enabledTools")] = toolsArr;
		}
		if (!p.enabledCapabilities.isEmpty()) {
			QJsonArray capsArr;
			for (const QString &c : p.enabledCapabilities)
				capsArr.append(c);
			obj[QStringLiteral("enabledCapabilities")] = capsArr;
		}
		arr.append(obj);
	}

	QJsonDocument doc(arr);
	Settings::setValue(Settings::AI_USER_PERSONAS, QString::fromUtf8(doc.toJson(QJsonDocument::Compact)));
}

void AIPersonaModel::resetAll()
{
	m_userPersonas.clear();
	mergeLists();
	m_currentPersonaIndex = 0;
	Settings::setValue(Settings::AI_USER_PERSONAS, QStringLiteral("[]"));
	Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, m_personas.at(0).id);

	emit currentPersonaIndexChanged();
	emit countChanged();
}

// ============================================================================
// Private helpers
// ============================================================================

int AIPersonaModel::indexOfId(const QString &id) const
{
	if (id.isEmpty()) return -1;
	for (int i = 0; i < m_personas.size(); ++i)
		if (m_personas.at(i).id == id)
			return i;
	return -1;
}

void AIPersonaModel::mergeLists()
{
	beginResetModel();
	m_personas.clear();

	// Build map of user overrides keyed by persona ID
	QMap<QString, PersonaEntry> overrides;
	for (const auto &p : m_userPersonas)
		overrides.insert(p.id, p);

	// System personas first, overlaid with any matching user edits
	for (auto p : m_systemPersonas) {
		auto it = overrides.find(p.id);
		if (it != overrides.end()) {
			// Apply user's overrides but keep system flag and original ID
			p.name         = it->name;
			p.personaPrompt = it->personaPrompt;
			p.imagePath    = it->imagePath;
			p.enabledTools = it->enabledTools;
			p.enabledCapabilities = it->enabledCapabilities;
			p.isSystem     = true;  // still a system persona, just edited
		}
		m_personas.append(p);
	}

	// User-created personas (those without matching system ID)
	for (const auto &p : m_userPersonas) {
		if (!m_systemPersonas.contains(p))
			m_personas.append(p);
	}

	endResetModel();
	emit countChanged();
}

QString AIPersonaModel::resolveDefaultImage() const
{
	return {};
}

// ============================================================================
// Tool set management
// ============================================================================

QStringList AIPersonaModel::defaultToolSet() const
{
	// Default: union of all methods from all capabilities.
	QStringList capsList;
	for (const QVariant &cv : capabilities()) {
		QVariantMap cap = cv.toMap();
		capsList.append(cap.value(QStringLiteral("id")).toString());
	}
	if (capsList.isEmpty())
		return allKnownToolNames();

	// Reuse the JSON-based resolver
	QJsonArray capsArr;
	for (const QString &id : capsList)
		capsArr.append(id);
	return resolveCapabilitiesToTools(capsArr);
}

QStringList AIPersonaModel::effectiveEnabledTools(int index) const
{
	if (index < 0 || index >= m_personas.size())
		return defaultToolSet();

	const PersonaEntry &p = m_personas.at(index);

	if (p.enabledTools.isEmpty() || (p.enabledTools.size() == 1 && p.enabledTools.first() == QStringLiteral("_default_"))) {
		// System persona: check for per-persona override in defaultPersonas.json
		if (p.isSystem) {
			auto it = std::find_if(m_systemPersonas.begin(), m_systemPersonas.end(),
				[&](const PersonaEntry &s) { return s.id == p.id; });
			if (it != m_systemPersonas.end() && !it->enabledTools.isEmpty())
				return it->enabledTools;
		}
		return defaultToolSet();
	}

	if (p.enabledTools.size() == 1 && p.enabledTools.first() == QStringLiteral("*"))
		return defaultToolSet();

	if (p.enabledTools.size() == 1 && p.enabledTools.first() == QStringLiteral("_none_"))
		return {};

	return p.enabledTools;
}

QStringList AIPersonaModel::allKnownToolNames() const
{
	QStringList names;
	JaspRpcDispatcher *disp = JaspRpcDispatcher::singleton();
	if (disp) {
		for (const auto &tname : disp->knownSpecNames())
			names.append(QString::fromStdString(tname));
	}
	names.sort();
	return names;
}

QString AIPersonaModel::toolDisplayName(const QString &methodName) const
{
	JaspRpcDispatcher *disp = JaspRpcDispatcher::singleton();
	if (!disp) return methodName;
	std::string dn = disp->toolDisplayName(methodName.toStdString());
	return dn.empty() ? methodName : QString::fromStdString(dn);
}

QVariantList AIPersonaModel::capabilities() const
{
	QVariantList result;
	std::string path = Dirs::resourcesDir() + "JASP_Capabilities.json";
	QFile file(tq(path));
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
		return result;

	QJsonParseError err;
	QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
	file.close();
	if (err.error != QJsonParseError::NoError || !doc.isObject())
		return result;

	QJsonArray caps = doc.object().value(QStringLiteral("capabilities")).toArray();
	for (const QJsonValue &cv : caps)
	{
		if (!cv.isObject()) continue;
		QJsonObject co = cv.toObject();

		QVariantMap cap;
		cap[QStringLiteral("id")]          = co.value(QStringLiteral("id")).toString();
		cap[QStringLiteral("displayName")] = co.value(QStringLiteral("displayName")).toString();
		cap[QStringLiteral("description")] = co.value(QStringLiteral("description")).toString();

		QVariantList methods;
		for (const QJsonValue &mv : co.value(QStringLiteral("methods")).toArray())
			if (mv.isString()) methods.append(mv.toString());
		cap[QStringLiteral("methods")] = methods;

		result.append(cap);
	}

	return result;
}

void AIPersonaModel::useDefaultToolSet(int index)
{
	if (index < 0 || index >= m_personas.size()) return;

	// Use the resolved effective defaults as an explicit list
	QStringList defaults = defaultToolSet();

	PersonaEntry &merged = m_personas[index];
	auto it = std::find_if(m_userPersonas.begin(), m_userPersonas.end(),
		[&](const PersonaEntry &u) { return u.id == merged.id; });
	if (it == m_userPersonas.end()) {
		PersonaEntry entry = merged;
		entry.isSystem = false;
		m_userPersonas.append(entry);
		it = m_userPersonas.end() - 1;
	}

	it->enabledTools = defaults;
	merged.enabledTools = defaults;
	it->enabledCapabilities = resolveCaps(defaults);
	merged.enabledCapabilities = it->enabledCapabilities;
	saveToSettings();
	emit dataChanged(this->index(index, 0), this->index(index, 0), {EnabledToolsRole});
	emit personaEnabledToolsChanged(index, merged.enabledTools);
}

void AIPersonaModel::toggleCapability(int personaIndex, const QString &capId)
{
	if (personaIndex < 0 || personaIndex >= m_personas.size()) return;

	PersonaEntry &merged = m_personas[personaIndex];
	auto it = std::find_if(m_userPersonas.begin(), m_userPersonas.end(),
		[&](const PersonaEntry &u) { return u.id == merged.id; });
	if (it == m_userPersonas.end()) {
		PersonaEntry entry = merged;
		entry.isSystem = false;
		m_userPersonas.append(entry);
		it = m_userPersonas.end() - 1;
	}

	// Init from effective tools if never stored
	QStringList effective = effectiveEnabledTools(personaIndex);
	QSet<QString> toolSet(effective.begin(), effective.end());
	if (it->enabledCapabilities.isEmpty())
		it->enabledCapabilities = resolveCaps(effective);
	if (it->enabledTools.isEmpty())
		it->enabledTools = effective;

	// Resolve "*" wildcard to actual cap IDs before toggling
	QStringList storedCaps = it->enabledCapabilities;
	if (storedCaps.contains(QStringLiteral("*")))
		storedCaps = resolveCaps(effective);

	QStringList capTools = resolveCapabilitiesToTools(toJsonArr({capId}));
	QSet<QString> capSet(storedCaps.begin(), storedCaps.end());

	if (capSet.contains(capId))
	{
		capSet.remove(capId);
		for (const QString &t : capTools) toolSet.remove(t);
	}
	else
	{
		capSet.insert(capId);
		for (const QString &t : capTools) toolSet.insert(t);
	}

	it->enabledCapabilities = QStringList(capSet.begin(), capSet.end());
	it->enabledTools = QStringList(toolSet.begin(), toolSet.end());
	it->enabledTools.sort();
	it->enabledCapabilities.sort();
	merged.enabledTools = it->enabledTools;
	merged.enabledCapabilities = it->enabledCapabilities;

	saveToSettings();
	emit dataChanged(this->index(personaIndex, 0), this->index(personaIndex, 0), {EnabledToolsRole, EnabledCapabilitiesRole});
	emit personaEnabledToolsChanged(personaIndex, it->enabledTools);
	emit personaEnabledCapabilitiesChanged(personaIndex, it->enabledCapabilities);
}

void AIPersonaModel::toggleTool(int personaIndex, const QString &toolName)
{
	if (personaIndex < 0 || personaIndex >= m_personas.size()) return;

	PersonaEntry &merged = m_personas[personaIndex];
	auto it = std::find_if(m_userPersonas.begin(), m_userPersonas.end(),
		[&](const PersonaEntry &u) { return u.id == merged.id; });
	if (it == m_userPersonas.end()) {
		PersonaEntry entry = merged;
		entry.isSystem = false;
		m_userPersonas.append(entry);
		it = m_userPersonas.end() - 1;
	}

	// Init from effective tools if never stored
	QStringList effective = effectiveEnabledTools(personaIndex);
	QSet<QString> toolSet(effective.begin(), effective.end());
	if (it->enabledTools.isEmpty())
		it->enabledTools = effective;

	if (toolSet.contains(toolName))
		toolSet.remove(toolName);
	else
		toolSet.insert(toolName);

	it->enabledTools = QStringList(toolSet.begin(), toolSet.end());
	it->enabledTools.sort();
	it->enabledCapabilities = resolveCaps(it->enabledTools);

	merged.enabledTools = it->enabledTools;
	merged.enabledCapabilities = it->enabledCapabilities;

	saveToSettings();
	emit dataChanged(this->index(personaIndex, 0), this->index(personaIndex, 0), {EnabledToolsRole, EnabledCapabilitiesRole});
	emit personaEnabledToolsChanged(personaIndex, it->enabledTools);
	emit personaEnabledCapabilitiesChanged(personaIndex, it->enabledCapabilities);
}

QStringList AIPersonaModel::enabledCapabilityIds(int personaIndex)
{
	loadFromSettings();
	if (personaIndex < 0 || personaIndex >= m_personas.size())
		return {};
	const QStringList &s = m_personas.at(personaIndex).enabledCapabilities;
	if (!s.isEmpty() && !s.contains(QStringLiteral("*"))) return s;
	QStringList all;
	for (const QVariant &cv : capabilities())
		all.append(cv.toMap().value(QStringLiteral("id")).toString());
	return all;
}
