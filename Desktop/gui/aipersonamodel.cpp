#include "aipersonamodel.h"
#include "dirs.h"
#include "jasptheme.h"
#include "utilities/settings.h"
#include "utilities/appdirs.h"
#include "qutils.h"
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
#include "preferencesmodel.h"

// Forward declarations — static helpers defined later in this file
static QJsonArray toJsonArr(const QStringList &ids);

// ============================================================================
// Construction
// ============================================================================

AIPersonaModel::AIPersonaModel(QObject *parent)
	: QAbstractListModel(parent)
{
	loadCapabilities();
	loadPersonaSettings();
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
	case IdRole:					return p.id;
	case NameRole:					return p.name;
	case NameDisplayRole:
	case Qt::DisplayRole:
	{
		bool isActive = currentPersonaIndex() == index.row();
		return (isActive ? "✓ " : "") + p.name;
	}
	case PersonaPromptRole:			return p.personaPrompt;
	case ImagePathRole:				return resolvedImageUrl(p.imagePath);
	case IsSystemRole:				return p.isSystem;
	case EnabledToolsRole:			return p.enabledTools;
	case EnabledCapabilitiesRole:	return p.enabledCapabilities;
	default:						return {};
	}
}

bool AIPersonaModel::setData(const QModelIndex &index, const QVariant &value, int role)
{
	if (!index.isValid() || index.row() < 0 || index.row() >= m_personas.size())
		return false;

	PersonaEntry &persona = m_personas[index.row()];

	switch (role) {
	case NameRole:
		if (value.toString() == persona.name) return true;
		setUniqueName(persona, value.toString());
		break;
	case PersonaPromptRole:
		if (value.toString() == persona.personaPrompt) return true;
		persona.personaPrompt = value.toString();
		break;
	case ImagePathRole:
		if (value.toString().isEmpty()) return true;
		persona.imagePath = copyImageToPersonasDir(value.toString());
		if (index.row() == m_currentPersonaIndex)
			emit activePersonaAvatarChanged();
		break;
	default:
		return false;
	}

	saveToSettings();

	emit dataChanged(index, index, {role});

	return true;
}

QHash<int, QByteArray> AIPersonaModel::roleNames() const
{
	return {
		{ IdRole,					"personaId" },
		{ NameRole,					"personaName" },
		{ NameDisplayRole,			"personaDisplayName" },
		{ PersonaPromptRole,		"personaPrompt" },
		{ ImagePathRole,			"personaImagePath" },
		{ IsSystemRole,				"personaIsSystem" },
		{ EnabledToolsRole,			"personaEnabledTools" },
		{ EnabledCapabilitiesRole,	"personaEnabledCapabilities" }
	};
}

// ============================================================================
// Persona CRUD
// ============================================================================

int AIPersonaModel::addPersona()
{
	PersonaEntry p;
	p.id           = QUuid::createUuid().toString(QUuid::WithoutBraces);
	p.name         = QStringLiteral("New Persona");
	p.personaPrompt = QStringLiteral("");
	p.imagePath    = QStringLiteral("");
	p.isSystem     = false;
	p.enabledCapabilities = getAllCapabilityIds();  // new personas start with all caps
	p.enabledTools = resolveCapabilitiesToTools(toJsonArr(p.enabledCapabilities));

	beginInsertRows(QModelIndex(), m_personas.size(), m_personas.size());
	m_personas.append(p);
	endInsertRows();

	saveToSettings();

	return indexOfId(p.id);
}

void AIPersonaModel::removePersona(int index)
{
	if (index < 0 || index >= m_personas.size()) return;
	const PersonaEntry &p = m_personas.at(index);
	if (p.isSystem) return; // cannot delete system personas

	beginResetModel();
	m_personas.erase(
		std::remove_if(m_personas.begin(), m_personas.end(),
			[&](const PersonaEntry &u) { return u.id == p.id; }),
		m_personas.end());
	endResetModel();


	if (m_currentPersonaIndex >= m_personas.size())
		setCurrentPersonaIndex(m_personas.size() - 1);

	saveToSettings();
}

void AIPersonaModel::duplicatePersona(int ind)
{
	if (ind < 0 || ind >= m_personas.size()) return;

	PersonaEntry p = m_personas.at(ind);
	p.id          = QUuid::createUuid().toString(QUuid::WithoutBraces);
	setUniqueName(p, p.name + QStringLiteral(" (copy)"));
	p.isSystem    = false;

	beginInsertRows(QModelIndex(), m_personas.size(), m_personas.size());
	m_personas.append(p);
	endInsertRows();

	int newIdx = indexOfId(p.id);
	setCurrentPersonaIndex(newIdx);
	saveToSettings();
}

int AIPersonaModel::getRole(QString name)
{
	for (const auto &[roleId, roleName] : roleNames().asKeyValueRange())
		if (name == roleName)
			return roleId;
	return 0;
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


QUrl AIPersonaModel::resolvedImageUrl(const QString &imagePath) const
{
	if (imagePath.isEmpty() || !QFile::exists(imagePath))
		return QUrl(JaspTheme::currentIconPath() + "jaspAI.png");

	if (QFile::exists(imagePath))
		return QUrl::fromLocalFile(imagePath);

	return QUrl(defaultPersonaImagePath());
}

QUrl AIPersonaModel::shippedPersonaImageUrl(const QString &filename) const
{
	QString path = tq(Dirs::resourcesDir()) + "PersonaImages/" + filename;
	if (QFile::exists(path))
		return QUrl::fromLocalFile(path);
	return QUrl(defaultPersonaImagePath());
}

QString AIPersonaModel::shippedPersonaImagePath(const QString &filename) const
{
	QString path = tq(Dirs::resourcesDir()) + "PersonaImages/" + filename;
	if (QFile::exists(path))
		return QDir::toNativeSeparators(path);
	return {};
}

QUrl AIPersonaModel::shippedPersonaImagesDir() const
{
	QString path = tq(Dirs::resourcesDir()) + "PersonaImages";
	return QUrl::fromLocalFile(path);
}

int AIPersonaModel::personaIndexForName(const QString &name) const
{
	for (int i = 0; i < m_personas.size(); ++i)
	{
		if (m_personas[i].name == name)
			return i;
	}
	return -1;
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

QString AIPersonaModel::makeWebPath(const QString& path) const
{
	if (path.isEmpty())
		return {};

	QFileInfo fi(path);
	if (!fi.exists())
		return {};

	return QStringLiteral("jaspPersona:///") + fi.fileName();
}

void AIPersonaModel::setUniqueName(PersonaEntry & persona, const QString & name)
{
	bool isUnique = false;
	int counter = 1;
	QString uniqueName = name;

	while (!isUnique)
	{
		isUnique = true;
		for (const PersonaEntry &p : m_personas)
			if (p != persona && p.name == uniqueName)
				isUnique = false;
		if (!isUnique)
		{
			counter++;
			uniqueName = name + " " + QString::number(counter);
		}
	}
	persona.name = uniqueName;
}

QString AIPersonaModel::activePersonaAvatarWeb() const
{
	const PersonaEntry &p = activePersona();
	return makeWebPath(p.imagePath);
}

QString AIPersonaModel::userAvatar() const
{
	QString stored = PreferencesModel::prefs()->aiUserAvatar();
	if (stored.isEmpty()) return shippedPersonaImageUrl("userPersona5.png").toString();
	return resolvedImageUrl(stored).toString();
}

QString AIPersonaModel::userAvatarWeb() const
{
	QString stored = PreferencesModel::prefs()->aiUserAvatar();
	if (stored.isEmpty())
	{
		QString path = shippedPersonaImagePath("userPersona5.png");
		if (!path.isEmpty())
			return makeWebPath(path);
		return {};
	}
	return makeWebPath(stored);
}

void AIPersonaModel::setUserAvatar(QString path)
{
	if (path.isEmpty())
		return;

	QUrl urlPath = path;

	path = copyImageToPersonasDir(urlPath.toString());
	if (!path.isEmpty())
		PreferencesModel::prefs()->setAiUserAvatar(path);

	emit userAvatarChanged();
}

void AIPersonaModel::setCurrentPersonaIndex(int ind)
{
	if (ind < 0) ind = 0;
	if (ind >= m_personas.size()) ind = m_personas.size() - 1;
	if (ind == m_currentPersonaIndex) return;

	emit dataChanged(index(m_currentPersonaIndex), index(m_currentPersonaIndex)); // In order to change the Tab Button text
	m_currentPersonaIndex = ind;
	emit dataChanged(index(m_currentPersonaIndex), index(m_currentPersonaIndex));
	emit currentPersonaIndexChanged();
	emit activePersonaAvatarChanged();
	emit activePersonaAllowAnnotationChanged();

	// Persist the active persona by UUID
	if (ind >= 0 && ind < m_personas.size())
		Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, m_personas.at(ind).id);
	else
		Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, QString());
}

bool AIPersonaModel::activePersonaAllowAnnotation()
{
	const PersonaEntry &p = activePersona();

	if (p.enabledCapabilities.contains("base") && p.enabledCapabilities.contains("write-reports") && p.enabledCapabilities.contains("inspect-analyses"))
		return true;

	return false;
}


// ============================================================================
// Helper — resolve capability IDs to tool names
// ============================================================================

QStringList AIPersonaModel::resolveCapabilitiesToTools(const QJsonArray &capsArr) const
{
	// "*" means all capabilities → all tools
	bool getAllMethods = false;
	for (const QJsonValue &v : capsArr)
		if (v.isString() && v.toString() == QStringLiteral("*"))
			getAllMethods = true;

	QStringList enabledCaps;
	if (!getAllMethods)
	{
		for (const QJsonValue &v : capsArr)
		{
			if (!v.isString()) continue;
			enabledCaps.append(v.toString());
		}
	}

	// Collect methods for the requested capabilities
	QSet<QString> resolved;
	for (const auto capVar : m_capabilities)
	{
		QMap<QString, QVariant> cap = capVar.toMap();
		if (getAllMethods || enabledCaps.contains(cap["id"].toString()))
		{
			QStringList methods = cap["methods"].toStringList();
			for (const QString & method : methods)
				resolved.insert(method);
		}
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
QStringList AIPersonaModel::resolveCaps(const QStringList &tools) const
{
	if (tools.isEmpty()) return {};

	QSet<QString> toolSet(tools.begin(), tools.end());
	QStringList covered;
	for (const auto capVar : m_capabilities)
	{
		QMap<QString, QVariant> cap = capVar.toMap();
		QStringList methods = cap["methods"].toStringList();
		QString id = cap["id"].toString();

		bool allPresent = true;
		for (const QString &mv : methods)
		{
			if (!toolSet.contains(mv)) {
				allPresent = false;
				break;
			}
		}
		if (allPresent)
			covered.append(id);
	}
	covered.sort();
	return covered;
}

// ============================================================================
// Persistence
// ============================================================================

void AIPersonaModel::loadPersonaSettings(bool onlySystem)
{
	beginResetModel();

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
					if (capsArr.size() == 1 && capsArr[0].isString() && capsArr[0].toString() == "*")
						entry.enabledCapabilities = getAllCapabilityIds();
					else
					{
						for (const QJsonValue &cv : capsArr)
							if (cv.isString()) entry.enabledCapabilities.append(cv.toString());
					}
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

				m_personas.append(entry);
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
	if (m_personas.isEmpty())
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
		m_personas.append(fallback);
	}

	if (!onlySystem)
	{
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
						if (capsArr.size() == 1 && capsArr[0].isString() && capsArr[0].toString() == "*")
							entry.enabledCapabilities = getAllCapabilityIds();
						else
						{
							for (const QJsonValue &cv : capsArr)
								if (cv.isString()) entry.enabledCapabilities.append(cv.toString());
						}
						entry.enabledCapabilities.sort();
					}


					// Migration: if caps not stored but tools are, derive caps from tools
					if (entry.enabledCapabilities.isEmpty() && !entry.enabledTools.isEmpty())
						entry.enabledCapabilities = resolveCaps(entry.enabledTools);

					if (entry.id.isEmpty())
						entry.id = QUuid::createUuid().toString(QUuid::WithoutBraces);

					m_personas.append(entry);
				}
			}
		}
	}

	endResetModel();

	// 5. Restore the active persona.
	QString activeId = Settings::value(Settings::AI_CURRENT_PERSONA_ID).toString();
	int idx = indexOfId(activeId);
	if (idx < 0 && !m_personas.isEmpty())
		idx = 0;
	if (idx >= m_personas.size())
		idx = m_personas.size() - 1;

	if (idx >= 0)
	{
		m_currentPersonaIndex = idx;
		emit currentPersonaIndexChanged();
	}
}

void AIPersonaModel::saveToSettings()
{
	QJsonArray arr;
	for (const PersonaEntry &p : m_personas)
	{
		if (p.isSystem) continue;
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
	beginResetModel();
	m_personas.clear();
	loadPersonaSettings(true);
	endResetModel();

	Settings::setValue(Settings::AI_USER_PERSONAS, QStringLiteral("[]"));
	Settings::setValue(Settings::AI_CURRENT_PERSONA_ID, m_personas.at(0).id);
	Settings::setValue(Settings::AI_USER_AVATAR, QString());

	m_currentPersonaIndex = 0;
	emit currentPersonaIndexChanged();
	emit activePersonaAvatarChanged();
	emit userAvatarChanged();
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

QString AIPersonaModel::resolveDefaultImage() const
{
	return {};
}

QString AIPersonaModel::defaultPersonaImagePath() const
{
	return JaspTheme::currentIconPath() + "jaspAI.png";
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
		auto it = std::find_if(m_personas.begin(), m_personas.end(),
			[&](const PersonaEntry &s) { return s.id == p.id; });
		if (it != m_personas.end() && !it->enabledTools.isEmpty())
			return it->enabledTools;
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

void AIPersonaModel::loadCapabilities()
{
	std::string path = Dirs::resourcesDir() + "JASP_Capabilities.json";
	QFile file(tq(path));
	if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
		return;

	QJsonParseError err;
	QJsonDocument doc = QJsonDocument::fromJson(file.readAll(), &err);
	file.close();
	if (err.error != QJsonParseError::NoError || !doc.isObject())
		return;

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

		m_capabilities.append(cap);
	}
}

void AIPersonaModel::toggleCapability(int personaIndex, const QString &capId)
{
	if (personaIndex < 0 || personaIndex >= m_personas.size()) return;

	PersonaEntry &persona = m_personas[personaIndex];

	// Init from effective tools if never stored
	QStringList effective = effectiveEnabledTools(personaIndex);
	QSet<QString> toolSet(effective.begin(), effective.end());
	if (persona.enabledCapabilities.isEmpty())
		persona.enabledCapabilities = resolveCaps(effective);
	if (persona.enabledTools.isEmpty())
		persona.enabledTools = effective;

	QStringList storedCaps = persona.enabledCapabilities;

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

	persona.enabledCapabilities = QStringList(capSet.begin(), capSet.end());
	persona.enabledTools = QStringList(toolSet.begin(), toolSet.end());
	persona.enabledTools.sort();
	persona.enabledCapabilities.sort();

	saveToSettings();
	emit dataChanged(this->index(personaIndex, 0), this->index(personaIndex, 0), {EnabledToolsRole, EnabledCapabilitiesRole});

	if (persona == activePersona())
		emit activePersonaAllowAnnotationChanged();
}

void AIPersonaModel::toggleTool(int personaIndex, const QString &toolName)
{
	if (personaIndex < 0 || personaIndex >= m_personas.size()) return;

	PersonaEntry &persona = m_personas[personaIndex];

	// Init from effective tools if never stored
	QStringList effective = effectiveEnabledTools(personaIndex);
	QSet<QString> toolSet(effective.begin(), effective.end());
	if (persona.enabledTools.isEmpty())
		persona.enabledTools = effective;

	if (toolSet.contains(toolName))
		toolSet.remove(toolName);
	else
		toolSet.insert(toolName);

	persona.enabledTools = QStringList(toolSet.begin(), toolSet.end());
	persona.enabledTools.sort();
	persona.enabledCapabilities = resolveCaps(persona.enabledTools);

	saveToSettings();
	emit dataChanged(this->index(personaIndex, 0), this->index(personaIndex, 0), {EnabledToolsRole, EnabledCapabilitiesRole});

	if (persona == activePersona())
		emit activePersonaAllowAnnotationChanged();
}

QStringList AIPersonaModel::getAllCapabilityIds() const
{
	QStringList result;
	for (const auto c : m_capabilities)
		result.append(c.toMap()["id"].toString());

	return result;
}
