#include "aiconfigmodel.h"
#include "utilities/settings.h"
#include "utilities/secretstore.h"
#include "qutils.h"
#include "log.h"
#include "dirs.h"

#include <QFile>
#include <QJsonDocument>
#include <QJsonArray>
#include <QUuid>

// ═══════════════════════════════════════════════════════════════
// AIProviderListModel
// ═══════════════════════════════════════════════════════════════

AIProviderListModel::AIProviderListModel(QObject *parent)
	: QAbstractListModel(parent)
{}

void AIProviderListModel::setProviders(const QVector<AIProviderEntry> &providers)
{
	beginResetModel();
	m_providers = providers;
	endResetModel();
}

int AIProviderListModel::indexOfId(const QString &id) const
{
	for (int i = 0; i < m_providers.size(); ++i)
		if (m_providers[i].id == id)
			return i;
	return -1;
}

int AIProviderListModel::rowCount(const QModelIndex &) const
{
	return m_providers.size();
}

QVariant AIProviderListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid() || index.row() >= m_providers.size())
		return {};

	const auto &p = m_providers[index.row()];
	switch (role) {
	case IdRole:         return p.id;
	case NameRole:       return p.name;
	case EndpointRole:   return p.endpoint;
	case IsSystemRole:   return p.isSystem;
	case ModelCountRole: return p.models.size();
	default:             return {};
	}
}

QHash<int, QByteArray> AIProviderListModel::roleNames() const
{
	return {
		{ IdRole,         "providerId" },
		{ NameRole,       "providerName" },
		{ EndpointRole,   "providerEndpoint" },
		{ IsSystemRole,   "providerIsSystem" },
		{ ModelCountRole, "providerModelCount" }
	};
}

// ═══════════════════════════════════════════════════════════════
// AIModelListModel
// ═══════════════════════════════════════════════════════════════

AIModelListModel::AIModelListModel(QObject *parent)
	: QAbstractListModel(parent)
{}

void AIModelListModel::setModels(const QVector<AIModelEntry> &models)
{
	beginResetModel();
	m_models = models;
	endResetModel();
}

void AIModelListModel::clear()
{
	beginResetModel();
	m_models.clear();
	endResetModel();
}

int AIModelListModel::indexOfId(const QString &id) const
{
	for (int i = 0; i < m_models.size(); ++i)
		if (m_models[i].id == id)
			return i;
	return -1;
}

int AIModelListModel::rowCount(const QModelIndex &) const
{
	return m_models.size();
}

QVariant AIModelListModel::data(const QModelIndex &index, int role) const
{
	if (!index.isValid() || index.row() >= m_models.size())
		return {};

	const auto &m = m_models[index.row()];
	switch (role) {
	case IdRole:          return m.id;
	case NameRole:        return m.name;
	case ModelStringRole: return m.model;

	case ExtraParamsRole:
		return QString::fromUtf8(QJsonDocument(m.extraParams).toJson(QJsonDocument::Compact));

	case PostfixRole:  return m.systemPromptPostfix;
	case IsSystemRole: return m.isSystem;
	default:           return {};
	}
}

QHash<int, QByteArray> AIModelListModel::roleNames() const
{
	return {
		{ IdRole,          "modelId" },
		{ NameRole,        "modelName" },
		{ ModelStringRole, "modelString" },
		{ ExtraParamsRole, "modelExtraParams" },
		{ PostfixRole,     "modelPostfix" },
		{ IsSystemRole,    "modelIsSystem" }
	};
}

// ═══════════════════════════════════════════════════════════════
// AIConfigModel — singleton
// ═══════════════════════════════════════════════════════════════

AIConfigModel* AIConfigModel::s_singleton = nullptr;

AIConfigModel::AIConfigModel(QObject *parent)
	: QObject(parent)
{
	s_singleton = this;

	m_providerListModel = new AIProviderListModel(this);
	m_modelListModel    = new AIModelListModel(this);

	loadShippedProviders();

	// Always prepend a synthetic "Custom Provider" — replaces the old
	// "Generic Provider" JSON entry. Users select it for ad-hoc connections.
	addCustomProvider();

	loadUserData();

	// Feed list models (loadUserData may have returned early with no saved data)
	m_providerListModel->setProviders(m_providers);

	// Ensure something is selected on first launch
	if (m_currentProviderIndex < 0 && !m_providers.isEmpty())
		setCurrentProviderIndex(0);
}

AIConfigModel::~AIConfigModel()
{
	if (s_singleton == this)
		s_singleton = nullptr;
}

// ── List model accessors ──────────────────────────────────────

QObject* AIConfigModel::providerListModel() { return m_providerListModel; }
QObject* AIConfigModel::modelListModel()    { return m_modelListModel; }

QVariantList AIConfigModel::providerValues() const
{
	QVariantList list;
	for (const auto &p : m_providers)
	{
		QVariantMap item;
		item[QStringLiteral("label")] = p.name;
		item[QStringLiteral("value")] = p.id;
		list.append(item);
	}
	return list;
}

QVariantList AIConfigModel::modelValues() const
{
	QVariantList list;

	// Virtual entry: "Custom" — no model preset, user types model name manually
	{
		QVariantMap item;
		item[QStringLiteral("label")] = QStringLiteral("Custom (No Model Preset)");
		item[QStringLiteral("value")] = QString(); // empty id = custom
		list.append(item);
	}

	const auto *prov = currentProvider();
	if (prov)
	{
		for (const auto &m : prov->models)
		{
			QVariantMap item;
			item[QStringLiteral("label")] = m.name;
			item[QStringLiteral("value")] = m.id;
			list.append(item);
		}
	}
	return list;
}

// ── Provider entry access ─────────────────────────────────────

const AIProviderEntry* AIConfigModel::currentProvider() const
{
	if (m_currentProviderIndex < 0 || m_currentProviderIndex >= m_providers.size())
		return nullptr;
	return &m_providers[m_currentProviderIndex];
}

const AIModelEntry* AIConfigModel::currentModelEntry() const
{
	// m_currentModelIndex is the dropdown index: 0 = custom, 1+ = real model at [i-1]
	if (m_currentModelIndex <= 0) return nullptr;
	const auto *prov = currentProvider();
	int realIdx = m_currentModelIndex - 1;
	if (!prov || realIdx >= prov->models.size())
		return nullptr;
	return &prov->models[realIdx];
}

// ── Current selection ─────────────────────────────────────────

int AIConfigModel::currentProviderIndex() const { return m_currentProviderIndex; }
int AIConfigModel::currentModelIndex()    const { return m_currentModelIndex;    }

void AIConfigModel::setCurrentProviderIndex(int i)
{
	if (i < 0 || i >= m_providers.size() || i == m_currentProviderIndex)
		return;

	m_currentProviderIndex = i;
	m_modelListModel->setModels(m_providers[i].models);
	emit currentProviderIndexChanged();
	emit currentProviderChanged();
	emit modelValuesChanged();

	// Restore last-used model for this provider. 0 = custom, 1+ = real model.
	const auto &prov = m_providers[i];
	int modelIdx = prov.models.isEmpty() ? 0 : 1; // custom if provider has no models
	if (m_providerOverrides.contains(prov.id))
	{
		const auto &ov = m_providerOverrides[prov.id];
		if (ov.currentModelId.isEmpty())
			modelIdx = 0; // saved as custom mode
		else
		{
			int found = m_modelListModel->indexOfId(ov.currentModelId);
			if (found >= 0)
				modelIdx = found + 1; // offset for virtual "Custom" entry
		}
	}
	setCurrentModelIndex(modelIdx);

	// Always fire derived signals on provider switch — even if the model
	// index happens to be the same number, the actual model is different.
	emitAllDerivedSignals();
	saveUserData();
}

void AIConfigModel::setCurrentModelIndex(int i)
{
	// i is the dropdown index: 0 = custom, 1+ = real model
	const auto *prov = currentProvider();
	if (!prov) return;

	// Provider has no models — only custom mode (0) is valid
	if (prov->models.isEmpty())
		i = 0;

	// Valid range: 0 (custom) through prov->models.size() (last real model)
	int maxIndex = prov->models.size();
	if (i < 0 || i > maxIndex) return;
	if (i == m_currentModelIndex) return;

	m_currentModelIndex = i;
	emit currentModelIndexChanged();
	emitAllDerivedSignals();

	saveUserData();
}

void AIConfigModel::emitAllDerivedSignals()
{
	emit currentEndpointChanged();
	emit currentApiKeyChanged();
	emit currentModelChanged();
	emit currentExtraParamsChanged();
	emit currentUseCompleteSchemaChanged();
	emit currentSystemPromptPostfixChanged();
	emit currentChatLimitActiveChanged();
	emit currentChatLimitChanged();
	emit currentMessageExtraChanged();
	emit currentWarningChanged();
}

// ── Derived getters — read straight from m_providers ──────────

QString AIConfigModel::currentEndpoint() const
{
	const auto *prov = currentProvider();
	if (!prov) return {};
	if (m_providerOverrides.contains(prov->id) && !m_providerOverrides[prov->id].endpoint.isEmpty())
		return m_providerOverrides[prov->id].endpoint;
	return prov->endpoint;
}

QString AIConfigModel::currentApiKey() const
{
	const auto *prov = currentProvider();
	if (!prov) return {};
	if (m_providerOverrides.contains(prov->id) && !m_providerOverrides[prov->id].apiKey.isEmpty())
		return SecretStore::decryptValue(m_providerOverrides[prov->id].apiKey);
	return prov->defaultApiKey;
}

QString AIConfigModel::currentModel() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id) && m_modelOverrides[m->id].modelNameSet)
			return m_modelOverrides[m->id].modelName;
		return m->model;
	}

	// Custom mode: return the user-typed model string from overrides
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id))
		return m_providerOverrides[prov->id].customModel;
	return {};
}

void AIConfigModel::setCurrentModel(const QString &v)
{
	auto *m = const_cast<AIModelEntry*>(currentModelEntry());
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		if (ov.modelName == v && ov.modelNameSet) return;
		ov.modelName = v;
		ov.modelNameSet = true;
		m_modelOverrides[m->id] = ov;
		m->model = v;
	}
	else
	{
		// Custom mode: store the user-typed model in ProviderOverrides.customModel
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		if (ov.customModel == v) return;
		ov.customModel = v;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentModelChanged();
	saveUserData();
}

QString AIConfigModel::currentExtraParams() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id) && m_modelOverrides[m->id].extraParamsSet)
			return QString::fromUtf8(QJsonDocument(m_modelOverrides[m->id].extraParams).toJson(QJsonDocument::Compact));
		if (!m->extraParams.isEmpty())
			return QString::fromUtf8(QJsonDocument(m->extraParams).toJson(QJsonDocument::Compact));
		return {};
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id) && m_providerOverrides[prov->id].extraParamsSet)
		return QString::fromUtf8(QJsonDocument(m_providerOverrides[prov->id].extraParams).toJson(QJsonDocument::Compact));
	return {};
}

bool AIConfigModel::currentUseCompleteSchema() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id))
			return m_modelOverrides[m->id].useCompleteSchema;
		return m->useCompleteSchema;
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id))
		return m_providerOverrides[prov->id].useCompleteSchema;
	return true;
}

QString AIConfigModel::currentSystemPromptPostfix() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id) && m_modelOverrides[m->id].systemPromptPostfixSet)
			return m_modelOverrides[m->id].systemPromptPostfix;
		return m->systemPromptPostfix;
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id) && m_providerOverrides[prov->id].systemPromptPostfixSet)
		return m_providerOverrides[prov->id].systemPromptPostfix;
	return {};
}

bool AIConfigModel::currentChatLimitActive() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id))
			return m_modelOverrides[m->id].chatLimitActive;
		return m->chatLimitActive;
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id))
		return m_providerOverrides[prov->id].chatLimitActive;
	return true;
}

int AIConfigModel::currentChatLimit() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id))
			return m_modelOverrides[m->id].chatLimit;
		return m->chatLimit;
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id))
		return m_providerOverrides[prov->id].chatLimit;
	return 256000;
}

QString AIConfigModel::currentMessageExtra() const
{
	const auto *m = currentModelEntry();
	if (m)
	{
		if (m_modelOverrides.contains(m->id) && m_modelOverrides[m->id].messageExtraSet)
			return m_modelOverrides[m->id].messageExtra;
		return {};
	}
	const auto *prov = currentProvider();
	if (prov && m_providerOverrides.contains(prov->id) && m_providerOverrides[prov->id].messageExtraSet)
		return m_providerOverrides[prov->id].messageExtra;
	return {};
}

QString AIConfigModel::currentWarning() const
{
	const auto *m = currentModelEntry();
	if (!m) return {};
	return m->warning;
}

bool AIConfigModel::currentProviderIsUserEditable() const
{
	const auto *prov = currentProvider();
	return prov && !prov->isSystem;
}

// ── Derived setters — write to m_providers, save diff ─────────

void AIConfigModel::setCurrentEndpoint(const QString &v)
{
	const auto *prov = currentProvider();
	if (!prov) return;

	QString cur = currentEndpoint();
	if (cur == v) return;

	ProviderOverrides ov;
	if (m_providerOverrides.contains(prov->id))
		ov = m_providerOverrides[prov->id];
	ov.endpoint = v;
	m_providerOverrides[prov->id] = ov;

	auto *mutableProv = const_cast<AIProviderEntry*>(prov);
	mutableProv->endpoint = v;

	emit currentEndpointChanged();
	saveUserData();
}

void AIConfigModel::setCurrentApiKey(const QString &v)
{
	const auto *prov = currentProvider();
	if (!prov) return;

	QString cur = currentApiKey();
	if (cur == v) return;

	ProviderOverrides ov;
	if (m_providerOverrides.contains(prov->id))
		ov = m_providerOverrides[prov->id];
	ov.apiKey = v.isEmpty() ? QString() : SecretStore::encryptValue(v);
	m_providerOverrides[prov->id] = ov;

	emit currentApiKeyChanged();
	saveUserData();
}

void AIConfigModel::setCurrentExtraParams(const QString &v)
{
	QJsonDocument doc = QJsonDocument::fromJson(v.toUtf8());
	QJsonObject obj = doc.isObject() ? doc.object() : QJsonObject();

	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		if (ov.extraParams == obj && ov.extraParamsSet) return;
		ov.extraParams = obj;
		ov.extraParamsSet = true;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		if (ov.extraParams == obj && ov.extraParamsSet) return;
		ov.extraParams = obj;
		ov.extraParamsSet = true;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentExtraParamsChanged();
	saveUserData();
}

void AIConfigModel::setCurrentUseCompleteSchema(bool v)
{
	if (currentUseCompleteSchema() == v) return;

	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		ov.useCompleteSchema = v;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		ov.useCompleteSchema = v;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentUseCompleteSchemaChanged();
	saveUserData();
}

void AIConfigModel::setCurrentSystemPromptPostfix(const QString &v)
{
	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		if (ov.systemPromptPostfix == v && ov.systemPromptPostfixSet) return;
		ov.systemPromptPostfix = v;
		ov.systemPromptPostfixSet = true;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		if (ov.systemPromptPostfix == v && ov.systemPromptPostfixSet) return;
		ov.systemPromptPostfix = v;
		ov.systemPromptPostfixSet = true;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentSystemPromptPostfixChanged();
	saveUserData();
}

void AIConfigModel::setCurrentChatLimitActive(bool v)
{
	if (currentChatLimitActive() == v) return;

	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		ov.chatLimitActive = v;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		ov.chatLimitActive = v;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentChatLimitActiveChanged();
	saveUserData();
}

void AIConfigModel::setCurrentChatLimit(int v)
{
	if (currentChatLimit() == v) return;

	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		ov.chatLimit = v;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		ov.chatLimit = v;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentChatLimitChanged();
	saveUserData();
}

void AIConfigModel::setCurrentMessageExtra(const QString &v)
{
	const auto *m = currentModelEntry();
	if (m)
	{
		ModelOverrides ov;
		if (m_modelOverrides.contains(m->id))
			ov = m_modelOverrides[m->id];
		else
			ov = freshModelOverrides(m);
		if (ov.messageExtra == v && ov.messageExtraSet) return;
		ov.messageExtra = v;
		ov.messageExtraSet = true;
		m_modelOverrides[m->id] = ov;
	}
	else
	{
		const auto *prov = currentProvider();
		if (!prov) return;
		ProviderOverrides ov;
		if (m_providerOverrides.contains(prov->id))
			ov = m_providerOverrides[prov->id];
		if (ov.messageExtra == v && ov.messageExtraSet) return;
		ov.messageExtra = v;
		ov.messageExtraSet = true;
		m_providerOverrides[prov->id] = ov;
	}
	emit currentMessageExtraChanged();
	saveUserData();
}

// ── Reset ────────────────────────────────────────────────────

void AIConfigModel::resetToDefaults()
{
	m_providers.clear();
	m_providerOverrides.clear();
	m_modelOverrides.clear();

	loadShippedProviders();
	addCustomProvider();

	m_providerListModel->setProviders(m_providers);
	emit providerValuesChanged();
	m_modelListModel->clear();
	emit modelValuesChanged();

	// Invalidate indices so setCurrentProviderIndex fires the full
	// signal cascade even when the target happens to be index 0.
	m_currentProviderIndex = -1;
	m_currentModelIndex = -1;
	if (!m_providers.isEmpty())
		setCurrentProviderIndex(0);
}

void AIConfigModel::resetCurrentModelToDefaults()
{
	const auto *m = currentModelEntry();
	if (!m) return;

	const auto *prov = currentProvider();

	// Find the shipped pristine copies by model UUID
	const AIModelEntry*  shippedModel = nullptr;
	const AIProviderEntry* shippedProv = nullptr;
	for (const auto& sp : m_shipped)
	{
		for (const auto& sm : sp.models)
		{
			if (sm.id == m->id)
			{
				shippedModel = &sm;
				shippedProv  = &sp;
				break;
			}
		}
		if (shippedModel) break;
	}
	if (!shippedModel) return;

	// Remove any user overrides for this model
	m_modelOverrides.remove(m->id);

	// Restore shipped values into the active provider's model entry
	auto* mutableEntry = const_cast<AIModelEntry*>(m);
	mutableEntry->extraParams         = shippedModel->extraParams;
	mutableEntry->systemPromptPostfix = shippedModel->systemPromptPostfix;
	mutableEntry->model               = shippedModel->model;
	mutableEntry->useCompleteSchema   = shippedModel->useCompleteSchema;
	mutableEntry->chatLimit           = shippedModel->chatLimit;
	mutableEntry->chatLimitActive     = shippedModel->chatLimitActive;

	// Restore endpoint to the shipped provider value
	if (prov && shippedProv)
	{
		auto* mutableProv = const_cast<AIProviderEntry*>(prov);
		mutableProv->endpoint = shippedProv->endpoint;
		if (m_providerOverrides.contains(prov->id))
		{
			auto &ov = m_providerOverrides[prov->id];
			ov.endpoint.clear();
		}
	}

	emitAllDerivedSignals();
	saveUserData();
}

// ── Init ──────────────────────────────────────────────────────

void AIConfigModel::addCustomProvider()
{
	AIProviderEntry cp;
	cp.id        = QStringLiteral("00000000-0000-0000-0000-000000000001");
	cp.name      = QStringLiteral("Custom Provider");
	cp.isSystem  = true;
	// No models — the virtual "Custom (No Model Preset)" entry
	// in modelValues() provides the ad-hoc model field.
	m_providers.prepend(cp);
	m_shipped.prepend(cp);
}

void AIConfigModel::loadShippedProviders()
{
	std::string path = Dirs::resourcesDir() + "defaultProviders.json";
	QFile f(tq(path));
	if (!f.open(QIODevice::ReadOnly))
	{
		Log::log() << "AIConfigModel: could not open defaultProviders.json" << std::endl;
		return; // custom provider will be prepended by constructor
	}

	QJsonDocument doc = QJsonDocument::fromJson(f.readAll());
	f.close();

	if (!doc.isObject())
	{
		Log::log() << "AIConfigModel: defaultProviders.json is not a valid JSON object" << std::endl;
		return;
	}

	const QJsonArray arr = doc.object()["providers"].toArray();
	for (const auto &pval : arr)
	{
		if (!pval.isObject()) continue;
		QJsonObject pobj = pval.toObject();

		AIProviderEntry prov;
		prov.id           = pobj["id"].toString();
		prov.name         = pobj["name"].toString();
		prov.endpoint     = pobj["endpoint"].toString();
		prov.defaultApiKey = pobj["defaultApiKey"].toString();
		prov.isSystem     = true;

		const QJsonArray marr = pobj["models"].toArray();
		for (const auto &mval : marr)
		{
			if (!mval.isObject()) continue;
			QJsonObject mobj = mval.toObject();
			AIModelEntry m;
			m.id       = mobj["id"].toString();
			m.name     = mobj["name"].toString();
			m.model    = mobj["model"].toString();
			if (mobj.contains("extraParams") && mobj["extraParams"].isObject())
				m.extraParams = mobj["extraParams"].toObject();
			m.systemPromptPostfix = mobj["systemPromptPostfix"].toString();
			m.warning             = mobj["warning"].toString();
			m.useCompleteSchema = mobj["useCompleteSchema"].toBool(true);
			m.chatLimit         = mobj["chatLimit"].toInt(256000);
			m.chatLimitActive   = mobj["chatLimitActive"].toBool(true);
			m.isSystem = true;
			prov.models.append(m);
		}

		if (prov.models.isEmpty())
		{
			AIModelEntry m;
			m.id       = QUuid::createUuid().toString(QUuid::WithoutBraces);
			m.name     = QStringLiteral("Default");
			m.isSystem = true;
			prov.models.append(m);
		}

		m_providers.append(prov);
	}

	// Save pristine copy for diffing
	m_shipped = m_providers;
}

void AIConfigModel::loadUserData()
{
	QString json = Settings::value(Settings::AI_USER_PROVIDERS).toString();
	if (json.isEmpty())
		return;

	QJsonDocument doc = QJsonDocument::fromJson(json.toUtf8());
	if (!doc.isObject())
		return;

	QJsonObject root = doc.object();

	// ── Restore current selection ──────────────────────
	QString savedProviderId = root["currentProviderId"].toString();
	QString savedModelId    = root["currentModelId"].toString();

	// ── Apply provider overrides ────────────────────────
	const QJsonObject pov = root["providerOverrides"].toObject();
	for (auto it = pov.begin(); it != pov.end(); ++it)
	{
		QJsonObject o = it.value().toObject();
		ProviderOverrides ov;
		ov.endpoint      = o["endpoint"].toString();
		ov.apiKey        = o["apiKey"].toString();
		ov.currentModelId = o["currentModelId"].toString();
		ov.customModel    = o["customModel"].toString();
		if (o.contains("systemPromptPostfix")) { ov.systemPromptPostfix = o["systemPromptPostfix"].toString(); ov.systemPromptPostfixSet = true; }
		if (o.contains("extraParams"))         { ov.extraParams = o["extraParams"].toObject(); ov.extraParamsSet = true; }
		if (o.contains("useCompleteSchema"))    ov.useCompleteSchema = o["useCompleteSchema"].toBool();
		if (o.contains("chatLimit"))            ov.chatLimit = o["chatLimit"].toInt();
		if (o.contains("chatLimitActive"))      ov.chatLimitActive = o["chatLimitActive"].toBool();
		if (o.contains("messageExtra"))         { ov.messageExtra = o["messageExtra"].toString(); ov.messageExtraSet = true; }
		m_providerOverrides[it.key()] = ov;

		// Apply to m_providers
		for (auto &prov : m_providers)
		{
			if (prov.id == it.key())
			{
				if (!ov.endpoint.isEmpty())
					prov.endpoint = ov.endpoint;
				break;
			}
		}
	}

	// ── Apply model overrides ───────────────────────────
	const QJsonObject mov = root["modelOverrides"].toObject();
	for (auto it = mov.begin(); it != mov.end(); ++it)
	{
		QJsonObject o = it.value().toObject();
		ModelOverrides ov;
		if (o.contains("extraParams")) {
			ov.extraParams = o["extraParams"].toObject();
			ov.extraParamsSet = true;
		}
		if (o.contains("systemPromptPostfix")) {
			ov.systemPromptPostfix = o["systemPromptPostfix"].toString();
			ov.systemPromptPostfixSet = true;
		}
		if (o.contains("useCompleteSchema"))
			ov.useCompleteSchema = o["useCompleteSchema"].toBool();
		if (o.contains("chatLimit"))
			ov.chatLimit = o["chatLimit"].toInt();
		if (o.contains("chatLimitActive"))
			ov.chatLimitActive = o["chatLimitActive"].toBool();
		if (o.contains("messageExtra")) {
			ov.messageExtra = o["messageExtra"].toString();
			ov.messageExtraSet = true;
		}
		if (o.contains("modelName")) {
			ov.modelName = o["modelName"].toString();
			ov.modelNameSet = true;
		}
		m_modelOverrides[it.key()] = ov;
	}

	// ── Add user-created providers ──────────────────────
	const QJsonArray uprov = root["userProviders"].toArray();
	for (const auto &val : uprov)
	{
		if (!val.isObject()) continue;
		QJsonObject pobj = val.toObject();

		AIProviderEntry prov;
		prov.id       = pobj["id"].toString();
		prov.name     = pobj["name"].toString();
		prov.endpoint = pobj["endpoint"].toString();
		prov.defaultApiKey = pobj["defaultApiKey"].toString();
		prov.isSystem  = false;

		const QJsonArray marr = pobj["models"].toArray();
		for (const auto &mval : marr)
		{
			if (!mval.isObject()) continue;
			QJsonObject mobj = mval.toObject();
			AIModelEntry m;
			m.id    = mobj["id"].toString();
			m.name  = mobj["name"].toString();
			m.model = mobj["model"].toString();
			if (mobj.contains("extraParams"))
				m.extraParams = mobj["extraParams"].toObject();
			m.systemPromptPostfix = mobj["systemPromptPostfix"].toString();
			m.useCompleteSchema = mobj["useCompleteSchema"].toBool(true);
			m.chatLimit         = mobj["chatLimit"].toInt(256000);
			m.chatLimitActive   = mobj["chatLimitActive"].toBool(true);
			m.isSystem = false;
			prov.models.append(m);
		}

		if (prov.models.isEmpty())
		{
			AIModelEntry m;
			m.id   = QUuid::createUuid().toString(QUuid::WithoutBraces);
			m.name = QStringLiteral("Default");
			m.isSystem = false;
			prov.models.append(m);
		}

		m_providers.append(prov);
	}

	// ── Restore selection ───────────────────────────────
	if (!savedProviderId.isEmpty())
	{
		for (int i = 0; i < m_providers.size(); ++i)
		{
			if (m_providers[i].id == savedProviderId)
			{
				m_currentProviderIndex = i;
				m_modelListModel->setModels(m_providers[i].models);
				break;
			}
		}
	}

	if (m_currentProviderIndex >= 0)
	{
		// root contains "currentModelId" only if saved (always written by saveUserData)
		if (root.contains("currentModelId"))
		{
			if (savedModelId.isEmpty())
				m_currentModelIndex = 0; // explicit custom mode
			else
			{
				int mi = m_modelListModel->indexOfId(savedModelId);
				m_currentModelIndex = (mi >= 0) ? mi + 1 : 1; // dropdown index
			}
		}
		else
		{
			m_currentModelIndex = 1; // fresh start: first real model
		}

		// Clamp: if provider has no real models, only custom mode (0) is valid
		if (m_providers[m_currentProviderIndex].models.isEmpty())
			m_currentModelIndex = 0;
	}

	// Feed list models
	m_providerListModel->setProviders(m_providers);
}

void AIConfigModel::saveUserData()
{
	QJsonObject root;

	// ── Current selection ───────────────────────────────
	const auto *prov = currentProvider();
	const auto *mod  = currentModelEntry();
	if (prov)
	{
		root["currentProviderId"] = prov->id;
		// Always write currentModelId (empty = custom mode)
		root["currentModelId"] = mod ? mod->id : QString();
		// Update the last-used model for this provider
		ProviderOverrides pov;
		if (m_providerOverrides.contains(prov->id))
			pov = m_providerOverrides[prov->id];
		pov.currentModelId = mod ? mod->id : QString();
		if (pov.endpoint.isEmpty() && pov.apiKey.isEmpty() && pov.customModel.isEmpty())
			m_providerOverrides.remove(prov->id);
		else
			m_providerOverrides[prov->id] = pov;
	}

	// ── Provider overrides (diff against shipped) ───────
	QJsonObject povJson;
	for (auto it = m_providerOverrides.begin(); it != m_providerOverrides.end(); ++it)
	{
		QJsonObject o;
		// Diff: only store if differs from shipped
		bool hasShipped = false;
		for (const auto &sp : m_shipped)
		{
			if (sp.id == it.key()) { hasShipped = true; break; }
		}
		const auto &ov = it.value();
		if (hasShipped)
		{
			// For shipped providers, store only overrides
			if (!ov.endpoint.isEmpty())
			{
				// Check if differs from shipped
				bool differs = false;
				for (const auto &sp : m_shipped)
					if (sp.id == it.key() && sp.endpoint != ov.endpoint) { differs = true; break; }
				if (differs) o["endpoint"] = ov.endpoint;
			}
		}
		else
		{
			// User-created: store all
			if (!ov.endpoint.isEmpty()) o["endpoint"] = ov.endpoint;
		}
		if (!ov.apiKey.isEmpty())        o["apiKey"]        = ov.apiKey;
		if (!ov.currentModelId.isEmpty()) o["currentModelId"] = ov.currentModelId;
		if (!ov.customModel.isEmpty())    o["customModel"]    = ov.customModel;
		// Per-provider custom-mode fields
		if (ov.systemPromptPostfixSet)    o["systemPromptPostfix"] = ov.systemPromptPostfix;
		if (ov.extraParamsSet)            o["extraParams"]         = ov.extraParams;
		if (!ov.useCompleteSchema)        o["useCompleteSchema"]   = false;
		if (ov.chatLimit != 256000)       o["chatLimit"]           = ov.chatLimit;
		if (!ov.chatLimitActive)          o["chatLimitActive"]     = false;
		if (ov.messageExtraSet)           o["messageExtra"]        = ov.messageExtra;
		if (!o.isEmpty()) povJson[it.key()] = o;
	}
	root["providerOverrides"] = povJson;

	// ── Model overrides ─────────────────────────────────
	QJsonObject movJson;
	for (auto it = m_modelOverrides.begin(); it != m_modelOverrides.end(); ++it)
	{
		QJsonObject o;
		const auto &ov = it.value();

		// Find the shipped model to diff boolean/int fields against
		const AIModelEntry* shipped = nullptr;
		for (const auto &sp : m_shipped)
			for (const auto &sm : sp.models)
				if (sm.id == it.key()) { shipped = &sm; break; }

		if (ov.extraParamsSet)            o["extraParams"]          = ov.extraParams;
		if (ov.systemPromptPostfixSet)    o["systemPromptPostfix"] = ov.systemPromptPostfix;
		// Only write these when they differ from the shipped model —
		// otherwise the override's default would overwrite the
		// shipped value on first save of an unrelated field.
		if (!shipped || ov.useCompleteSchema != shipped->useCompleteSchema)
			o["useCompleteSchema"] = ov.useCompleteSchema;
		if (!shipped || ov.chatLimit != shipped->chatLimit)
			o["chatLimit"] = ov.chatLimit;
		if (!shipped || ov.chatLimitActive != shipped->chatLimitActive)
			o["chatLimitActive"] = ov.chatLimitActive;
		if (ov.messageExtraSet)           o["messageExtra"]         = ov.messageExtra;
		if (ov.modelNameSet)               o["modelName"]            = ov.modelName;
		movJson[it.key()] = o;
	}
	root["modelOverrides"] = movJson;

	// ── User-created providers (full entries) ────────────
	QJsonArray uprov;
	for (const auto &prov : m_providers)
	{
		if (prov.isSystem) continue; // shipped, not user-created
		QJsonObject po;
		po["id"]            = prov.id;
		po["name"]          = prov.name;
		po["endpoint"]      = prov.endpoint;
		po["defaultApiKey"] = prov.defaultApiKey;

		QJsonArray marr;
		for (const auto &m : prov.models)
		{
			QJsonObject mo;
			mo["id"]                 = m.id;
			mo["name"]               = m.name;
			mo["model"]              = m.model;
			if (!m.extraParams.isEmpty())
				mo["extraParams"] = m.extraParams;
			mo["systemPromptPostfix"] = m.systemPromptPostfix;
			if (!m.useCompleteSchema) mo["useCompleteSchema"] = false;
			if (m.chatLimit != 256000) mo["chatLimit"] = m.chatLimit;
			if (!m.chatLimitActive) mo["chatLimitActive"] = false;
			marr.append(mo);
		}
		po["models"] = marr;
		uprov.append(po);
	}
	root["userProviders"] = uprov;

	QString json = QString::fromUtf8(QJsonDocument(root).toJson(QJsonDocument::Compact));
	Settings::setValue(Settings::AI_USER_PROVIDERS, json);
}

AIConfigModel::ModelOverrides AIConfigModel::freshModelOverrides(const AIModelEntry *m) const
{
	ModelOverrides ov;
	if (m)
	{
		ov.useCompleteSchema = m->useCompleteSchema;
		ov.chatLimit         = m->chatLimit;
		ov.chatLimitActive   = m->chatLimitActive;
	}
	return ov;
}
