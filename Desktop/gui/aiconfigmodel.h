//
// AIConfigModel — singleton holding AI provider/model configuration.
//
// Owns provider list, model list, current selection, and all current
// connection/advanced settings that the provider+model dropdowns affect.
//
// Access pattern (C++):  AIConfigModel::config()->currentEndpoint()
// Access pattern (QML):  aiConfigModel.currentEndpoint
//

#ifndef AICONFIGMODEL_H
#define AICONFIGMODEL_H

#include <QObject>
#include <QAbstractListModel>
#include <QVector>
#include <QString>
#include <QJsonObject>
#include <QMap>

// ────────────────────────────────────────────────────────────
// Internal data entries
// ────────────────────────────────────────────────────────────

struct AIModelEntry
{
	QString     id;                     // UUID
	QString     name;                   // "DeepSeek V4 Flash"
	QString     model;                  // "deepseek-v4-flash"  (API string)
	QJsonObject extraParams;            // per-model JSON merged into request
	QString     systemPromptPostfix;    // appended after common+persona prompt
	QString     warning;                // shown as red banner in UI; empty = hidden
	bool        useCompleteSchema = true; // include full tool schemas
	int         chatLimit         = 256000;
	bool        chatLimitActive   = true;
	bool        isSystem = true;        // from shipped JSON?
};

struct AIProviderEntry
{
	QString               id;           // UUID
	QString               name;         // "DeepSeek"
	QString               endpoint;     // full chat completions URL
	QString               defaultApiKey;
	bool                  isSystem  = true;   // from shipped JSON?
	QVector<AIModelEntry> models;             // at least 1
};

// ────────────────────────────────────────────────────────────
// List models for the two ComboBox dropdowns
// ────────────────────────────────────────────────────────────

class AIProviderListModel : public QAbstractListModel
{
	Q_OBJECT
public:
	enum Roles {
		IdRole       = Qt::UserRole + 1,
		NameRole,
		EndpointRole,
		IsSystemRole,
		ModelCountRole
	};

	explicit AIProviderListModel(QObject *parent = nullptr);

	void setProviders(const QVector<AIProviderEntry> &providers);
	int  indexOfId(const QString &id) const;

	int      rowCount(const QModelIndex &parent = QModelIndex()) const override;
	QVariant data(const QModelIndex &index, int role) const override;
	QHash<int, QByteArray> roleNames() const override;

private:
	QVector<AIProviderEntry> m_providers;
};

// ────────────────────────────────────────────────────────────

class AIModelListModel : public QAbstractListModel
{
	Q_OBJECT
public:
	enum Roles {
		IdRole             = Qt::UserRole + 1,
		NameRole,
		ModelStringRole,
		ExtraParamsRole,
		PostfixRole,
		IsSystemRole
	};

	explicit AIModelListModel(QObject *parent = nullptr);

	void setModels(const QVector<AIModelEntry> &models);
	void clear();

	int      rowCount(const QModelIndex &parent = QModelIndex()) const override;
	QVariant data(const QModelIndex &index, int role) const override;
	QHash<int, QByteArray> roleNames() const override;

	int  indexOfId(const QString &id) const;

private:
	QVector<AIModelEntry> m_models;
};

// ────────────────────────────────────────────────────────────
// Main config singleton
// ────────────────────────────────────────────────────────────

class AIConfigModel : public QObject
{
	Q_OBJECT

public:
	static AIConfigModel* config() { return s_singleton; }
	explicit AIConfigModel(QObject *parent = nullptr);
	~AIConfigModel() override;

	// ── List models exposed to QML ──────────────────────
	Q_PROPERTY(QObject* providerListModel READ providerListModel CONSTANT)
	Q_PROPERTY(QObject* modelListModel    READ modelListModel    CONSTANT)

	// ── Current selection ───────────────────────────────
	Q_PROPERTY(int currentProviderIndex READ currentProviderIndex
	           WRITE setCurrentProviderIndex NOTIFY currentProviderIndexChanged)
	Q_PROPERTY(int currentModelIndex    READ currentModelIndex
	           WRITE setCurrentModelIndex    NOTIFY currentModelIndexChanged)

	// ── Effective active values ─────────────────────────
	Q_PROPERTY(QString currentEndpoint             READ currentEndpoint
	           WRITE setCurrentEndpoint             NOTIFY currentEndpointChanged)
	Q_PROPERTY(QString currentApiKey               READ currentApiKey
	           WRITE setCurrentApiKey               NOTIFY currentApiKeyChanged)
	Q_PROPERTY(QString currentModel                READ currentModel
	           WRITE setCurrentModel               NOTIFY currentModelChanged)
	Q_PROPERTY(QString currentExtraParams          READ currentExtraParams
	           WRITE setCurrentExtraParams          NOTIFY currentExtraParamsChanged)
	Q_PROPERTY(bool   currentUseCompleteSchema     READ currentUseCompleteSchema
	           WRITE setCurrentUseCompleteSchema     NOTIFY currentUseCompleteSchemaChanged)
	Q_PROPERTY(QString currentSystemPromptPostfix  READ currentSystemPromptPostfix
	           WRITE setCurrentSystemPromptPostfix  NOTIFY currentSystemPromptPostfixChanged)
	Q_PROPERTY(bool   currentChatLimitActive       READ currentChatLimitActive
	           WRITE setCurrentChatLimitActive       NOTIFY currentChatLimitActiveChanged)
	Q_PROPERTY(int    currentChatLimit             READ currentChatLimit
	           WRITE setCurrentChatLimit             NOTIFY currentChatLimitChanged)
	Q_PROPERTY(QString currentMessageExtra         READ currentMessageExtra
	           WRITE setCurrentMessageExtra         NOTIFY currentMessageExtraChanged)
	Q_PROPERTY(QString currentWarning              READ currentWarning              NOTIFY currentWarningChanged)

	// ── Is current provider user-editable? ──────────────
	Q_PROPERTY(bool currentProviderIsUserEditable
	           READ currentProviderIsUserEditable NOTIFY currentProviderChanged)

	// ── Manual setter (not auto-declared via macro) ────
	void setCurrentModel(const QString &v);

	// ── Dropdown value arrays (JASP DropDown convention) ──
	Q_PROPERTY(QVariantList providerValues READ providerValues NOTIFY providerValuesChanged)
	Q_PROPERTY(QVariantList modelValues    READ modelValues    NOTIFY modelValuesChanged)

	// ── Getters/setters (declared for moc/QML) ─────────
	QObject* providerListModel();
	QObject* modelListModel();
	int     currentProviderIndex() const;
	int     currentModelIndex()    const;
	QString currentEndpoint()              const;
	void    setCurrentEndpoint(const QString &v);
	QString currentApiKey()                const;
	void    setCurrentApiKey(const QString &v);
	QString currentModel()                 const;
	QString currentExtraParams()           const;
	void    setCurrentExtraParams(const QString &v);
	bool    currentUseCompleteSchema()     const;
	void    setCurrentUseCompleteSchema(bool v);
	QString currentSystemPromptPostfix()   const;
	void    setCurrentSystemPromptPostfix(const QString &v);
	bool    currentChatLimitActive()       const;
	void    setCurrentChatLimitActive(bool v);
	int     currentChatLimit()             const;
	void    setCurrentChatLimit(int v);
	QString currentMessageExtra()          const;
	void    setCurrentMessageExtra(const QString &v);
	QString currentWarning()               const;
	bool    currentProviderIsUserEditable() const;

	// ── Reset ────────────────────────────────────────────
	Q_INVOKABLE void   resetToDefaults();
	Q_INVOKABLE void   resetCurrentModelToDefaults();

signals:
	void currentProviderIndexChanged();
	void currentModelIndexChanged();
	void currentEndpointChanged();
	void currentApiKeyChanged();
	void currentModelChanged();
	void currentExtraParamsChanged();
	void currentUseCompleteSchemaChanged();
	void currentSystemPromptPostfixChanged();
	void currentChatLimitActiveChanged();
	void currentChatLimitChanged();
	void currentMessageExtraChanged();
	void currentWarningChanged();
	void currentProviderChanged();
	void providerValuesChanged();
	void modelValuesChanged();

private:
	// ── Override helpers ────────────────────────────────
	struct ProviderOverrides {
		QString endpoint;
		QString apiKey;
		QString currentModelId;
		QString customModel;

		QString     systemPromptPostfix;
		bool        systemPromptPostfixSet = false;
		QJsonObject extraParams;
		bool        extraParamsSet         = false;
		bool        useCompleteSchema      = true;
		int         chatLimit              = 256000;
		bool        chatLimitActive        = true;
		QString     messageExtra;
		bool        messageExtraSet        = false;

		bool operator==(const ProviderOverrides &o) const = default;
	};

	struct ModelOverrides {
		QJsonObject extraParams;
		QString     systemPromptPostfix;
		bool        useCompleteSchema = true;
		int         chatLimit         = 256000;
		bool        chatLimitActive   = true;
		QString     messageExtra;
		QString     modelName;

		bool extraParamsSet         = false;
		bool systemPromptPostfixSet = false;
		bool messageExtraSet        = false;
		bool modelNameSet           = false;

		bool operator==(const ModelOverrides &o) const = default;
	};

	// ── Init ────────────────────────────────────────────
	void addCustomProvider();
	void loadShippedProviders();
	void loadUserData();
	void saveUserData();
	ModelOverrides freshModelOverrides(const AIModelEntry *m) const;

	// ── Values array getters ────────────────────────────
	QVariantList providerValues() const;
	QVariantList modelValues()    const;

	// ── Selection helpers ───────────────────────────────
	void setCurrentProviderIndex(int i);
	void setCurrentModelIndex(int i);
	void emitAllDerivedSignals();

	const AIProviderEntry* currentProvider() const;
	const AIModelEntry*    currentModelEntry() const;

	// ── Members ─────────────────────────────────────────
	QVector<AIProviderEntry>          m_providers;
	QVector<AIProviderEntry>          m_shipped;
	QMap<QString, ProviderOverrides>  m_providerOverrides;
	QMap<QString, ModelOverrides>     m_modelOverrides;

	int m_currentProviderIndex = -1;
	int m_currentModelIndex    = -1;

	AIProviderListModel* m_providerListModel = nullptr;
	AIModelListModel*    m_modelListModel    = nullptr;

	static AIConfigModel* s_singleton;
};

#endif // AICONFIGMODEL_H
