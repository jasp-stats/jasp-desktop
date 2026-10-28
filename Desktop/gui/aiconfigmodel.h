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
	// Auth is described by protocol, not by vendor, so supporting another
	// identity provider is configuration rather than new fields:
	//   scheme = authMode   who = authAuthority   app = authClientId
	//   what   = authScope  how = authBackend     wire = authHeaderName/Prefix
	QString               authMode;         // "apiKey" (default when empty) | "oidc" | "none"
	QString               authAuthority;    // OIDC authority: tenant id, "organizations", or issuer URL
	QString               authScope;        // resource the token is requested for
	QString               authClientId;     // app registration; empty = built-in JASP client id
	QString               authBackend;      // "auto" (default when empty) | wam | browser | devicecode
	QString               authHeaderName;   // empty = "Authorization"
	QString               authHeaderPrefix; // empty = "Bearer " for Authorization, raw otherwise
	int                   authRedirectPort = 0;  // loopback redirect port; 0 = ephemeral (Okta needs a fixed one)
	QString               authAuthorizationUrl;  // empty = derive from the authority (discovery / Entra convention)
	QString               authTokenUrl;          // empty = likewise; must be set together with the above
	QString               authTokenType;         // "" / "access_token" (default) | "id_token" — which JWT is the bearer
	QString               authOfflineAccess;     // "" / "on" (default) = request offline_access | "off" = exactly the configured scopes
	QString               extraHeaders;          // raw JSON object of extra HTTP headers on every request; empty = none. Routing only — never credentials
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

	// ── Authentication (scheme | authority | scope | backend | wire) ─────
	Q_PROPERTY(QString currentAuthMode         READ currentAuthMode
	           WRITE setCurrentAuthMode         NOTIFY currentAuthModeChanged)

	// ── Which connection method the AI page shows (the tab): "apiKey"
	// (default) or "oidc". Stored so JASP reopens where the user left off,
	// and so a group policy can pin it — Settings::value() prefers
	// HKCU/HKLM Software\Policies\JASP over the user's own setting.
	// Kept consistent with the active provider: changing either follows
	// the other.
	Q_PROPERTY(QString authMode READ authMode WRITE setAuthMode NOTIFY authModeChanged)
	Q_PROPERTY(QString currentAuthAuthority    READ currentAuthAuthority
	           WRITE setCurrentAuthAuthority    NOTIFY currentAuthAuthorityChanged)
	Q_PROPERTY(QString currentAuthScope        READ currentAuthScope
	           WRITE setCurrentAuthScope        NOTIFY currentAuthScopeChanged)
	Q_PROPERTY(QString currentAuthClientId     READ currentAuthClientId
	           WRITE setCurrentAuthClientId     NOTIFY currentAuthClientIdChanged)
	Q_PROPERTY(int    currentAuthRedirectPort  READ currentAuthRedirectPort
	           WRITE setCurrentAuthRedirectPort NOTIFY currentAuthRedirectPortChanged)
	Q_PROPERTY(QString currentAuthAuthorizationUrl READ currentAuthAuthorizationUrl
	           WRITE setCurrentAuthAuthorizationUrl NOTIFY currentAuthAuthorizationUrlChanged)
	Q_PROPERTY(QString currentAuthTokenUrl     READ currentAuthTokenUrl
	           WRITE setCurrentAuthTokenUrl     NOTIFY currentAuthTokenUrlChanged)
	Q_PROPERTY(QString currentAuthTokenType    READ currentAuthTokenType
	           WRITE setCurrentAuthTokenType    NOTIFY currentAuthTokenTypeChanged)
	Q_PROPERTY(QString currentAuthOfflineAccess READ currentAuthOfflineAccess
	           WRITE setCurrentAuthOfflineAccess NOTIFY currentAuthOfflineAccessChanged)
	Q_PROPERTY(QString authDiscoveryMessage READ authDiscoveryMessage WRITE setAuthDiscoveryMessage NOTIFY authDiscoveryMessageChanged)
	Q_PROPERTY(QString currentExtraHeaders     READ currentExtraHeaders
	           WRITE setCurrentExtraHeaders     NOTIFY currentExtraHeadersChanged)
	Q_PROPERTY(QString currentAuthBackend      READ currentAuthBackend
	           WRITE setCurrentAuthBackend      NOTIFY currentAuthBackendChanged)
	Q_PROPERTY(QString currentAuthHeaderName   READ currentAuthHeaderName
	           WRITE setCurrentAuthHeaderName   NOTIFY currentAuthHeaderNameChanged)
	Q_PROPERTY(QString currentAuthHeaderPrefix READ currentAuthHeaderPrefix
	           WRITE setCurrentAuthHeaderPrefix NOTIFY currentAuthHeaderPrefixChanged)

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
	QString currentAuthMode()              const;
	void    setCurrentAuthMode(const QString &v);
	QString authMode()                     const;
	void    setAuthMode(const QString &v);
	QString currentAuthAuthority()         const;
	void    setCurrentAuthAuthority(const QString &v);
	QString currentAuthScope()             const;
	void    setCurrentAuthScope(const QString &v);
	QString currentAuthClientId()          const;
	void    setCurrentAuthClientId(const QString &v);
	int     currentAuthRedirectPort()      const;
	void    setCurrentAuthRedirectPort(int v);
	QString currentAuthAuthorizationUrl() const;
	void    setCurrentAuthAuthorizationUrl(const QString &v);
	QString currentAuthTokenUrl()          const;
	void    setCurrentAuthTokenUrl(const QString &v);
	QString currentAuthTokenType()         const;
	void    setCurrentAuthTokenType(const QString &v);
	QString currentAuthOfflineAccess()    const;
	void    setCurrentAuthOfflineAccess(const QString &v);

	/// Fetch the authority's /.well-known/openid-configuration and fill the
	/// authorization and token URL fields from it. Button-triggered (PrefsAI):
	/// endpoint discovery is a configuration-time action, never something the
	/// sign-in flow does silently. Status lands in authDiscoveryMessage.
	Q_INVOKABLE void discoverAuthEndpoints();

	/// Result of the last discoverAuthEndpoints() — "Discovering…", the
	/// discovered endpoints, or the reason it failed. Empty before first use.
	/// Writable so the UI can stage guidance text (e.g. from the provider picker).
	QString authDiscoveryMessage() const;
	void    setAuthDiscoveryMessage(const QString &message);
	QString currentExtraHeaders()         const;
	void    setCurrentExtraHeaders(const QString &v);
	QString currentAuthBackend()           const;
	void    setCurrentAuthBackend(const QString &v);
	QString currentAuthHeaderName()        const;
	void    setCurrentAuthHeaderName(const QString &v);
	QString currentAuthHeaderPrefix()      const;
	void    setCurrentAuthHeaderPrefix(const QString &v);
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
	void currentAuthModeChanged();
	void authModeChanged();
	void currentAuthAuthorityChanged();
	void currentAuthScopeChanged();
	void currentAuthClientIdChanged();
	void currentAuthRedirectPortChanged();
	void currentAuthAuthorizationUrlChanged();
	void currentAuthTokenUrlChanged();
	void currentAuthTokenTypeChanged();
	void currentAuthOfflineAccessChanged();
	void authDiscoveryMessageChanged();
	void currentExtraHeadersChanged();
	void currentAuthBackendChanged();
	void currentAuthHeaderNameChanged();
	void currentAuthHeaderPrefixChanged();
	void currentProviderChanged();
	void providerValuesChanged();
	void modelValuesChanged();

private:
	// ── Override helpers ────────────────────────────────
	struct ProviderOverrides {
		QString endpoint;
		// Legacy carrier for API keys written before SecretVault existed. Read
		// once by the migration at the end of loadUserData(); never written.
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

		// Auth overrides — empty means "not overridden".
		QString     authMode;
		QString     authAuthority;
		QString     authScope;
		QString     authClientId;
		QString     authBackend;
		QString     authHeaderName;
		QString     authHeaderPrefix;
		int         authRedirectPort = 0;
		QString     authAuthorizationUrl;
		QString     authTokenUrl;
		QString     authTokenType;
		QString     authOfflineAccess;
		QString     extraHeaders;

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

	/// Make the active provider match the stored authMode — switch to the last
	/// provider of that kind, or revert the mode when none exists.
	void applyAuthModeToSelection();
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

	// ── Members ─────────────────────────────────
	QVector<AIProviderEntry>          m_providers;
	QVector<AIProviderEntry>          m_shipped;
	QMap<QString, ProviderOverrides>  m_providerOverrides;
	QMap<QString, ModelOverrides>     m_modelOverrides;
	QString                            m_authDiscoveryMessage;

	int m_currentProviderIndex = -1;
	int m_currentModelIndex    = -1;

	AIProviderListModel* m_providerListModel = nullptr;
	AIModelListModel*    m_modelListModel    = nullptr;

	static AIConfigModel* s_singleton;
};

#endif // AICONFIGMODEL_H
