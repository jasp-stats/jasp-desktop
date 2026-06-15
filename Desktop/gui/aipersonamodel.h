//
// AIPersonaModel — QAbstractListModel holding user+system personas for the AI chat feature.
//
// Personas come from two sources:
//   1. Resources/defaultPersonas.json  — shipped, read-only, updates with each release
//   2. QSettings AI_USER_PERSONAS       — user-created, editable
//
// Active persona is tracked by UUID in QSettings AI_CURRENT_PERSONA_ID.
//

#ifndef AIPERSONAMODEL_H
#define AIPERSONAMODEL_H

#include <QAbstractListModel>
#include <QVector>
#include <QString>
#include <QVariantList>
#include <QUuid>

struct PersonaEntry
{
	QString id;
	QString name;
	QString personaPrompt;
	QString imagePath;   // absolute path or empty (then fallback icon is shown)
	bool    isSystem = false;
	QStringList enabledTools; // empty = use default set; ["*"] = all tools
	QStringList enabledCapabilities; // cap IDs explicitly enabled/disabled by user

	bool operator==(const PersonaEntry &other) const { return id == other.id; }
	bool operator!=(const PersonaEntry &other) const { return !(*this == other); }
};

class AIPersonaModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int			currentPersonaIndex		READ currentPersonaIndex	WRITE setCurrentPersonaIndex	NOTIFY currentPersonaIndexChanged)
	Q_PROPERTY(QString		activePersonaAvatar		READ activePersonaAvatar									NOTIFY activePersonaAvatarChanged)
	Q_PROPERTY(QString		activePersonaAvatarWeb	READ activePersonaAvatarWeb									NOTIFY activePersonaAvatarChanged)

public:
	enum Roles {
		IdRole             = Qt::UserRole + 1,
		NameRole,
		NameDisplayRole,
		PersonaPromptRole,
		ImagePathRole,
		IsSystemRole,
		EnabledToolsRole,
		EnabledCapabilitiesRole
	};

	explicit AIPersonaModel(QObject *parent = nullptr);

	// QAbstractListModel
	int                    rowCount(const QModelIndex &parent = QModelIndex()) const override;
	QVariant               data(const QModelIndex &index, int role) const override;
	bool                   setData(const QModelIndex &index, const QVariant &value, int role) override;
	QHash<int, QByteArray> roleNames() const override;

	// Persona CRUD (Q_INVOKABLE for QML)
	Q_INVOKABLE int		addPersona();
	Q_INVOKABLE void	removePersona(int index);
	Q_INVOKABLE void	duplicatePersona(int index);
	Q_INVOKABLE int		getRole(QString name);

	/// Restore a system persona to its shipped defaults.
	Q_INVOKABLE void resetSystemPersona(int index);

	/// Copy an image file into the personas directory, return the absolute path.
	Q_INVOKABLE QString copyImageToPersonasDir(const QUrl &sourceUrl);

	QUrl resolvedImageUrl(const QString &imagePath) const;

	// Accessors (for C++ consumers like AiBridge)
	const PersonaEntry &activePersona() const;

	int currentPersonaIndex() const;
	QString activePersonaAvatar() const;
	QString activePersonaAvatarWeb() const;

	/// Return the default tool set (all known tools).
	QStringList defaultToolSet() const;

	/// Return the effective enabled tools for the persona at index.
	/// If the persona has its own list, return that; otherwise return defaultToolSet().
	/// A list containing "*" means all known tools.
	QStringList effectiveEnabledTools(int index) const;

	/// All known tool names from JaspRpcDispatcher.
	Q_INVOKABLE QStringList allKnownToolNames() const;

	/// Human-readable display name for a tool, falling back to its RPC name.
	Q_INVOKABLE QString toolDisplayName(const QString &methodName) const;

	/// Toggle a single capability. Recalculates tools.
	Q_INVOKABLE void toggleCapability(int personaIndex, const QString &capId);

	/// Toggle a single tool. Recalculates caps.
	Q_INVOKABLE void toggleTool(int personaIndex, const QString &toolName);

	/// Return the list of enabled capability IDs for a persona.
	Q_INVOKABLE QStringList enabledCapabilityIds(int personaIndex);

	Q_INVOKABLE QVariantList capabilities()	const { return m_capabilities; }

public slots:
	void setCurrentPersonaIndex(int index);

	// Persistence
	void saveToSettings();
	void resetAll();             // clear user personas, reload system defaults

signals:
	void currentPersonaIndexChanged();
	void activePersonaAvatarChanged();

private:
	int				indexOfId(const QString &id) const;
	void			mergeLists();
	void			insertPersona(int pos, const PersonaEntry &entry);
	QString			resolveDefaultImage()	const;
	QString			personaImagesDir()		const;
	void			loadPersonaSettings();
	void			loadCapabilities();
	QStringList		resolveCapabilitiesToTools(const QJsonArray &capsArr)	const;
	QStringList		getAllCapabilityIds()									const;
	QStringList		resolveCaps(const QStringList &tools)					const;

	QVector<PersonaEntry>	m_systemPersonas;
	QVector<PersonaEntry>	m_userPersonas;
	QVector<PersonaEntry>	m_personas;         // merged: system first, then user
	int						m_currentPersonaIndex	= 0;
	bool					m_loaded				= false;
	QVariantList			m_capabilities;
};

#endif // AIPERSONAMODEL_H
