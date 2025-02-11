#ifndef LANGUAGEMODEL_H
#define LANGUAGEMODEL_H

#include <QAbstractListModel>
#include <QTranslator>
#include <QApplication>
#include <QQmlApplicationEngine>

namespace Modules { class DynamicModule; }


/// Handles language selection in JASP
/// Scans the run folder for qm files etc and handles changes in the preferences.
/// Sends out the right signals to get the whole application to change language.
/// In Desktop it is as easy as setting the new locale and then reloading QML (might not be needed with Qt 6, this was a workaround)
/// The Engines are stopped and restarted with the right language code, because R/gettext only seems to check it during startup (on linux anyway)
class LanguageModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(QString		currentLanguage			READ currentLanguage			WRITE setCurrentLanguage			NOTIFY currentLanguageChanged		)
	Q_PROPERTY(QLocale		currentLocale			READ currentLocale													NOTIFY currentLocaleChanged			)
	Q_PROPERTY(bool			useAlternativeLocale	READ useAlternativeLocale		WRITE setUseAlternativeLocale		NOTIFY useAlternativeLocaleChanged	)
	Q_PROPERTY(QStringList	altLanguages			READ altLanguages													CONSTANT							)
	Q_PROPERTY(QStringList	altTerritories			READ altTerritories													NOTIFY altTerritoriesChanged		)
	Q_PROPERTY(QString		currentAltLanguage		READ currentAltLanguage			WRITE setCurrentAltLanguage			NOTIFY currentAltLanguageChanged	)
	Q_PROPERTY(QString		currentAltTerritory		READ currentAltTerritory		WRITE setCurrentAltTerritory		NOTIFY currentAltTerritoryChanged	)
	Q_PROPERTY(QString		exampleFormatting		READ exampleFormatting												NOTIFY exampleFormattingChanged		)

	struct LanguageInfo
	{
		LanguageInfo(const QLocale& _locale = LanguageModel::_defaultLocale, const QString& _code = getLanguageCode(LanguageModel::_defaultLocale), const QString& _qmFilename = "");
		
		
		static QString				getLanguageCode(const QLocale& locale);
		static bool					isLanguageAllowed(const QString& language) { return _allowedLanguages.contains(language); }
		static QMap<QString, bool>	_allowedLanguages;
		static QString				_incompleteFlag;

		QString						code,		// This is not necessary the same as locale.name(), especially with zh_Hans or zh_Hant
									entryName;	// Name used in the language dropdown
		QLocale						locale;
		QVector<QString>			qmFilenames;
	};

public:
	static LanguageModel * lang() { return _singleton; }

	enum {
		NameRole = Qt::UserRole + 1,
		LabelRole,
		ValueRole,
		NationFlagRole,
		LocalNameRole
	};

	explicit								LanguageModel(QApplication *app = nullptr, QQmlApplicationEngine *qml = nullptr, QObject *parent = nullptr) ;
											~LanguageModel() override { _singleton = nullptr; }

	void									initialize();
	void									refreshAll();
	
	int										rowCount(const QModelIndex & = QModelIndex())				const override { return _languages.size(); }
	int										columnCount(const QModelIndex & = QModelIndex())			const override { return 1; }
	QVariant								data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;

	QHash<int, QByteArray>					roleNames() const override;

	QString									currentLanguageCode()											const	{ return _currentLanguageCode; }
	QString									currentLanguage()												const;
	QLocale									currentLocale()													const;
	bool									hasDefaultLanguage()											const;
	bool									useAlternativeLocale()											const { return _useAlternativeLocale; }
	QString									currentAltLanguage()											const;
	QString									currentAltTerritory()											const;
	QString									exampleFormatting()												const;
	QStringList								altLanguages()													const { return _altLanguages; }
	QStringList								altTerritories()												const { return _altTerritories; }
	

	//This function (currentTranslationSuffix) should be made obsolete through the abolishment of all the _nl etc files:
	static	QString							currentTranslationSuffix()	{ return _singleton->hasDefaultLanguage() ? "" : ("_" + _singleton->currentLanguageCode()); }

	void									setApplicationEngine(QQmlApplicationEngine	 * ae) { _qml = ae; }
	void									setDefaultLocaleFromCurrent();
	
public slots:
	void									setCurrentLanguage(QString language);
	void									setUseAlternativeLocale(bool useIt);
	void									setCurrentAltLanguage(const QString &newCurrentAltLanguage);
	void									setCurrentAltTerritory(const QString &newCurrentAltTerritory);
	void									loadModuleTranslationFiles(Modules::DynamicModule *dyn);
	void									resultsPageLoaded();

signals:
	void									currentLanguageChanged();
	void									currentLocaleChanged(QString);
	void									currentAltLanguageChanged();
	void									currentAltTerritoryChanged();
	void									useAlternativeLocaleChanged();
	void									exampleFormattingChanged();
	void									altTerritoriesChanged();
	void									aboutToChangeLanguage();
	void									languageChangeDone();
	void									pauseEngines(bool unloadData = false);
	void									stopEngines();
	void									resumeEngines();
	
private:
	static LanguageModel *					_singleton;
	static QLocale							_defaultLocale;
	static QLocale							_alternativeLocale;

	void									findQmFiles();
	void									loadQmFilesForLanguage(const QString& languageCode);
	void									loadQmFile(const QString& filename);
	void									removeTranslators();
	bool									isValidLocaleName(const QString& filename, QLocale & loc, QString & languageCode);
	void									fillAltOptions();
	void									fillAltTerritories();
	void									setAlternativeLocaleStatic();

	QApplication						*	_mApp						= nullptr;
	QTranslator							*	_mTranslator				= nullptr;
	QQmlApplicationEngine				*	_qml						= nullptr;
	QString									_currentLanguageCode,
											_qmLocation,
											_currentAltLanguage,
											_currentAltTerritory;
	QMap<QString, LanguageInfo>				_languages;
	QVector<QTranslator *>					_translators;
	bool									_shouldEmitLanguageChanged	= false,
											_useAlternativeLocale;
	QStringList								_altLanguages,
											_altTerritories;
	std::map<QString,QLocale::Language>		_nativeLanguageNameToEnum;
	std::map<QString,QLocale::Territory>	_nativeTerritoryNameToEnum;
	
	
};


#endif // LANGUAGEMODEL_H
