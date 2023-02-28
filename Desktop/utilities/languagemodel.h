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
	Q_PROPERTY(QString		currentLanguage		READ currentLanguage			WRITE setCurrentLanguage			NOTIFY currentLanguageChanged)

	struct LanguageInfo
	{
		static QString getLanguageCode(const QLocale& locale);
		static bool isLanguageAllowed(const QString& language) { return _allowedLanguages.contains(language); }

		LanguageInfo(const QLocale& _locale = LanguageModel::_defaultLocale, const QString& _code = getLanguageCode(LanguageModel::_defaultLocale), const QString& _qmFilename = "");

		static QMap<QString, bool> _allowedLanguages;
		static QString				_incompleteFlag;

		QString				code;		// This is not necessary the same as locale.name(), especially with zh_Hans or zh_Hant
		QString				entryName;	// Name used in the language dropdown
		QLocale				locale;
		QVector<QString>	qmFilenames;
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

	explicit LanguageModel(QApplication *app = nullptr, QQmlApplicationEngine *qml = nullptr, QObject *parent = nullptr) ;
	~LanguageModel() override { _singleton = nullptr; }

	int						rowCount(const QModelIndex & = QModelIndex())				const override { return _languages.size(); }
	int						columnCount(const QModelIndex & = QModelIndex())			const override { return 1; }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;

	QHash<int, QByteArray>	roleNames() const override;

	QString currentLanguageCode()											const	{ return _currentLanguageCode; }
	QString currentLanguage()												const;
	bool	hasDefaultLanguage()											const;

	//This function (currentTranslationSuffix) should be made obsolete through the abolishment of all the _nl etc files:
	static	QString	currentTranslationSuffix()	{ return _singleton->hasDefaultLanguage() ? "" : ("_" + _singleton->currentLanguageCode()); }

	void setApplicationEngine(QQmlApplicationEngine	 * ae) { _qml = ae; }
	void initialize();


public slots:
	void setCurrentLanguage(QString language);
	void loadModuleTranslationFiles(Modules::DynamicModule *dyn);
	void resultsPageLoaded();

signals:
	void currentLanguageChanged();
	void aboutToChangeLanguage();
	void pauseEngines(bool unloadData = false);
	void stopEngines();
	void resumeEngines();

private:
	static LanguageModel *		_singleton;
	static QLocale				_defaultLocale;

	void					findQmFiles();
	void					loadQmFilesForLanguage(const QString& languageCode);
	void					loadQmFile(const QString& filename);
	void					removeTranslators();
	bool					isValidLocaleName(const QString& filename, QLocale & loc, QString & languageCode);

	QApplication					* _mApp						= nullptr;
	QTranslator						* _mTranslator				= nullptr;
	QQmlApplicationEngine			* _qml						= nullptr;
	QString							_currentLanguageCode,
									_qmLocation;
	QMap<QString, LanguageInfo>		_languages;
	QVector<QTranslator *>			_translators;
	bool							_shouldEmitLanguageChanged	= false;
};


#endif // LANGUAGEMODEL_H
