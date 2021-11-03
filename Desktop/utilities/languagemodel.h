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
		LanguageInfo(const QLocale& _locale = LanguageModel::_defaultLocale, const QString& _qmFilename = "", bool _isComplete = true);

		QLocale				locale;
		QVector<QString>	qmFilenames;
		bool				isComplete;
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

	QString languageCode(const QLocale& locale)		const;
	QString languageCode(const QString& language)	const;
	QString	languageName(const QLocale& loc)		const;
	QString currentLanguageCode()					const	{ return languageCode(_currentLanguage); }
	QString currentLanguage()						const;
	bool	hasDefaultLanguage()					const;

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
	static LanguageModel	* _singleton;
	static QLocale			_defaultLocale;
	static QMap<QString, bool> _allowedLanguages;
	static QString			_incompleteFlag;

	void					findQmFiles();
	void					loadQmFilesForLanguage(const QString& language);
	void					loadQmFile(const QString& filename);
	void					removeTranslators();
	bool					isValidLocaleName(const QString& filename, QLocale & loc);

	QApplication					* _mApp						= nullptr;
	QTranslator						* _mTranslator				= nullptr;
	QQmlApplicationEngine			* _qml						= nullptr;
	QString							_currentLanguage,
									_qmLocation;
	QMap<QString, LanguageInfo>		_languages;
	QVector<QTranslator *>			_translators;
	bool							_shouldEmitLanguageChanged	= false;
};


#endif // LANGUAGEMODEL_H
