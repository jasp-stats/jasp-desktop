#ifndef LANGUAGEMODEL_H
#define LANGUAGEMODEL_H

#include <QAbstractListModel>
#include <QTranslator>
#include <QApplication>
#include <QQmlApplicationEngine>

namespace Modules { class DynamicModule; }

class LanguageModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(QString		currentLanguage		READ currentLanguage			WRITE setCurrentLanguage			NOTIFY currentLanguageChanged)

	struct LanguageInfo
	{
		LanguageInfo(const QLocale& _locale = LanguageModel::_defaultLocale, const QString& _qmFilename = "");

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

	QString languageCode(const QLocale& locale)		const;
	QString languageCode(const QString& language)	const;
	QString	languageName(const QLocale& loc)		const;
	QString currentLanguageCode()					const	{ return languageCode(_currentLanguage); }
	QString currentLanguage()						const	{ return _currentLanguage; }

	static	bool	hasDefaultLanguage()		{ return _singleton->currentLanguage() == _singleton->languageName(_defaultLocale); }
	//This function (currentTranslationSuffix) should be made obsolete through the abolishment of all the _nl etc files:
	static	QString	currentTranslationSuffix()	{ return hasDefaultLanguage() ? "" : ("_" + _singleton->currentLanguageCode()); }

	void setApplicationEngine(QQmlApplicationEngine	 * ae) { _qml = ae; }
	void initialize();


public slots:
	void setCurrentLanguage(QString language);
	void loadModuleTranslationFiles(Modules::DynamicModule *dyn);
	void resultsPageLoaded();

signals:
	void currentLanguageChanged();
	void aboutToChangeLanguage();
	void pauseEngines();
	void stopEngines();
	void resumeEngines();

private:
	static LanguageModel	* _singleton;
	static QLocale			_defaultLocale;

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
