#ifndef LANGUAGEMODEL_H
#define LANGUAGEMODEL_H

#include <QAbstractListModel>
#include <QTranslator>
#include <QApplication>
#include <QQuickItem>
#include <QQmlApplicationEngine>
#include <QVector>
#include "modules/dynamicmodule.h"
#include "gui/preferencesmodel.h"

struct LanguageInfo {


	LanguageInfo();
	LanguageInfo(QLocale::Language language, QString languageName, QString nativeLanguageName, QString localName, QString qmFilename, QString qmFolder);


	QLocale::Language	language			= QLocale::English; //e.g. QLocale::English
	QString				languageName		= "",
						nativeLanguageName	= "",	//e.g. English
						localName			= "",
						qmFolder			= "";	//e.g. en_US
	QVector<QString>	qmFilenames;				//e.g. jasp_en.qm

	QString toString();
};

class LanguageModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int		currentIndex		READ currentIndex			WRITE setCurrentIndex			NOTIFY currentIndexChanged)
	Q_PROPERTY(QString	currentLanguageCode READ currentLanguageCode									NOTIFY currentIndexChanged)

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

	int						rowCount(const QModelIndex &parent = QModelIndex())			const override { return _languages.size(); }
	int						columnCount(const QModelIndex & = QModelIndex())			const override { return 1; }
	QVariant				data(const QModelIndex &index, int role = Qt::DisplayRole)	const override;

	QHash<int, QByteArray>	roleNames() const override;

	int		currentIndex()			const { return _currentIndex; }
	QString currentLanguageCode()	const;

	//This function (currentTranslationSuffix) should be made obsolete through the abolishment of all the _nl etc files:
	static			QString			currentTranslationSuffix()	{ return currentLanguageInfo().language  == QLocale::English ? "" : ("_" + currentLanguageInfo().localName); }

	void setApplicationEngine(QQmlApplicationEngine	 * ae) { _qml = ae; }
	void initialize();


public slots:
	void changeLanguage(int index);
	void setCurrentIndex(int currentIndex);
	void loadModuleTranslationFile(Modules::DynamicModule *dyn);
	void resultsPageLoaded();

signals:
	void currentIndexChanged();
	void languageChanged();

private:
	static const	LanguageInfo &	currentLanguageInfo()		{ return _singleton->_currentLanguageInfo; }

	void		findQmFiles(QString qmlocation);
	void		loadQmFilesForLanguage(QLocale::Language cl);
	void		loadQmFile(QString filename);
	void		removeTranslators();

	QString		getLocalName(QLocale::Language cl) const;
	QString		getNativeLanguaName(QLocale::Language cl) const;
	QString		getLocalNameFromQmFileName(QString filename) const;

	bool		isValidLocalName(QString filename, QLocale::Language & lang);
	bool		isValidLocalName(QString filename, QLocale & loc);
	bool		isJaspSupportedLanguage(QLocale::Language lang) { return _languages.count(lang) > 0;}

	static LanguageModel	* _singleton;
	QApplication			* _mApp			= nullptr;
	QTranslator				* _mTranslator	= nullptr;
	QQmlApplicationEngine	* _qml			= nullptr;

	LanguageInfo							_currentLanguageInfo; //I am quite unhappy with the fact that we store this info twice, is there a scenario where they need to be different or are in fact allowed to be? Why does it need to be inside this (previously static, publicyly accessible) copy of the info and *also* store the _currentIndex in a list... But I won't change it now as we are about to release.
	QMap<QLocale::Language, LanguageInfo>	_languagesInfo;
	QVector<QLocale::Language>				_languages;
	QVector<QTranslator *>					_translators;

	QString		_qmlocation;

	int			_currentIndex;
	bool		_shouldEmitLanguageChanged = false;
};


#endif // LANGUAGEMODEL_H
