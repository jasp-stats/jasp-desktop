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

	LanguageInfo(QLocale::Language lang, QString name, QString nativeName, QString local, QString qmfile, QString qmfolder)
	{
		language = lang;
		languageName = name; //Language Name
		nativeLanguageName = nativeName; //Name of the language in native language
		localName = local; //QLocale::system().name(); // e.g. "nl_NL" then truncated to "nl"
		qmFilenames.push_back(qmfile) ;		
		qmFolder = qmfolder; //QTranslator::load(const QString &filename, const QString &directory = QString()
	}

	LanguageInfo()
	{
		language = QLocale::English;
		languageName = ""; //Language Name
		nativeLanguageName = ""; //Name of the language in native language
		localName = ""; //QLocale::system().name(); // e.g. "nl_NL" then truncated to "nl"
		qmFilenames.clear();
		qmFolder = ""; //QTranslator::load(const QString &filename, const QString &directory = QString()

	}

	QLocale::Language language; //e.g. QLocale::English
	QString languageName;
	QString nativeLanguageName; //e.g. English
	QString localName; //e.g. en_US
	QVector<QString> qmFilenames; //e.g. jasp_en.qm	
	QString qmFolder;

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

	explicit LanguageModel(QString qsources, QApplication *app = nullptr, QQmlApplicationEngine *qml = nullptr, QObject *parent = nullptr) ;

	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	int	columnCount(const QModelIndex &parent = QModelIndex())		const override { return 1; }

	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

	QHash<int, QByteArray>			roleNames() const override;	

	int currentIndex() const;

	static LanguageInfo CurrentLanguageInfo;
	static QString getCurrentLanguageFileExtension();

	void setApplicationEngine(QQmlApplicationEngine	 * ae);	
	void initialize();

	QString currentLanguageCode() const;

public slots:
	void changeLanguage(int index);
	void setCurrentIndex(int currentIndex);
	void loadModuleTranslationFile(Modules::DynamicModule *dyn);	

signals:
	void currentIndexChanged();
	void languageChanged();

private:

	void findQmFiles(QString qmlocation);
	void loadQmFilesForLanguage(QLocale::Language cl);
	void loadQmFile(QString filename);
	void removeTranslators();

	QString getLocalName(QLocale::Language cl) const;
	QString getNativeLanguaName(QLocale::Language cl) const;

	QLocale::Language getLanguageKeyFromName(QString lname) const;
	QString  getLocalNameFromQmFileName(QString filename) const;
	bool isValidLocalName(QString filename, QLocale::Language & lang);
	bool isValidLocalName(QString filename, QLocale & loc);
	bool isJaspSupportedLanguage(QLocale::Language lang);

	static LanguageModel * _singleton;

	QApplication *_mApp = nullptr;
	QTranslator *_mTransLator = nullptr;
	QQmlApplicationEngine *_qml = nullptr;
	QObject *_parent = nullptr;

	QMap<QLocale::Language, LanguageInfo> _languagesInfo;
	QVector<QLocale::Language> _languages;
	QVector<QTranslator *> _translators;

	QString _qmlocation;

	int _currentIndex;
};


#endif // LANGUAGEMODEL_H
