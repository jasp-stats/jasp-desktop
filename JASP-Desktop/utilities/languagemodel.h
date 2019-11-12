#ifndef LANGUAGEMODEL_H
#define LANGUAGEMODEL_H

#include <QAbstractListModel>
#include <QTranslator>
#include <QApplication>
#include <QQuickItem>
#include <QQmlApplicationEngine>
#include <QVector>

struct LanguageInfo {

	LanguageInfo(QString lname, QString local, QString qmfile)
	{
		nativeLanguageName=lname; //Translated name of the language
		localName=local; //QLocale::system().name(); // e.g. "nl_NL" then truncated to "nl"
		qmFilenames.push_back(qmfile) ;
	}

	LanguageInfo()
	{
		nativeLanguageName=""; //Translated name of the language
		localName=""; //QLocale::system().name(); // e.g. "nl_NL" then truncated to "nl"
		qmFilenames.clear();
	}

	QString nativeLanguageName; //e.g. English
	QString localName; //e.g. en_US
	QVector<QString> qmFilenames; //e.g. jasp_en.qm
};

class LanguageModel : public QAbstractListModel
{
	Q_OBJECT
	Q_PROPERTY(int		currentIndex	READ currentIndex	WRITE setCurrentIndex	NOTIFY currentIndexChanged)
	Q_PROPERTY(QString	emptyString		READ getEmptyString							NOTIFY languageChanged)

public:

	enum {
		NameRole = Qt::UserRole + 1,
		LabelRole,
		ValueRole,
		NationFlagRole,
		LocalNameRole
	};

	explicit LanguageModel(QString qsources, QApplication *app = nullptr, QObject *parent = nullptr) ;

	int rowCount(const QModelIndex &parent = QModelIndex()) const override;
	int	columnCount(const QModelIndex &parent = QModelIndex())		const override { return 1; }

	QVariant data(const QModelIndex &index, int role = Qt::DisplayRole) const override;

	QHash<int, QByteArray>			roleNames() const override;	

	int currentIndex() const;

	QString getEmptyString() const;
	static LanguageInfo CurrentLanguageInfo;

	void setApplicationEngine(QQmlApplicationEngine	 * ae);

public slots:
	void changeLanguage(int index);
	void setCurrentIndex(int currentIndex);

signals:
	void currentIndexChanged(int currentIndex);
	void languageChanged();

private:

	QStringList findQmFiles();

	QString getLocalName(QLocale::Language cl) const;
	QString getNativeLanguaName(QLocale::Language cl) const;

	QLocale::Language getLanguageKeyFromName(QString lname) const;
	QString  getLocalNameFromQmFileName(QString filename) const;
	QString getQmTranslationName(QLocale::Language cl) const;

	QGuiApplication *_mApp = nullptr;
	QTranslator *_mTransLator = nullptr;

	QMap<QLocale::Language, LanguageInfo> _languagesInfo;
	QVector<QLocale::Language> _languages;
	QObject *_parent;
	QString _qmlocation;

	int m_currentIndex;

	QString m_emptyString;
	QQmlApplicationEngine	* _qml = nullptr;

};


#endif // LANGUAGEMODEL_H
