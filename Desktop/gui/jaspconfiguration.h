#ifndef JASPSETTINGS_H
#define JASPSETTINGS_H

#include <QNetworkAccessManager>
#include <QObject>
#include <QVariant>
#include "qregularexpression.h"
#include "version.h"

class JASPConfiguration : public QObject
{
	Q_OBJECT
public:
    explicit JASPConfiguration(QObject *parent = nullptr);

	//QML programming constants interface
	Q_INVOKABLE bool exists(const QString& constant, const QString& module = "", const QString& analysis = "");
	Q_INVOKABLE QVariant get(const QString& constant, QVariant defaultValue = QVariant(), const QString& module = "", const QString& analysis = "");

	//Predefined analysis options interface
	bool isSet(const QString& module, const QString& analysis, const QString& optionName);
	QString& getAnalysisOptionValue(const QString& module, const QString& analysis, const QString& optionName, const QString& defaultValue = "");

    //read and parse local and remote settings
	void processSettings();

public slots:
	void remoteChanged(QString remoteURL);

signals:
    void settingsProcessed(QString results);

private slots:
	void sslErrors(const QList<QSslError> &errors);

private:
	void processTasks();
	void parse(const QString& settings);
    bool getVersion(const QString& settings);

	QNetworkAccessManager	_networkManager;
    Version jaspVersion;
	QSet<QString> _patches;

    const QString configurationFilename = "userConfiguration.conf";

};

#endif // JASPSETTINGS_H
