#ifndef JASPCONFIGURATION_H
#define JASPCONFIGURATION_H

#include <QNetworkAccessManager>
#include <QObject>
#include <QVariant>
#include <QFile>
#include <QRegularExpression>
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

	//read and parse local and remote configuration
	void processConfiguration();

public slots:
	void remoteChanged(QString remoteURL);

signals:
	void configurationProcessed(QString results);

private slots:
	void sslErrors(const QList<QSslError> &errors);

private:
	bool processLocal();

	void processTasks();
	void parse(const QString& conf);
	void getVersion(const QString& conf);
	std::shared_ptr<QFile> getLocalConfFile();

	QNetworkAccessManager	_networkManager;
    Version jaspVersion;
    QMap<QString, QVariant> _keyValueConstants;

    const QString configurationFilename = "userConfiguration.conf";


	const QString versionPattern = "JASP_Version:\\s*(?<versionNum>[\\S]+)\\s*$";
	const QString keyValuePattern = "\\s*(?<key>[\\S+)\\s*=\\s*(?<value>[\\S+)\\s*$";

	QRegularExpression versionRE;
	QRegularExpression keyValueRE;

};

#endif // JASPCONFIGURATION_H
