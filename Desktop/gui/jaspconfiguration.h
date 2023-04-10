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
	std::shared_ptr<QFile> getLocalConfFile(bool truncate = false);

	void parse(QString conf);
	void parseVersion(QString& conf);
	void parseModulesToLoad(QString& conf);
	void parseModuleStatements(QString& conf);
	void parseAnalysisStatements(QString& conf, const QString& moduleName);
	void parseKeyValuePairs(const QString& conf, const QString moduleName = "", const QString analysisName = "");

	QNetworkAccessManager	_networkManager;

	Version _jaspVersion;
	QMap<QString, QMap<QString, QMap<QString, QVariant>>> _definedConstants;
	QStringList _modulesToLoad;

    const QString configurationFilename = "userConfiguration.conf";


	const QString versionPattern = "^\\s*JASP_Version:\\s*(?<versionNum>\\S+)\\s*$";
	const QString keyValuePattern = "^\\s*(?<key>\\S+)\\s*=\\s*(?<value>\\S+)\\s*$";
	const QString moduleSectionPattern = "^\\s*Module\\s*(?<name>\\S+)\\s*$(?<section>.*)\\s*END\\s*Module\\s*$";
	const QString analysisSectionPattern = "^\\s*Analysis\\s*(?<name>\\S+)\\s*$(?<section>.*)\\s*END\\s*Analysis\\s*$";
	const QString loadModulesPattern = "^\\s*Load\\s*Modules\\s*:\\s*(?<list>\\w+(\\s*,\\s*\\w*)*)?\\s*$";



	QRegularExpression versionRE;
	QRegularExpression keyValueRE;
	QRegularExpression loadModulesRE;
	QRegularExpression moduleStatementRE;
	QRegularExpression analysisStatementRE;

};

#endif // JASPCONFIGURATION_H
