/*! @file altnavigation.h
 *  @brief Defines a singleton that can be commanded to gather and parse local and remote configuration files.
 *  It provides an interface that allows querying of the configuration data from both qml and cpp environments.
 *  Configuration includes: Modules to enable, analysis options, qml runtime constants, startup commands, etc.
 *
 *  Defines a singleton that can be commanded to gather and parse local and remote configuration files.
 *  It provides an interface that allows querying of the configuration data from both qml and cpp environments.
 *  Configuration includes: Modules to enable, analysis options, qml runtime constants, startup commands,etc.
 *
 *	This subsystem will parse the local configuration file.
 *	Additionally when a remote is provided in settings, the remote configuration and override
 *	the local configuration file with the remote as a local cache (in case of offline JASP use)
 *
 *	This subsystem is made of this frontend class along with a Parser base class.
 *	This class contains a factory function and associated enum so that additional formats can be added easily.
 *	The parser classes make use of the interface of the JASPConfiguration class to fill in the internal data structures.
 *
 *
 *  @author Rens Dofferhoff
 */

#ifndef JASPCONFIGURATION_H
#define JASPCONFIGURATION_H

#include <QNetworkAccessManager>
#include <QObject>
#include <QVariant>
#include <QFile>
#include "version.h"
#include "json/json.h"

class JASPConfiguration : public QObject
{
	Q_OBJECT
public:
	//read and parse local and remote configuration
	void processConfiguration();

	//QML programming constants interface
	Q_INVOKABLE bool constantExists(const QString& constant, const QString& module = "", const QString& analysis = "");
	Q_INVOKABLE QVariant getConstant(const QString& constant, const QVariant& defaultValue = QVariant(), const QString& module = "", const QString& analysis = "");

	//C++ configuration access functions
	bool optionSet(const QString& module, const QString& analysis, const QString& optionName);
	bool optionsSet(const QString& module, const QString& analysis);
	bool optionLocked(const QString& module, const QString& analysis, const QString& optionName);
	Json::Value getAnalysisOptionValues(const QString& module, const QString& analysis);
	const QStringList* getAdditionalModules() { return &_modulesToLoad; }
	QString getStartupCommands() { return _startupCommands; }


	//Parser set functions
	bool addConstant(QString key, QVariant value, QString moduleName = "", QString analysisName = "");
	bool addOption(QString key, QVariant value, bool locked, QString moduleName = "", QString analysisName = "");
	void setAdditionalModule(const QString& module) { _modulesToLoad.push_back(module); };
	void setAdditionalModules(const QStringList& modules) { _modulesToLoad += modules; };
	void setStartupCommands(const QString& commands) { _startupCommands += commands; };
	void setJASPVersion(const Version& v) { _jaspVersion = v; };


	//singleton stuff
	static JASPConfiguration* getInstance(QObject *parent = nullptr);
	JASPConfiguration(JASPConfiguration& other) = delete;
	void operator=(const JASPConfiguration&) = delete;

public slots:
    void remoteChanged();


signals:
	void configurationProcessed(QString result);

private slots:
	void sslErrors(const QList<QSslError> &errors);

private:
	bool processLocal();
	void clear();


	std::shared_ptr<QFile> getLocalConfFile(bool truncate = false);

	QNetworkAccessManager	_networkManager;

	Version _jaspVersion;
	QMap<QString, QMap<QString, QMap<QString, QVariant>>> _definedConstants;
	QMap<QString, QMap<QString, Json::Value>> _analysisOptions;
	QMap<QString, QMap<QString, QMap<QString, bool>>> _analysisOptionsLocked;
	QString _startupCommands;
	QStringList _modulesToLoad;

	const QString configurationFilename = "conf.toml";

	explicit JASPConfiguration(QObject *parent = nullptr);
	static JASPConfiguration* _instance;
};

#endif // JASPCONFIGURATION_H
