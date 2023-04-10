#include "jaspconfiguration.h"
#include "utilities/settings.h"
#include "log.h"
#include <QNetworkReply>
#include <QStandardPaths>
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>
#include <QString>
#include <QDir>


JASPConfiguration::JASPConfiguration(QObject *parent)
	: QObject{parent}
{
	versionRE.setPattern(versionPattern);
	versionRE.setPatternOptions(QRegularExpression::MultilineOption);
	loadModulesRE.setPattern(loadModulesPattern);
	loadModulesRE.setPatternOptions(QRegularExpression::MultilineOption);
	moduleStatementRE.setPattern(moduleSectionPattern);
	moduleStatementRE.setPatternOptions(QRegularExpression::MultilineOption | QRegularExpression::DotMatchesEverythingOption);
	analysisStatementRE.setPattern(analysisSectionPattern);
	analysisStatementRE.setPatternOptions(QRegularExpression::MultilineOption | QRegularExpression::DotMatchesEverythingOption);
	keyValueRE.setPattern(keyValuePattern);
}

bool JASPConfiguration::exists(const QString &constant, const QString &module, const QString &analysis)
{
	return _definedConstants.contains(module) && _definedConstants[module].contains(analysis) && _definedConstants[module][analysis].contains(constant);
}

QVariant JASPConfiguration::get(const QString &constant, QVariant defaultValue, const QString &module, const QString &analysis)
{
    if(exists(constant, module, analysis))
		return _definedConstants[module][analysis][constant];
    return defaultValue;
}

bool JASPConfiguration::isSet(const QString &module, const QString &analysis, const QString &optionName)
{

}

QString &JASPConfiguration::getAnalysisOptionValue(const QString &module, const QString &analysis, const QString &optionName, const QString &defaultValue)
{

}

void JASPConfiguration::processConfiguration()
{
	bool localOK = processLocal();

	//read, parse & save remote settings
	if(Settings::value(Settings::REMOTE_CONFIGURATION_URL).toString() != "")
    {
        auto conn = std::make_shared<QMetaObject::Connection>();
		*conn = connect(&_networkManager, &QNetworkAccessManager::finished, this, [=, this](QNetworkReply* reply) {
			QObject::disconnect(*conn);
			reply->deleteLater();

			try
			{
				if(reply->error())
					throw std::runtime_error("Error fetching remote configuration file " + reply->request().url().toString().toStdString() + " : " + reply->errorString().toStdString());
				QByteArray payload = reply->readAll();
				parse(payload);

				auto localConfFile = getLocalConfFile(true);
				localConfFile->write(payload);
				localConfFile->close();
				Log::log() << "!!!Updated local copy of remote configuration" << std::endl;
				emit this->configurationProcessed("OK");
			}
			catch (std::runtime_error& e)
			{
				Log::log() << "!!!Failed to process remote configuration: " << e.what() << std::endl;
				if(!localOK)
					emit this->configurationProcessed("FAIL");
				else
					emit this->configurationProcessed("OK");
				return;
			}
        });

        //make the request
		QNetworkRequest request(Settings::value(Settings::REMOTE_CONFIGURATION_URL).toString());
        QNetworkReply* reply = _networkManager.get(request);
        connect(reply, &QNetworkReply::sslErrors, this, &JASPConfiguration::sslErrors);
    }
    else
		emit configurationProcessed("OK");
}

bool JASPConfiguration::processLocal()
{
	try
	{
		auto localConfFile = getLocalConfFile();
		QTextStream in(localConfFile.get());
		parse(in.readAll());
	}
	catch(std::runtime_error& e)
	{
		Log::log() <<  "!!!Could not parse local configuration: " << e.what() << std::endl;
		return false;
	}
	return true;
}

void JASPConfiguration::remoteChanged(QString remoteURL)
{
	processConfiguration();
}

void JASPConfiguration::parse(QString conf)
{
	parseVersion(conf);
	parseModulesToLoad(conf);
	parseModuleStatements(conf);
	parseKeyValuePairs(conf);
}

void JASPConfiguration::parseVersion(QString& conf)
{
    auto match = versionRE.match(conf);
	if(match.hasCaptured("versionNum"))
	{
		try
		{
			_jaspVersion = Version(match.captured("versionNum").toStdString());
			conf.remove(match.capturedStart(), match.capturedLength());
			return;
		}
		catch (std::runtime_error& e)
		{
			throw std::runtime_error("Could not parse JASP Version number in configuration file: " + std::string(e.what()));
		}
	}
	throw std::runtime_error("No JASP Version number present in configuration file");
}

void JASPConfiguration::parseModulesToLoad(QString &conf)
{
	auto match = loadModulesRE.match(conf);
	if(match.hasCaptured("list"))
	{
		QStringList list = match.captured("list").split(",", Qt::SkipEmptyParts);
		for(const QString& item : list)
			_modulesToLoad.push_back(item.trimmed());
		conf.remove(match.capturedStart(), match.capturedLength());
	}
}

void JASPConfiguration::parseModuleStatements(QString &conf)
{
	auto match = moduleStatementRE.match(conf);
	while (match.hasMatch()) {
		QString section = match.captured("section");
		QString moduleName = match.captured("name");
		parseAnalysisStatements(section, moduleName);
		parseKeyValuePairs(section, moduleName);

		conf.remove(match.capturedStart(), match.capturedLength());
		match = moduleStatementRE.match(conf);
	}
}

void JASPConfiguration::parseAnalysisStatements(QString &conf, const QString& moduleName)
{
	auto match = analysisStatementRE.match(conf);
	while (match.hasMatch()) {
		QString section = match.captured("section");
		QString analysisName = match.captured("name");
		//TODO: parse analysis options
		parseKeyValuePairs(section, moduleName, analysisName);

		conf.remove(match.capturedStart(), match.capturedLength());
		match = analysisStatementRE.match(conf);
	}
}

void JASPConfiguration::parseKeyValuePairs(const QString &conf, const QString moduleName, const QString analysisName)
{

	QStringList lines = conf.split("\n");
	for(auto& line : lines)
	{
		auto match = keyValueRE.match(line);
		if(match.hasMatch())
		{
			QString key = match.captured("key");
			QVariant value = QVariant(match.captured("value"));
			_definedConstants[moduleName][analysisName][key] = value;

			Log::log() << "!!!: " << moduleName.toStdString() << " " << analysisName.toStdString() << " " << key.toStdString() << ": " << match.captured("value").toStdString() << std::endl;
		}
		else
			Log::log() << "!!!invalid line in configuration: " + line.toStdString() << std::endl;
	}
}

std::shared_ptr<QFile> JASPConfiguration::getLocalConfFile(bool truncate) {
	QString confPath = QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
	QDir confDir;
	if(!confDir.mkpath(confPath))
		throw std::runtime_error("Could not Access app configuration path");

	QString configurationFilePath = confPath + "/" + configurationFilename;
	std::shared_ptr<QFile> localConfFile = std::make_shared<QFile>(configurationFilePath);
	QIODeviceBase::OpenMode flags = QIODeviceBase::ReadWrite | QIODeviceBase::Text | (truncate ? QIODeviceBase::Truncate : QIODeviceBase::NotOpen);
	if (!localConfFile->open(flags))
		throw std::runtime_error("Could not open local configuration file " + configurationFilePath.toStdString() + ": " + localConfFile->errorString().toStdString());
	return localConfFile;
}

void JASPConfiguration::sslErrors(const QList<QSslError> &errors)
{
	for (const QSslError &error : errors)
		Log::log() << "Error fetching remote settings:" << error.errorString().toStdString() << std::endl;
}



