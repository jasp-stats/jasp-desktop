#include "jaspconfiguration.h"
#include "jaspConfigurationParser.h"
#include "utilities/settings.h"
#include "log.h"
#include <QNetworkReply>
#include <QStandardPaths>
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>
#include <QString>
#include <QDir>
#include <QMetaType>

JASPConfiguration* JASPConfiguration::_instance = nullptr;

JASPConfiguration* JASPConfiguration::getInstance(QObject *parent)
{
	if(!_instance)
		_instance = new JASPConfiguration(parent);
	return _instance;
}


JASPConfiguration::JASPConfiguration(QObject *parent)
	: QObject{parent}
{

}

bool JASPConfiguration::constantExists(const QString &constant, const QString &module, const QString &analysis)
{
	return _definedConstants.contains(module) && _definedConstants[module].contains(analysis) && _definedConstants[module][analysis].contains(constant);
}

QVariant JASPConfiguration::getConstant(const QString &constant, const QVariant& defaultValue, const QString &module, const QString &analysis)
{
    if(constantExists(constant, module, analysis))
		return _definedConstants[module][analysis][constant];
    else if(constantExists(constant, module))
		return _definedConstants[module][""][constant];
    else if(constantExists(constant))
		return _definedConstants[""][""][constant];
    return defaultValue;
}

bool JASPConfiguration::optionSet(const QString &module, const QString &analysis, const QString &optionName)
{
	return _analysisOptions.contains(module) && _analysisOptions[module].contains(analysis) && _analysisOptions[module][analysis].isMember(optionName.toStdString());
}

bool JASPConfiguration::optionsSet(const QString &module, const QString &analysis)
{
	return _analysisOptions.contains(module) && _analysisOptions[module].contains(analysis);
}

bool JASPConfiguration::optionLocked(const QString &module, const QString &analysis, const QString &optionName)
{
	return _analysisOptionsLocked[module][analysis][optionName];
}

Json::Value JASPConfiguration::getAnalysisOptionValues(const QString &module, const QString &analysis)
{
	if(optionsSet(module, analysis))
		return _analysisOptions[module][analysis];
	return Json::nullValue;
}

void JASPConfiguration::processConfiguration()
{
	clear();
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
				clear();
				if(!JASPConfigurationParser::getInstance()->parse(this, payload))
					throw std::runtime_error("Parsing failed");

				auto localConfFile = getLocalConfFile(true);
				localConfFile->write(payload);
				localConfFile->close();
				Log::log() << "Updated local copy of remote configuration" << std::endl;
				emit this->configurationProcessed("REMOTE");
			}
			catch (std::runtime_error& e)
			{
				Log::log() << "Failed to process remote configuration: " << e.what() << std::endl;
				if(!localOK)
					emit this->configurationProcessed("FAIL");
				else
					emit this->configurationProcessed("LOCAL");
				return;
			}
        });

        //make the request
		QNetworkRequest request(Settings::value(Settings::REMOTE_CONFIGURATION_URL).toString());
        QNetworkReply* reply = _networkManager.get(request);
        connect(reply, &QNetworkReply::sslErrors, this, &JASPConfiguration::sslErrors);
    }
    else
		emit configurationProcessed("LOCAL");
}

bool JASPConfiguration::processLocal()
{
	try
	{
		auto localConfFile = getLocalConfFile();
		QTextStream in(localConfFile.get());
		if(!JASPConfigurationParser::getInstance()->parse(this, in.readAll()))
			throw std::runtime_error("Parsing failed");
	}
	catch(std::runtime_error& e)
	{
		Log::log() <<  "Could not parse local configuration: " << e.what() << std::endl;
		return false;
	}
	return true;
}

void JASPConfiguration::clear()
{
	_definedConstants.clear();
	_modulesToLoad.clear();
}

void JASPConfiguration::remoteChanged(QString remoteURL)
{
	processConfiguration();
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

bool JASPConfiguration::addConstant(QString key, QVariant value, QString moduleName, QString analysisName)
{
	_definedConstants[moduleName][analysisName][key] = value;
	return true;
}

bool JASPConfiguration::addOption(QString key, QVariant value, bool locked, QString moduleName, QString analysisName)
{
	_analysisOptionsLocked[moduleName][analysisName][key] = locked;
	if(!(_analysisOptions.contains(moduleName) && _analysisOptions[moduleName].contains(analysisName)))
		_analysisOptions[moduleName][analysisName] = Json::Value(Json::objectValue);

	//TODO fix very suboptimal way of doing things
	if(value.userType() == QMetaType::Double)
		_analysisOptions[moduleName][analysisName][key.toStdString()] = value.value<double>();
	else if(value.userType() == QMetaType::LongLong)
		_analysisOptions[moduleName][analysisName][key.toStdString()] = static_cast<int64_t>(value.value<long long>());
	else if(value.userType() == QMetaType::Bool)
		_analysisOptions[moduleName][analysisName][key.toStdString()] = value.value<bool>();
	else if(value.canConvert<QString>())
		_analysisOptions[moduleName][analysisName][key.toStdString()] = value.toString().toStdString();

	return true;
}

void JASPConfiguration::sslErrors(const QList<QSslError> &errors)
{
	for (const QSslError &error : errors)
		Log::log() << "Error fetching remote settings:" << error.errorString().toStdString() << std::endl;
}



