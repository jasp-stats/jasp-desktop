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

	keyValueRE.setPattern(keyValuePattern);

}

bool JASPConfiguration::exists(const QString &constant, const QString &module, const QString &analysis)
{

}

QVariant JASPConfiguration::get(const QString &constant, QVariant defaultValue, const QString &module, const QString &analysis)
{

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

				auto localConfFile = getLocalConfFile();
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

void JASPConfiguration::parse(const QString &conf)
{
	getVersion(conf);
	_patches.clear();
	QStringList lines = conf.split("\n");
	for(auto& line : lines)
	{
		_patches.insert(line);
	}
}

void JASPConfiguration::getVersion(const QString& conf)
{

	auto match =  versionRE.match(conf);
	if(match.hasCaptured("versionNum"))
	{
		try
		{
			jaspVersion = Version(match.captured("versionNum").toStdString());
			return;
		}
		catch (std::runtime_error& e)
		{
			throw std::runtime_error("Could not parse JASP Version number in configuration file: " + std::string(e.what()));
		}
	}
	throw std::runtime_error("No JASP Version number present in configuration file");
}

std::shared_ptr<QFile> JASPConfiguration::getLocalConfFile() {
	QString confPath = QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
	QDir confDir;
	if(!confDir.mkpath(confPath))
		throw std::runtime_error("Could not Access app configuration path");

	QString configurationFilePath = confPath + "/" + configurationFilename;
	std::shared_ptr<QFile> localConfFile = std::make_shared<QFile>(configurationFilePath);
	if (!localConfFile->open(QIODevice::ReadWrite | QIODevice::Text))
		throw std::runtime_error("Could not open local configuration file " + configurationFilePath.toStdString() + ": " + localConfFile->errorString().toStdString());
	return localConfFile;
}

void JASPConfiguration::sslErrors(const QList<QSslError> &errors)
{
	for (const QSslError &error : errors)
		Log::log() << "Error fetching remote settings:" << error.errorString().toStdString() << std::endl;
}



