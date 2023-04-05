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

void JASPConfiguration::processSettings()
{
	//read & parse local
	QString confPath = QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
	QDir confDir;
	confDir.mkpath(confPath);

	QString settingsPath = confPath + "/" + configurationFilename;
	QFile localSettingsFile(settingsPath);
	if (!localSettingsFile.open(QIODevice::ReadWrite | QIODevice::Text))
	{
		Log::log() << "!!!Could not open local configuration file " << settingsPath.toStdString() << ": " << localSettingsFile.errorString().toStdString() << std::endl;
		return;
	}
	QTextStream in(&localSettingsFile);
	parse(in.readAll());
	localSettingsFile.close();


    //read, parse & save remote settings
    if(Settings::value(Settings::REMOTE_SETTINGS_URL).toString() != "")
    {
        auto conn = std::make_shared<QMetaObject::Connection>();
        *conn = connect(&_networkManager, &QNetworkAccessManager::finished, this, [&, this, settingsPath, conn](QNetworkReply* reply) {
            QObject::disconnect(*conn);
            reply->deleteLater();

            if(reply->error())
            {
                Log::log() << "!!!Error fetching remote settings file " << reply->request().url().toString().toStdString() << " : " << reply->errorString().toStdString() << std::endl;
                emit this->settingsProcessed("FAIL");
                return;
            }
            QString payload = reply->readAll();
            parse(payload);

            QFile localSettingsFile(settingsPath);
            if (!localSettingsFile.open(QIODevice::ReadWrite | QIODevice::Text))
            {
                Log::log() << "!!!Could not open local configuration file " << settingsPath.toStdString() << ": " << localSettingsFile.errorString().toStdString() << std::endl;
                emit this->settingsProcessed("FAIL");
                return;
            }
            localSettingsFile.write(payload.toUtf8());
            localSettingsFile.close();

            Log::log() << "!!!Updated local copy of remote settings" << std::endl;
            emit this->settingsProcessed("OK");
        });

        //make the request
        QNetworkRequest request(Settings::value(Settings::REMOTE_SETTINGS_URL).toString());
        QNetworkReply* reply = _networkManager.get(request);
        connect(reply, &QNetworkReply::sslErrors, this, &JASPConfiguration::sslErrors);
    }
    else
        emit settingsProcessed("OK");
}

void JASPConfiguration::remoteChanged(QString remoteURL)
{
	processSettings();
}

void JASPConfiguration::parse(const QString &settings)
{
	_patches.clear();
	QStringList lines = settings.split("\n");
	for(auto& line : lines)
	{
		_patches.insert(line);
	}
}

void JASPConfiguration::sslErrors(const QList<QSslError> &errors)
{
	for (const QSslError &error : errors)
		Log::log() << "!!!Error fetching remote settings:" << error.errorString().toStdString() << std::endl;
}

//void RemoteSettings::parse(const QString &settings)
//{
//	_version = getVersion(settings);

//	_patches.clear();
//	QStringList lines = settings.split("\n");
//	for(auto& line : lines)
//	{
//		auto match =  versionRE.match(line);
//		if(!match.hasMatch())
//			_patches.append(line);
//	}
//}

bool JASPConfiguration::getVersion(const QString& settings)
{
    static QRegularExpression versionRE("JASP_Version:\\s*(?<versionNum>\\w+)\\s*$", QRegularExpression::MultilineOption);
    auto match =  versionRE.match(settings);
    if(match.hasCaptured("versionNum"))
    {
        try
        {
            jaspVersion = Version(match.captured("versionNum").toStdString());
            return true;
        }
        catch (std::runtime_error& e)
        {
            Log::log() << "Could not parse JASP Version number in configuration file: " << e.what() << std::endl;
            return false;
        }
    }
    Log::log() << "No JASP Version number present in configuration file" << std::endl;
    return false;
}
