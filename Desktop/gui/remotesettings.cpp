#include "remotesettings.h"
#include "utilities/settings.h"
#include "log.h"
#include <QNetworkReply>
#include <QStandardPaths>
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>
#include <QString>
#include <QDir>


RemoteSettings::RemoteSettings(QObject *parent)
	: QObject{parent}
{
	//connect(&_networkManager, &QNetworkAccessManager::finished, this, &RemoteSettings::downloadFinished);
//	versionRE.setPattern("Version:\\s*(?<versionNum>\\d+)");
}

void RemoteSettings::processRemoteSettings()
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


	//read,parse & save remote settings
	auto conn = std::make_shared<QMetaObject::Connection>();
	*conn = connect(&_networkManager, &QNetworkAccessManager::finished, this, [&, this, settingsPath, conn](QNetworkReply* reply) {
		QObject::disconnect(*conn);
		reply->deleteLater();

		if(reply->error())
		{
			Log::log() << "!!!Error fetching remote settings file " << reply->request().url().toString().toStdString() << " : " << reply->errorString().toStdString() << std::endl;
			emit this->remoteSettingsProcessed("FAIL");
			return;
		}
		QString payload = reply->readAll();
		parse(payload);

		QFile localSettingsFile(settingsPath);
		if (!localSettingsFile.open(QIODevice::ReadWrite | QIODevice::Text))
		{
			Log::log() << "!!!Could not open local configuration file " << settingsPath.toStdString() << ": " << localSettingsFile.errorString().toStdString() << std::endl;
			emit this->remoteSettingsProcessed("FAIL");
			return;
		}
		localSettingsFile.write(payload.toUtf8());
		localSettingsFile.close();

		Log::log() << "!!!Updated local copy of remote settings" << std::endl;
		emit this->remoteSettingsProcessed("OK");
	});

	//make the request
	QNetworkRequest request(Settings::value(Settings::REMOTE_SETTINGS_URL).toString());
	QNetworkReply* reply = _networkManager.get(request);
	connect(reply, &QNetworkReply::sslErrors, this, &RemoteSettings::sslErrors);
}

void RemoteSettings::remoteChanged(QString remoteURL)
{
	processRemoteSettings();
}

void RemoteSettings::parse(const QString &settings)
{
	_patches.clear();
	QStringList lines = settings.split("\n");
	for(auto& line : lines)
	{
		_patches.insert(line);
	}
}

void RemoteSettings::sslErrors(const QList<QSslError> &errors)
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

//int32_t RemoteSettings::getVersion(const QString& settings)
//{
//	auto match =  versionRE.match(settings);
//	if(match.hasCaptured("versionNum"))
//	{
//		bool ok;
//		int32_t version = match.captured("versionNum").toULong(&ok);
//		return ok ? version : -1;
//	}
//	return -1;
//}
