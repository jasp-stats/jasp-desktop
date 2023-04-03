#include "remotesettings.h"
#include "utilities/settings.h"
#include "log.h"
#include <QNetworkReply>
#include <QStandardPaths>
#include <QFile>
#include <QTextStream>
#include <QRegularExpression>
#include <QString>


RemoteSettings::RemoteSettings(QObject *parent)
	: QObject{parent}
{
	//connect(&_networkManager, &QNetworkAccessManager::finished, this, &RemoteSettings::downloadFinished);
	versionRE.setPattern("Version:\\s*(?<versionNum>\\d+)");
}

void RemoteSettings::processRemoteSettings()
{
	//read & parse local
	QString confPath = QStandardPaths::writableLocation(QStandardPaths::AppConfigLocation);
	QFile local(confPath + configurationFilename);
	if (!local.open(QIODevice::ReadWrite | QIODevice::Text))
	{
		Log::log() << "Could not open local configuration file!";
		return;
	}
	QTextStream in(&local);
	parse(in.readAll());


	//read & parse remote
}

void RemoteSettings::parse(const QString &settings)
{
	_version = getVersion(settings);

	_patches.clear();
	QStringList lines = settings.split("\n");
	for(auto& line : lines)
	{
		auto match =  versionRE.match(line);
		if(!match.hasMatch())
			_patches.append(line);
	}
}

int32_t RemoteSettings::getVersion(const QString& settings)
{
	auto match =  versionRE.match(settings);
	if(match.hasCaptured("versionNum"))
	{
		bool ok;
		int32_t version = match.captured("versionNum").toULong(&ok);
		return ok ? version : -1;
	}
	return -1;
}
