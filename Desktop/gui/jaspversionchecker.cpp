//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "jaspversionchecker.h"
#include "knownissues.h"
#include "appinfo.h"
#include "log.h"
#include "utilities/settings.h"

JASPVersionChecker::JASPVersionChecker(QObject *parent) : QObject(parent)
{
	Log::log() << "JASP check for updates started." << std::endl;
	
	QTimer::singleShot(500, this, &JASPVersionChecker::checkForJaspUpdate);
}

//Check every day?
#define EXPIRATION_TIME_SEC 60 * 60 * 24

bool JASPVersionChecker::timeForDailyCheck()
{
	long	lastTime	= Settings::value(Settings::LAST_CHECK).toInt(),
			curTime		= Utils::currentSeconds();
	
	return lastTime == -1 || curTime - lastTime > EXPIRATION_TIME_SEC;
}

void JASPVersionChecker::checkForJaspUpdate()
{
	QNetworkRequest request(_urlVersion);
	_networkReply = _networkManager.get(request);
	connect(_networkReply, &QNetworkReply::finished, this, &JASPVersionChecker::downloadVersionFinished);
}

void JASPVersionChecker::downloadKnownIssues()
{
	QNetworkRequest request(_urlKnownIssues);
	_networkReply = _networkManager.get(request);
	connect(_networkReply, &QNetworkReply::finished, this, &JASPVersionChecker::downloadIssuesFinished);
}

void JASPVersionChecker::downloadVersionFinished()
{
	Settings::setValue(Settings::LAST_CHECK, int(Utils::currentSeconds()));
	
	QString version			= _networkReply->readAll().trimmed(),
			downloadfile	= "https://jasp-stats.org/download/";

	if(version != "")
	{
		try
		{
			Version cv		= AppInfo::version,
					lv		= version.toStdString();
			long	cur		= cv.major()*1000000 + cv.minor()*100000 + cv.release()*1000 + cv.fourth(),
					latest	= lv.major()*1000000 + lv.minor()*100000 + lv.release()*1000 + lv.fourth();

			if (latest > cur)
				emit showDownloadButton(downloadfile);

			if(KnownIssues::issues()->downloadNeededOrLoad())	downloadKnownIssues();
			else deleteLater(); //Remove yourself!
		}
		catch(std::runtime_error& e)
		{
			Log::log() << "Unable to parse version number:\n " << e.what() << std::endl;
		}
	}

}

void JASPVersionChecker::downloadIssuesFinished()
{
	Log::log() << "New version of knownIssues downloaded!" << std::endl;
	try
	{
		KnownIssues::issues()->loadJson(_networkReply->readAll().trimmed(), true);
	}
	catch (...)
	{
		Log::log() << "Something went wrong parsing the known issues" << std::endl;
	}

	deleteLater(); //Remove yourself!
}
