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

#include "utilities/qutils.h"
#include "gui/aboutmodel.h"
#include "appinfo.h"
#include "knownissues.h"
#include "log.h"

JASPVersionChecker::JASPVersionChecker(QObject *parent) : QObject(parent)
{
	QTimer::singleShot(500, this, &JASPVersionChecker::checkForJaspUpdate);
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
	QString version			= _networkReply->readAll().trimmed(),
			downloadfile	= "https://jasp-stats.org/download/";

	if(version != "")
	{

		Version cv		= AppInfo::version,
				lv		= version.toStdString();
		long	cur		= cv.major()*1000000 + cv.minor()*100000 + cv.release()*1000 + cv.fourth(),
				latest	= lv.major()*1000000 + lv.minor()*100000 + lv.release()*1000 + lv.fourth();

		if (latest > cur)
			emit showDownloadButton(downloadfile);
	}

	if(KnownIssues::issues()->downloadNeededOrLoad())	downloadKnownIssues();
	else												deleteLater(); //Remove yourself!
}

void JASPVersionChecker::downloadIssuesFinished()
{
	Log::log() << "New version of knownIssues downloaded!" << std::endl;
	KnownIssues::issues()->loadJson(_networkReply->readAll().trimmed(), true);

	deleteLater(); //Remove yourself!
}
