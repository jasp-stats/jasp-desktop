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

JASPVersionChecker::JASPVersionChecker(QObject *parent) : QObject(parent)
{
	QTimer::singleShot(500, this, &JASPVersionChecker::checkForJaspUpdate);
}

void JASPVersionChecker::checkForJaspUpdate()
{
	QNetworkRequest request(_url);
	_networkReply = _networkManager.get(request);
	connect(_networkReply, &QNetworkReply::finished, this, &JASPVersionChecker::downloadFinished);
}

void JASPVersionChecker::downloadFinished()
{
	QString result(_networkReply->readAll());
	QRegExp rx("JASPVersion:.+</div>", Qt::CaseInsensitive);

	if  ((rx.indexIn(result, 0)) != -1)
	{
		QString g = rx.cap(0);
		rx.setPattern("JASPVersion:");
		g.remove(rx);
		rx.setPattern("</div>");
		g.remove(rx);
		g = g.trimmed(); 

		QString version			= g,
				downloadfile	= "https://jasp-stats.org/download/";

		Version cv		= AppInfo::version,
				lv		= version.toStdString();
		long	cur		= cv.major*1000000 + cv.minor*100000 + cv.revision*1000 + cv.build,
				latest	= lv.major*1000000 + lv.minor*100000 + lv.revision*1000 + lv.build;

		if (latest > cur)
			emit showDownloadButton(downloadfile);
	}

	delete this; //Remove yourself!
}
