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

#ifndef ABOUTDIALOG_H
#define ABOUTDIALOG_H

#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QUrl>
#include <QTimer>

///
/// One off version checker, will delete itself after downloading!
/// Also downloads the known issues json from static.jasp-stats.org for userfeedback on bad bugs
class JASPVersionChecker : public QObject
{
	Q_OBJECT

public:
	explicit JASPVersionChecker(QObject *parent);

signals:
	void showDownloadButton(QString downloadUrl);
	
private slots:
	void downloadVersionFinished();
	void downloadIssuesFinished();
	void downloadKnownIssues();
	void checkForJaspUpdate();

private:
	QNetworkReply			*	_networkReply			= nullptr;
	//Do not do use https here because then, on windows, openSSL dll needs to be loaded and that blocks the whole application
	QString						_urlVersion				= "http://static.jasp-stats.org/JASP-Version.txt",
								_urlKnownIssues			= "http://static.jasp-stats.org/JASP-KnownIssues.json";
	QNetworkAccessManager		_networkManager;
};

#endif // ABOUTDIALOG_H
