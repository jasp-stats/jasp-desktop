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

#include "aboutdialog.h"

#include "utilities/qutils.h"
#include "appinfo.h"

AboutDialog::AboutDialog(QObject *parent) :
	QObject(parent)
{
	m_aboutDialogJsInterface = new AboutDialogJsInterface(this);
	
	m_network_manager = new QNetworkAccessManager();
	m_pBuffer = new QByteArray();
	
	//setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::WindowMaximizeButtonHint | Qt::CustomizeWindowHint);

	//loading the about-page at construction time makes JASP start slow! Its better if we load it later (in showEvent)
}

AboutDialog::~AboutDialog()
{

}

void AboutDialog::aboutPageLoaded(bool success)
{

	// Show aboutWebView with about.html and patch information
	if (success)
	{
		QString version = tq(AppInfo::version.asString());

		version+="-Beta";

#ifdef JASP_DEBUG
		version+="-Debug";
#endif

		QString builddate = tq(AppInfo::builddate);
		
		m_aboutDialogJsInterface->setAppInfo(version, builddate);

		delayedVersionCheck = new QTimer();
		delayedVersionCheck->setInterval(200);
		connect(delayedVersionCheck, &QTimer::timeout, this, &AboutDialog::checkForJaspUpdate);
	}
}

void AboutDialog::checkForJaspUpdate()
{

	QUrl url("https://jasp-stats.org/jasp-version/");
	QNetworkRequest request(url);
	m_network_reply = m_network_manager->get(request);	
	connect(m_network_reply, &QNetworkReply::finished, this, &AboutDialog::downloadFinished);

}

void AboutDialog::downloadFinished()
{

	*m_pBuffer = m_network_reply->readAll();
	QString result(*m_pBuffer);
	QString downloadfile("");
	QRegExp rx("JASPVersion:.+<\/div>");
	rx.setCaseSensitivity(Qt::CaseInsensitive);
		
	//ui->aboutView->page()->runJavaScript("window.showDownLoadButton(false,'')");

	if  ((rx.indexIn(result, 0)) != -1)
	{
		QString g = rx.cap(0);
		rx.setPattern("JASPVersion:");
		g.remove(rx);
		rx.setPattern("<\/div>");
		g.remove(rx);
		g = g.trimmed(); 
		QString version = g;
	
#ifdef __APPLE__
		downloadfile = "https://static.jasp-stats.org/JASP-" + version + ".dmg";
#elif __WIN32__
		downloadfile = "https://static.jasp-stats.org/JASP-" + version + "-Setup.exe";
#endif

		Version cv = AppInfo::version;
		Version lv(version.toStdString());
		long cur = cv.major*100000 + cv.minor*10000 + cv.revision*1000 + cv.build;
		long latest = lv.major*100000 + lv.minor*10000 + lv.revision*1000 + lv.build;
		if (latest > cur)
		{
			m_aboutDialogJsInterface->setNewVersion(version);
#ifndef __linux__
			m_aboutDialogJsInterface->showDownloadButton(downloadfile);
#endif
		}
	}
}

/*
void AboutDialog::showEvent(QShowEvent * e)
{
	static QUrl aboutUrl = QUrl(QString("qrc:///core/about.html"));

	if(ui->aboutView->url() != aboutUrl)
	{
		ui->aboutView->load(aboutUrl);
		connect(ui->aboutView, &CustomWebEngineView::loadFinished, this, &AboutDialog::aboutPageLoaded);
	}

	QDialog::showEvent(e);
}
*/
