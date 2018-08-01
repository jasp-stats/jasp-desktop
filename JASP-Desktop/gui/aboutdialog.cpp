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
#include "ui_aboutdialog.h"

#include "utilities/qutils.h"
#include "appinfo.h"

AboutDialog::AboutDialog(QWidget *parent) :
	QDialog(parent),
	ui(new Ui::AboutDialog)
{
	ui->setupUi(this);

	m_aboutDialogJsInterface = new AboutDialogJsInterface(this);
	
	m_network_manager = new QNetworkAccessManager();
	m_pBuffer = new QByteArray();
	
	setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint | Qt::WindowMaximizeButtonHint | Qt::CustomizeWindowHint);
	
	//About core informataion with current version info
	ui->aboutView->setUrl((QUrl(QString("qrc:///core/about.html"))));
	
	connect(ui->aboutView, SIGNAL(loadFinished(bool)), this, SLOT(aboutPageLoaded(bool)));
 }

AboutDialog::~AboutDialog()
{
	delete ui;
}

void AboutDialog::aboutPageLoaded(bool success)
{

	// Show aboutWebView with about.html and patch information
	if (success)
	{
		QString version = tq(AppInfo::version.asString());
#ifdef JASP_DEBUG
		version+="-Debug";
#endif
		QString builddate = tq(AppInfo::builddate);
		
		m_aboutDialogJsInterface->setAppInfo(version, builddate);
		checkForJaspUpdate();
	}
}

void AboutDialog::checkForJaspUpdate()
{

	QUrl url("https://jasp-stats.org/jasp-version/");
	QNetworkRequest request(url);
	m_network_reply = m_network_manager->get(request);	
	connect(m_network_reply, SIGNAL(finished()), this, SLOT(downloadFinished()));

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
