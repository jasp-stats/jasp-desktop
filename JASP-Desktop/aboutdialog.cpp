//
// Copyright (C) 2013-2017 University of Amsterdam
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

#include "qutils.h"
#include <QWebFrame>
#include <QMessageBox>
#include <QFile>
#include <QTextStream>
#include "appinfo.h"

AboutDialog::AboutDialog(QWidget *parent) :
	QDialog(parent),
	ui(new Ui::AboutDialog)
{
	ui->setupUi(this);

	_aboutWebView = new QWebView(this);
	_aboutWebView->hide();
	m_network_manager = new QNetworkAccessManager();
	m_pBuffer = new QByteArray();

	// Disable maximize option dialog
	setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint /* | Qt::WindowMaximizeButtonHint */ | Qt::CustomizeWindowHint);

	//About core informataion with current version info
	_aboutWebView->setUrl((QUrl(QString("qrc:///core/about.html"))));

	//Check for new update information
	QUrl url("https://jasp-stats.org/download/");
	QNetworkRequest request(url);
	m_network_reply = m_network_manager->get(request);

	connect(_aboutWebView, SIGNAL(loadFinished(bool)), this, SLOT(aboutPageLoaded(bool)));
	connect(m_network_reply, SIGNAL(finished()), this, SLOT(downloadFinished()));

 }

AboutDialog::~AboutDialog()
{
	delete ui;
}

void AboutDialog::on_buttonBox_clicked(QAbstractButton *button)
{
	this->close();
}

void AboutDialog::aboutPageLoaded(bool success)
{

	// Show aboutWebView with about.html and patch information
	if (success)
	{
		QString version = tq(AppInfo::version.asString());
		QString builddate = tq(AppInfo::builddate);
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppYear()");
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppVersion('" + version + "')");
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppBuildDate('" + builddate +"')");
		QString html = _aboutWebView->page()->mainFrame()->toHtml();
		ui->label_2_About->setText(html);
		ui->label_2_About->setTextInteractionFlags(Qt::TextSelectableByMouse | Qt::LinksAccessibleByMouse);
	}
}

void AboutDialog::checkForJaspUpdate()
{

	QUrl url("https://jasp-stats.org/download/");
	QNetworkRequest request(url);
	m_network_reply = m_network_manager->get(request);

}

void AboutDialog::downloadFinished()
{

	*m_pBuffer = m_network_reply->readAll();
	QString result(*m_pBuffer);
	QString downloadfile("");
	QRegExp rx("Download.{1,30}JASP.{1,30}Release.{1,30}2...");
	rx.setCaseSensitivity(Qt::CaseInsensitive);

	if  ((rx.indexIn(result, 0)) != -1)
	{
		QString g = rx.cap(0);
		rx.setPattern("<[^>]*>|\n|Download");
		g.remove(rx);
		int p = g.indexOf("Rel");
		QString version  = g.mid(0,p);
		QString releasedate = g.mid(p,g.length());
		QString numberversion = version;
		rx.setPattern("JASP|[ ]*");
		numberversion.remove(rx);

#ifdef __APPLE__
		downloadfile = "https://static.jasp-stats.org/JASP-" + numberversion + ".dmg";
#endif

#ifdef __WIN32__
		downloadfile = "https://static.jasp-stats.org/JASP-" + numberversion + "-Setup.exe";
#endif

#ifdef __linux__ //No installer download
		QString innerHtml = "<html><head/><body><span style='font-size:16pt; font-weight:600; color:green;'><br/>New Version Available</span><span style='font-size:14pt;'><br/>Version " + version + " " + releasedate + "</body></html>";
#else
		QString innerHtml = "<html><head/><body><span style='font-size:16pt; font-weight:600; color:green;'><br/>New Version Available</span><span style='font-size:14pt;'><br/>Version " + version + " " + releasedate + "<br/>Download new version: </span><a href='" + downloadfile + "'><span style='text-decoration: underline; color:#0000ff;'>here</span></a><span style='font-size:14pt;'><br/></span></body></html>";
#endif
		Version cv = AppInfo::version;
		Version lv(numberversion.toStdString());
		long cur = cv.major*100000 + cv.minor*10000 + cv.revision*1000 + cv.build;
		long latest = lv.major*100000 + lv.minor*10000 + lv.revision*1000 + lv.build;
		if (latest > cur )
			ui->label_3_Update->setText(innerHtml);
	}
}
