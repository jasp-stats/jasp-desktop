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
#include <QDesktopServices>
#include <QDebug>
#include "appinfo.h"

AboutDialog::AboutDialog(QWidget *parent) :
	QDialog(parent),
	ui(new Ui::AboutDialog)
{
	ui->setupUi(this);

	_aboutWebView = new QWebView(this);
	_aboutWebView->setFixedWidth(900);
	_aboutWebView->setFixedHeight(600);
		
	m_network_manager = new QNetworkAccessManager();
	m_pBuffer = new QByteArray();

	// Disable maximize option dialog
	setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint /* | Qt::WindowMaximizeButtonHint */ | Qt::CustomizeWindowHint);

	//About core informataion with current version info
	_aboutWebView->setUrl((QUrl(QString("qrc:///core/about.html"))));

	connect(_aboutWebView, SIGNAL(loadFinished(bool)), this, SLOT(aboutPageLoaded(bool)));
	connect(_aboutWebView, SIGNAL( linkClicked( QUrl ) ), this, SLOT( linkClickedSlot( QUrl ) ) );
	connect(this, SIGNAL(closeWindow()), this, SLOT(closeWindowHandler()));
		
	_aboutWebView->page()->setLinkDelegationPolicy(QWebPage::DelegateAllLinks);
	_aboutWebView->page()->mainFrame()->addToJavaScriptWindowObject("about", this);
	
#ifdef QT_DEBUG
	_aboutWebView->page()->settings()->setAttribute(QWebSettings::DeveloperExtrasEnabled, true);
#else
	_aboutWebView->setContextMenuPolicy(Qt::NoContextMenu);
#endif
		
 }

AboutDialog::~AboutDialog()
{
	delete ui;
}

void AboutDialog::linkClickedSlot( QUrl url )
{
	QDesktopServices::openUrl ( url );
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
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppVersion('" + version +"')");
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppBuildDate('" + builddate +"')");
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.showDownLoadButton(false,'')");		
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
		
	_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.showDownLoadButton(false,'')");

	if  ((rx.indexIn(result, 0)) != -1)
	{
		QString g = rx.cap(0);
		rx.setPattern("JASPVersion:");
		g.remove(rx);
		rx.setPattern("<\/div>");
		g.remove(rx);
		QString version = g;
	
#ifdef __APPLE__
		downloadfile = "https://static.jasp-stats.org/JASP-" + version + ".dmg";
#endif

#ifdef __WIN32__
		downloadfile = "https://static.jasp-stats.org/JASP-" + version + "-Setup.exe";
#endif

		Version cv = AppInfo::version;
		Version lv(version.toStdString());
		long cur = cv.major*100000 + cv.minor*10000 + cv.revision*1000 + cv.build;
		long latest = lv.major*100000 + lv.minor*10000 + lv.revision*1000 + lv.build;
		if (latest > cur )
		{
			QString display = "true";
			_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setNewVersion('" + version +"')");
#ifndef __linux__
			_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.showDownLoadButton(true,'" + downloadfile + "')");
#endif
		}
	}
	
}

void AboutDialog::closeWindowHandler()
{
	this->close();
}
