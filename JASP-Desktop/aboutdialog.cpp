//
// Copyright (C) 2013-2016 University of Amsterdam
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

	// Disable maximize option dialog
	setWindowFlags(Qt::Tool | Qt::WindowTitleHint | Qt::WindowCloseButtonHint /* | Qt::WindowMaximizeButtonHint */ | Qt::CustomizeWindowHint);

	_aboutWebView->setUrl((QUrl(QString("qrc:///core/about.html"))));
	connect(_aboutWebView, SIGNAL(loadFinished(bool)), this, SLOT(aboutPageLoaded(bool)));

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
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppVersion('" + version + "')");
		_aboutWebView->page()->mainFrame()->evaluateJavaScript("window.setAppBuildDate('" + builddate +"')");
		QString html = _aboutWebView->page()->mainFrame()->toHtml();
		ui->label_2_About->setText(html);
	}
}
