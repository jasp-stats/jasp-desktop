//
// Copyright (C) 2017 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "aboutdialogjsinterface.h"
#include "ui_aboutdialog.h"
#include <QWebChannel>

AboutDialogJsInterface::AboutDialogJsInterface(QWidget *parent) : QObject(parent)
{
	_aboutDialog = dynamic_cast<AboutDialog *>(parent);
	_aboutView = _aboutDialog->ui->aboutView;
	_aboutView->setContextMenuPolicy(Qt::NoContextMenu);

	QWebChannel *channel = new QWebChannel(this);
	channel->registerObject(QStringLiteral("about"), this);
	_aboutView->page()->setWebChannel(channel);
}

void AboutDialogJsInterface::runJavaScript(const QString &str)
{
	_aboutView->page()->runJavaScript(str);
}


void AboutDialogJsInterface::setAppInfo(const QString& version, const QString& builddate)
{
	runJavaScript("window.setAppYear()");
	runJavaScript("window.setAppVersion('" + version + "')");
	runJavaScript("window.setAppBuildDate('" + builddate +"')");
	runJavaScript("window.showDownLoadButton(false,'')");
}

void AboutDialogJsInterface::showDownloadButton(const QString& str)
{
	runJavaScript("window.showDownLoadButton(true,'" + str + "')");
}

void AboutDialogJsInterface::setNewVersion(const QString& version)
{
	runJavaScript("window.setNewVersion('" + version +"')");
}

void AboutDialogJsInterface::closeWindow()
{
	_aboutDialog->close();
}

