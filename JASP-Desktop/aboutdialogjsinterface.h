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

#ifndef ABOUTDIALOGJSINTERFACE_H
#define ABOUTDIALOGJSINTERFACE_H

#include <QObject>
#include "aboutdialog.h"

class AboutDialog;

class AboutDialogJsInterface : public QObject
{
	Q_OBJECT
	
public:
	explicit AboutDialogJsInterface(QWidget *parent = 0);
	
	void setAppInfo(const QString& version, const QString& builddate);
	void setNewVersion(const QString& version);
	void showDownloadButton(const QString& str);
	
public slots:
	void closeWindow();
	
private:
	void runJavaScript(const QString &str);
	
	AboutDialog *_aboutDialog;
	QWebEngineView *_aboutView;

};

#endif // ABOUTDIALOGJSINTERFACE_H
