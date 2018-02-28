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

#include <QDialog>
#include <QAbstractButton>
#include <QWebEngineView>
#include <QNetworkAccessManager>
#include <QNetworkRequest>
#include <QNetworkReply>
#include <QUrl>

#include "aboutdialogjsinterface.h"

class AboutDialogJsInterface;

namespace Ui {
class AboutDialog;
}

class AboutDialog : public QDialog
{
	Q_OBJECT
	friend class AboutDialogJsInterface;
public:
	explicit AboutDialog(QWidget *parent = 0);
	~AboutDialog();	
	
private slots:
	void aboutPageLoaded(bool success);
	void downloadFinished();

private:
	void checkForJaspUpdate();
	Ui::AboutDialog *ui;
	QNetworkAccessManager *m_network_manager;	// make the HTTP GET request
	QNetworkReply *m_network_reply;
	QByteArray *m_pBuffer;
	
	AboutDialogJsInterface *m_aboutDialogJsInterface;
	
};

#endif // ABOUTDIALOG_H
