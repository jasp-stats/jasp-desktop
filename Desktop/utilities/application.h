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

#ifndef APPLICATION_H
#define APPLICATION_H

#include <QApplication>
#include "mainwindow.h"
#include "common.h"

///
/// Our override of QApplication, basically instantiates MainWindow and cleans it up at the end
class Application : public QApplication
{
	Q_OBJECT
public:
	explicit Application(int &argc, char **argv) : QApplication(argc, argv) {}
	~Application() override;

	virtual bool notify(QObject *receiver, QEvent *event) OVERRIDE;
	virtual bool event(QEvent *event) OVERRIDE;
	
	void init(QString filePath, bool unitTest, int timeOut, bool save, bool logToFile, const Json::Value & dbJson);

signals:

public slots:

private:
	MainWindow *_mainWindow;

};

#endif // APPLICATION_H
