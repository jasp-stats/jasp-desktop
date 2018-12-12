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

#include "application.h"

#include <QFileOpenEvent>
#include <QString>

#include <iostream>

Application::Application(int &argc, char **argv, QString filePath, bool unitTest, int timeOut, bool save) :
	QGuiApplication(argc, argv)
{
	_mainWindow = new MainWindow(this);

	QStringList args = QGuiApplication::arguments();

	if(unitTest)
		_mainWindow->testLoadedJaspFile(timeOut, save);

	if(filePath.size() > 0)
		_mainWindow->open(filePath);
}

Application::~Application()
{
	try
	{
		delete _mainWindow;
	}
	catch(...)
	{
	}
}

bool Application::notify(QObject *receiver, QEvent *event)
{
	try
	{
		return QGuiApplication::notify(receiver, event);
	}
	catch (std::exception &e)
	{
		std::cout << "Error in object: " << receiver->objectName().toStdString() << ", with event: " << event->type() << ": " << e.what() << "\n";
		std::cout.flush();

		throw e;
	}
}

bool Application::event(QEvent *event)
{
	if (event->type() == QEvent::FileOpen)
	{
		QFileOpenEvent *openEvent = static_cast<QFileOpenEvent*>(event);
		QString file = openEvent->file();
		_mainWindow->open(file);

		return true;
	}
	else
	{
		return QGuiApplication::event(event);
	}
}
