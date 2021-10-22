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

#include "log.h"
#include "settings.h"
#include <iostream>

void Application::init(QString filePath, bool unitTest, int timeOut, bool save, bool logToFile)
{	
	std::cout << "Application init entered" << std::endl;
	
	if(logToFile)
		Settings::setValue(Settings::LOG_TO_FILE, true);

	_mainWindow = new MainWindow(this);

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
	catch(...){}
}

bool Application::notify(QObject *receiver, QEvent *event)
{
	try
	{
		//Log::log()  << "Application::notify: " << receiver->objectName() << " with event: " << event  << std::endl;
		return QApplication::notify(receiver, event);
	}
	catch (std::exception &e)
	{
		Log::log() << "Error in object: " << receiver->objectName().toStdString() << ", with event: " << event->type() << ": " << e.what() << std::endl;
		throw e;
	}
	catch (...)
	{
		Log::log() << "Unknown error in object: " << receiver->objectName().toStdString() << ", with event: " << event->type() << std::endl;
		throw std::exception();
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
		try {
			return QApplication::event(event);
		}
		catch (const std::exception & e)
		{
			Log::log() << "Caught exception in Application::event(" << event << "): " << e.what() << std::endl;
			throw e;
		}
	}
}
