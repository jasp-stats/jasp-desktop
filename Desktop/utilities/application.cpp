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

#include "resultstesting/compareresults.h"
#include "utilities/settings.h"
#include <QFileOpenEvent>
#include <iostream>
#include <QString>
#include "log.h"

void Application::init(const ParsedArguments& arguments)
{
	std::cout << "Application init entered" << std::endl;

	if(arguments.logToFile)
		Settings::setValue(Settings::LOG_TO_FILE, true);

	Dirs::setReportingDir(fq(arguments.reportingDir.absoluteFilePath()));

	if(arguments.unitTest)
		resultXmlCompare::compareResults::theOne()->enableTestMode(); //So languagemodel can be aware

	_mainWindow = new MainWindow(this);
	PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(arguments.keepMissingColsWhenSyncing);

	//A JASP started to run a data file through a jasp-file is doing that on behalf of someone else, so it should not
	//steal the focus while they keep working. Unless it is meant to stay open at the end, because then they do want it.
	//MainWindow only gets to use this until it loads its QML, which happens after we return here.
	_mainWindow->setStartedForBatch(arguments.dataFiles.size() > 0 && !arguments.keepJASPOpenAfterExporting);

	connect(_mainWindow, &MainWindow::qmlLoadedChanged, _mainWindow, [=,this]() {
		// The QML files are not yet laoded when MainWindow is just created (loadQML is called via a QTmer::singleShot)
		// But to correctly work, the following calls need the QML files to be loaded.
		if (arguments.newData)
			_mainWindow->showNewData();
		else
		{
			if(arguments.unitTest)
				_mainWindow->testLoadedJaspFile(arguments.timeOut, arguments.save);

			ExportType exportType = arguments.save ? ExportType::Jasp : arguments.exportType;

			if(arguments.mainFilePath.exists() || arguments.mainFileIsOnline)
			{
				QFileInfo inputDataFile;
				QString exportFile;

				if (arguments.dataFiles.size() > 0)
				{
					inputDataFile = arguments.dataFiles.front();

					if (!inputDataFile.exists())
					{
						std::cerr << "File " << inputDataFile.absoluteFilePath() << " does not exist!" << std::endl;
						exit(-1);
					}

					if (exportType != ExportType::No)
					{
						QString outputDir = arguments.outputDir.exists() ? arguments.outputDir.absoluteFilePath() : inputDataFile.absoluteDir().absolutePath();
						exportFile = outputDir + "/" + inputDataFile.baseName() + "." + ExportTypeToQString(exportType).toLower();
					}
				}

				QString mainFile = arguments.mainFileIsOnline ? arguments.mainFilePath.filePath() : arguments.mainFilePath.absoluteFilePath();

				_mainWindow->open(mainFile, inputDataFile.absoluteFilePath(), exportFile, arguments.keepJASPOpenAfterExporting, exportType == ExportType::Jasp);
			}

			if(!arguments.dbJson.isNull())
				_mainWindow->open(arguments.dbJson);
		}

	});

	if(arguments.reportingDir.exists())
		_mainWindow->reportHere(arguments.reportingDir.absoluteFilePath());
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
		//Print events for main thread only
		/*if(receiver->thread() == QApplication::thread())
		{
			static int	eventEnumIndex	= QEvent::staticMetaObject.indexOfEnumerator("Type");
			QString		name			= QEvent::staticMetaObject.enumerator(eventEnumIndex).valueToKey(event->type()),
						logThis			= "Application::notify event type: " + (name != "" ? name : QString::number(event->type())) + " for receiver: '" + receiver->objectName() + "'";

			Log::log()  << logThis << std::endl;
		}*/

		return QApplication::notify(receiver, event);
	}
	catch (std::exception &e)
	{
		Log::log() << "Error in object: " << receiver->objectName().toStdString() << ", with event: " << event->type() << ": " << e.what() << std::endl;
		_mainWindow->fatalError();
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
