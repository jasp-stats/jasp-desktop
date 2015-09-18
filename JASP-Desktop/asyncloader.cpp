#include "asyncloader.h"

#include "exporters/jaspexporter.h"
#include "exporters/csvexporter.h"

#include <iostream>
#include <fstream>
#include <QTimer>
#include <QFileInfo>

#include <boost/bind.hpp>

#include "qutils.h"
#include "utils.h"

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{
	this->moveToThread(&_thread);

	connect(this, SIGNAL(beginLoad(FileEvent*, DataSetPackage*)), this, SLOT(loadTask(FileEvent*, DataSetPackage*)));
	connect(this, SIGNAL(beginSave(FileEvent*, DataSetPackage*)), this, SLOT(saveTask(FileEvent*, DataSetPackage*)));

	_thread.start();
}

void AsyncLoader::io(FileEvent *event, DataSetPackage *package)
{
	if (event->operation() == FileEvent::FileOpen)
	{
		emit progress("Loading Data Set", 0);
		emit beginLoad(event, package);
	}
	else if (event->operation() == FileEvent::FileSave)
	{
		emit progress("Saving Data Set", 0);
		emit beginSave(event, package);
	}
	else if (event->operation() == FileEvent::FileClose)
	{
		event->setComplete();
	}
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(FileEvent *event, DataSetPackage *package)
{
	try
	{		
		_loader.loadPackage(package, fq(event->path()), boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		event->setComplete();
	}
	catch (runtime_error e)
	{
		event->setComplete(false, e.what());
	}
	catch (exception e)
	{
		event->setComplete(false, e.what());
	}
}

void AsyncLoader::saveTask(FileEvent *event, DataSetPackage *package)
{
	QString path = event->path();
	QString tempPath = path + QString(".tmp");

	try
	{
		int maxSleepTime = 2000;
		int sleepTime = 100;
		int delay = 0;
		while (package->isReady() == false)
		{
			if (delay > maxSleepTime)
				break;

			sleep(sleepTime);
			delay += sleepTime;
		}

		JASPExporter::saveDataSet(fq(tempPath), package, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		if ( ! Utils::renameOverwrite(fq(tempPath), fq(path)))
			throw runtime_error("File '" + fq(path) + "' is being used by another application.");

		event->setComplete();
	}
	catch (runtime_error e)
	{
		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
	catch (exception e)
	{
		Utils::removeFile(fq(tempPath));
		event->setComplete(false, e.what());
	}
}

void AsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}



void AsyncLoader::sleep(int ms)
{

#ifdef __WIN32__
	Sleep(uint(ms));
#else
	struct timespec ts = { ms / 1000, (ms % 1000) * 1000 * 1000 };
	nanosleep(&ts, NULL);
#endif
}
