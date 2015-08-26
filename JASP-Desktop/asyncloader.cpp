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

	connect(this, SIGNAL(loads(DataSetPackage*, QString)), this, SLOT(loadTask(DataSetPackage*, QString)));
	connect(this, SIGNAL(saves(QString, DataSetPackage*)), this, SLOT(saveTask(QString, DataSetPackage*)));
	connect(this, SIGNAL(exports(QString, DataSetPackage*)), this, SLOT(exportTask(QString, DataSetPackage*)));

	_thread.start();
}

void AsyncLoader::load(DataSetPackage *package, const QString &filename)
{
	emit progress("Loading Data Set", 0);
	emit loads(package, filename);
}

void AsyncLoader::save(const QString &filename, DataSetPackage *package)
{
	emit progress("Saving Data Set", 0);
	emit saves(filename, package);
}

void AsyncLoader::exportData(const QString &filename, DataSetPackage *package)
{
	emit progress("Exporting Data Set", 0);
	emit exports(filename, package);
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(DataSetPackage *package, const QString &filename)
{
	try
	{		
		_loader.loadPackage(package, fq(filename), boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		QString name = QFileInfo(filename).baseName();

		emit complete(name, package, filename);
	}
	catch (runtime_error e)
	{
		emit fail(e.what());
	}
	catch (exception e)
	{
		emit fail(e.what());
	}
}

void AsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}

void AsyncLoader::exportTask(const QString &filename, DataSetPackage *package)
{
	try
	{
		CSVExporter::saveDataSet(fq(filename), package, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));
		QString name = QFileInfo(filename).baseName();
		emit exportComplete(name);
	}
	catch (runtime_error e)
	{
		emit exportFail(e.what());
	}
	catch (exception e)
	{
		emit exportFail(e.what());
	}
}

void AsyncLoader::saveTask(const QString &filename, DataSetPackage *package)
{
	QString tempFilename = filename + tq(".tmp");
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

		JASPExporter::saveDataSet(fq(tempFilename), package, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		if ( ! Utils::renameOverwrite(fq(tempFilename), fq(filename)))
			throw runtime_error("File '" + fq(filename) + "' is being used by another application.");

		QString name = QFileInfo(filename).baseName();

		emit saveComplete(name);
	}
	catch (runtime_error e)
	{
		Utils::removeFile(fq(tempFilename));
		emit saveFail(e.what());
	}
	catch (exception e)
	{
		Utils::removeFile(fq(tempFilename));
		emit saveFail(e.what());
	}
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
