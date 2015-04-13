#include "asyncloader.h"

#include "exporters/jaspexporter.h"

#include <iostream>
#include <fstream>
#include <QTimer>
#include <QFileInfo>

#include <boost/bind.hpp>

#include "qutils.h"

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{
	this->moveToThread(&_thread);

	connect(this, SIGNAL(loads(FilePackageData*, QString)), this, SLOT(loadTask(FilePackageData*, QString)));
	connect(this, SIGNAL(saves(QString, FilePackageData*)), this, SLOT(saveTask(QString, FilePackageData*)));

	_thread.start();
}

void AsyncLoader::load(FilePackageData *packageData, const QString &filename)
{
	emit progress("Loading Data Set", 0);
	emit loads(packageData, filename);
}

void AsyncLoader::save(const QString &filename, FilePackageData *packageData)
{
	emit progress("Saving Data Set", 0);
	emit saves(filename, packageData);
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(FilePackageData *packageData, const QString &filename)
{
	try
	{		
		_loader.loadPackage(packageData, fq(filename), boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		QString name = QFileInfo(filename).baseName();

		emit complete(name, packageData, filename);
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

void AsyncLoader::saveTask(const QString &filename, FilePackageData *dataSet)
{
	try
	{
		QString tempFilename = filename + tq(".tmp");

		JASPExporter::saveDataSet(fq(tempFilename), dataSet, boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		QFileInfo tempFileInfo = QFileInfo(tempFilename);
		if (!tempFileInfo.exists() || tempFileInfo.size() == 0)
			throw runtime_error("Saving file has failed");

		QFileInfo fileInfo = QFileInfo(filename);
		if (fileInfo.exists())
			QFile::remove(filename);

		if (!QFile::copy(tempFilename, filename))
			throw runtime_error("Saving file has failed");

		fileInfo.refresh();
		if (!fileInfo.exists() || fileInfo.size() != tempFileInfo.size())
			throw runtime_error("Saving file has failed");

		QFile::remove(tempFilename);

		QString name = QFileInfo(filename).baseName();

		emit saveComplete(name);
	}
	catch (runtime_error e)
	{
		emit saveFail(e.what());
	}
	catch (exception e)
	{
		emit saveFail(e.what());
	}
}
