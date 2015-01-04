#include "asyncloader.h"

#include <iostream>
#include <fstream>
#include <QTimer>
#include <QFileInfo>

#include <boost/bind.hpp>

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{
	this->moveToThread(&_thread);

	connect(this, SIGNAL(loads(QString)), this, SLOT(loadTask(QString)));

	_thread.start();
}

void AsyncLoader::load(const QString &filename)
{
	emit progress("Loading Data Set", 0);
	emit loads(filename);
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(const QString &filename)
{
	try
	{
		DataSet *dataSet = _loader.loadDataSet(filename.toStdString(), boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

		QString name = QFileInfo(filename).baseName();

		emit complete(name, dataSet);
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
