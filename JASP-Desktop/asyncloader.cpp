#include "asyncloader.h"

#include <iostream>
#include <fstream>
#include <QTimer>

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent)
{
	this->moveToThread(&_thread);

	connect(this, SIGNAL(loads(QString)), this, SLOT(loadTask(QString)));

	_loader.progress.connect(boost::bind(&AsyncLoader::progressHandler, this, _1, _2));

	_thread.start();
}

void AsyncLoader::load(QString filename)
{
	emit progress("Loading Data Set", 0);
	emit loads(filename);
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void AsyncLoader::loadTask(QString filename)
{
	DataSet *dataSet = _loader.loadDataSet(filename.toStdString());

	emit complete(dataSet);
}

void AsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}
