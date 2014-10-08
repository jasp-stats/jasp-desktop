#include "asyncloader.h"

#include <iostream>
#include <fstream>
#include <QTimer>

using namespace std;

AsyncLoader::AsyncLoader(QObject *parent) :
	QObject(parent), _loader(NULL, &_mutex)
{
	// this is a complicated proxy object, which i'm not sure is necessary.
	// there are reports of crashes, and so i added mutexes in for cross thread synchronization
	// unsure if that's what the problem is, because i would expect Qt to provide synchronization
	// automatically across threads with it's signals and slots mechanism
	// but we'll see if this works

	connect(&_loader, SIGNAL(progress(QString,int)), this, SLOT(progressSlot(QString,int)));
	connect(&_loader, SIGNAL(complete(DataSet*)), this, SLOT(completeSlot(DataSet*)));
	connect(&_loader, SIGNAL(fail(QString)), this, SLOT(failSlot(QString)));
}

void AsyncLoader::load(const QString &filename)
{
	_loader.load(filename);
}

void AsyncLoader::free(DataSet *dataSet)
{
	_loader.free(dataSet);
}

void AsyncLoader::progressSlot(const QString &status, int progress)
{
	emit this->progress(status, progress);
}

void AsyncLoader::completeSlot(DataSet *dataSet)
{
	_mutex.lock();
	emit complete(dataSet);
	_mutex.unlock();
}

void AsyncLoader::failSlot(const QString &reason)
{
	emit fail(reason);
}


XTAsyncLoader::XTAsyncLoader(QObject *parent, QMutex *mutex) :
	QObject(parent)
{
	_mutex = mutex;
	this->moveToThread(&_thread);

	connect(this, SIGNAL(loads(QString)), this, SLOT(loadTask(QString)));

	_loader.progress.connect(boost::bind(&XTAsyncLoader::progressHandler, this, _1, _2));

	_thread.start();
}

void XTAsyncLoader::load(const QString &filename)
{
	emit progress("Loading Data Set", 0);
	emit loads(filename);
}

void XTAsyncLoader::free(DataSet *dataSet)
{
	_loader.freeDataSet(dataSet);
}

void XTAsyncLoader::loadTask(const QString &filename)
{
	try {
		_mutex->lock();
		DataSet *dataSet = _loader.loadDataSet(filename.toStdString());
		_mutex->unlock();
		emit complete(dataSet);
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

void XTAsyncLoader::progressHandler(string status, int progress)
{
	emit this->progress(QString::fromUtf8(status.c_str(), status.length()), progress);
}

