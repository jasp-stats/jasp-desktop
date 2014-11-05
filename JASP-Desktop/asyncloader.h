#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"


class XTAsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit XTAsyncLoader(QObject *parent, QMutex *mutex);

	void load(const QString &filename);
	void free(DataSet *dataSet);

signals:
	void loads(const QString &filename);
	void progress(const QString &status, int progress);
	void complete(const QString &dataSetName, DataSet *dataSet);
	void fail(const QString &reason);

private slots:
	void loadTask(const QString &filename);

private:

	void progressHandler(std::string status, int progress);
	QMutex *_mutex;
	QThread _thread;
	DataSetLoader _loader;

};

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void load(const QString &filename);
	void free(DataSet *dataSet);

signals:
	void progress(const QString &status, int progress);
	void complete(const QString &dataSetName, DataSet *dataSet);
	void fail(const QString &reason);

private slots:
	void progressSlot(const QString &status, int progress);
	void completeSlot(const QString &dataSetName, DataSet *dataSet);
	void failSlot(const QString &reason);

private:

	QMutex _mutex;
	XTAsyncLoader _loader;
	void progressHandler(std::string status, int progress);
	
};

#endif // ASYNCLOADER_H
