#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void load(const QString &filename);
	void free(DataSet *dataSet);

signals:
	void loads(const QString &filename);
	void progress(const QString &status, int progress);
	void complete(DataSet *dataSet);
	void fail(const QString &reason);

private slots:
	void loadTask(const QString &filename);

private:

	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
