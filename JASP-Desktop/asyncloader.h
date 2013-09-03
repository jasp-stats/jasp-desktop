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

	void load(QString filename);

	void free(DataSet *dataSet);
signals:
	void loads(QString filename);
	void progress(QString status, int progress);
	void complete(DataSet *dataSet);

private slots:
	void loadTask(QString filename);

private:

	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
