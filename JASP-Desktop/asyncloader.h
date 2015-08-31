#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"
#include "datasetpackage.h"
#include "fileevent.h"

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void io(FileEvent *event, DataSetPackage *package);
	void free(DataSet *dataSet);

signals:
	void beginLoad(FileEvent*, DataSetPackage*);
	void beginSave(FileEvent*, DataSetPackage*);
	void progress(const QString &status, int progress);

private slots:
	void loadTask(FileEvent *event, DataSetPackage *package);
	void saveTask(FileEvent *event, DataSetPackage *package);

private:

	void sleep(int ms);
	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
