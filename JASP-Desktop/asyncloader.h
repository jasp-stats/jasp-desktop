#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"
#include "datasetpackage.h"

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void load(DataSetPackage* package, const QString &filename);
	void save(const QString &filename, DataSetPackage *package);
	void free(DataSet *dataSet);

signals:
	void loads(DataSetPackage*, const QString &filename);
	void saves(const QString &filename, DataSetPackage *dataSet);
	void progress(const QString &status, int progress);
	void complete(const QString &dataSetName, DataSetPackage *packageData, const QString &filename);
	void saveComplete(const QString &dataSetName);
	void fail(const QString &reason);
	void saveFail(const QString &reason);

private slots:
	void loadTask(DataSetPackage *package, const QString &filename);
	void saveTask(const QString &filename, DataSetPackage *package);

private:

	void sleep(int ms);
	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
