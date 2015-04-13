#ifndef ASYNCLOADER_H
#define ASYNCLOADER_H

#include <QObject>
#include <QThread>
#include <QMutex>
#include <QTimer>

#include "dataset.h"
#include "datasetloader.h"
#include "filepackagedata.h"

class AsyncLoader : public QObject
{
	Q_OBJECT

public:
	explicit AsyncLoader(QObject *parent = 0);

	void load(FilePackageData* packageData, const QString &filename);
	void save(const QString &filename, FilePackageData *dataSet);
	void free(DataSet *dataSet);

signals:
	void loads(FilePackageData*, const QString &filename);
	void saves(const QString &filename, FilePackageData *dataSet);
	void progress(const QString &status, int progress);
	void complete(const QString &dataSetName, FilePackageData *packageData, const QString &filename);
	void saveComplete(const QString &dataSetName);
	void fail(const QString &reason);
	void saveFail(const QString &reason);

private slots:
	void loadTask(FilePackageData*, const QString &filename);
	void saveTask(const QString &filename, FilePackageData *dataSet);

private:

	void progressHandler(std::string status, int progress);
	QThread _thread;
	DataSetLoader _loader;
	
};

#endif // ASYNCLOADER_H
