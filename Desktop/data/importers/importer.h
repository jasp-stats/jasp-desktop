#ifndef IMPORTER_H
#define IMPORTER_H

#include <QMutex>
#include <QObject>
#include "importdataset.h"
#include <boost/function.hpp>
#include <QCoreApplication>

class DataSet;

///
/// Base class for all importers
/// These are always run in a different thread (through AsyncLoader) than the rest of the application
class Importer : public QObject
{
	Q_OBJECT
public:
	Importer();
	virtual ~Importer();
	void loadDataSet(const std::string &locator, DataSet * dataSet, std::function<void (int)> progressCallback);
	void syncDataSet(const std::string &locator, DataSet * dataSet, std::function<void (int)> progressCallback);
	
	virtual bool importerDeliversLabels() const { return true; } //They all do except csv, so for synchronization to work we want labels to be ignored for csv when synching, this to allow people to enter better labels and not lose them on every sync
	
private slots:
	void						importColumnFinished(ImportColumn *column, bool doCallback);
	
protected:
    virtual ImportDataSet*		loadFile(const std::string &locator, std::function<void(int)> progressCallback) = 0;
	

private:
	void _syncPackage(
			const std::vector<std::pair<std::string, int>>	&	newColumns,
			const std::vector<std::pair<int, std::string>>	&	changedColumns,
			const stringset									&	missingColumns,
			const strstrmap									&	changeNameColumns,
			const stringvec									&	newOrder,	///<can be empty
			bool												rowCountChanged,
			size_t												newColCount, 
			std::function<void(int)> progress);
	
protected:
	bool						_synching = false;
	
private:	
	ImportDataSet *				_importDataSet;
	QMutex						_serialFinishing;
	std::set<ImportColumn*>		_waitingFor;
	std::function<void(int)>	_progressCallback;
	
};

#endif // IMPORTER_H
