#ifndef IMPORTER_H
#define IMPORTER_H

#include <boost/function.hpp>
#include "importdataset.h"

class ImportDataSet;
class ImportColumn;
#include <QCoreApplication>

///
/// Base class for all importers
/// These are always run in a different thread (through AsyncLoader) than the rest of the application
class Importer
{
	Q_DECLARE_TR_FUNCTIONS(Importer)
public:
	Importer();
	virtual ~Importer();
    void loadDataSet(const std::string &locator, std::function<void (int)> progressCallback);
    void syncDataSet(const std::string &locator, std::function<void (int)> progressCallback);
	
	virtual bool importerDeliversLabels() const { return true; } //They all do except csv, so for synchronization to work we want labels to be ignored for csv when synching, this to allow people to enter better labels and not lose them on every sync

protected:
    virtual ImportDataSet* loadFile(const std::string &locator, std::function<void(int)> progressCallback) = 0;

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	virtual void initColumn(QVariant colId, ImportColumn *importColumn);

	void initColumnWithStrings(QVariant colId, const std::string & newName, const std::vector<std::string> & values, const std::vector<std::string> & labels=stringvec(), const std::string & title="", columnType desiredTyp = columnType::unknown, const stringset & emptyValues = {});
	
	bool	_synching = false;

private:
	void _syncPackage(
			ImportDataSet									*	syncDataSet,
			const std::vector<std::pair<std::string, int>>	&	newColumns,
			const std::vector<std::pair<int, std::string>>	&	changedColumns,
			const stringset									&	missingColumns,
			const strstrmap									&	changeNameColumns,
			const stringvec									&	newOrder,	///<can be empty
			bool											rowCountChanged);
};

#endif // IMPORTER_H
