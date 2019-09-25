#ifndef IMPORTER_H
#define IMPORTER_H

#include <boost/function.hpp>
#include "../datasetpackage.h"
#include "importdataset.h"

class ImportDataSet;
class ImportColumn;

class Importer
{
public:
	Importer(DataSetPackage *packageData);
	virtual ~Importer();
	void loadDataSet(const std::string &locator, boost::function<void (const std::string &, int)> progressCallback);
	void syncDataSet(const std::string &locator, boost::function<void (const std::string &, int)> progressCallback);

protected:
	virtual ImportDataSet* loadFile(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback) = 0;

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	virtual void initColumn(QVariant colId, ImportColumn *importColumn);

	void initColumnWithStrings(QVariant colId, std::string newName, const std::vector<std::string> &values);

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	bool						initColumnAsNominalOrOrdinal(	QVariant colID,			std::string newName, const std::vector<int>			& values,	const std::set<int> &uniqueValues, bool is_ordinal = false) { return _packageData->initColumnAsNominalOrOrdinal(colID, newName, values, uniqueValues, is_ordinal);	}

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	std::map<int, std::string>	initColumnAsNominalText(		QVariant colID,			std::string newName, const std::vector<std::string>	& values)																{ return _packageData->initColumnAsNominalText(colID, newName, values);									}

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	bool						initColumnAsScale(				QVariant colID,			std::string newName, const std::vector<double>		& values)																{ return _packageData->initColumnAsScale(colID, newName, values);										}

	void						storeInEmptyValues(std::string columnName, std::map<int, std::string> emptyValues)																									{ _packageData->storeInEmptyValues(columnName, emptyValues);											}
	void						resetEmptyValues()																																									{ _packageData->resetEmptyValues();																		}

	DataSetPackage *_packageData;

private:
	void _syncPackage(
			ImportDataSet								*	syncDataSet,
			std::vector<std::pair<std::string, int>>	&	newColumns,
			std::vector<std::pair<int, std::string>>	&	changedColumns,
			std::set<std::string>						&	missingColumns,
			std::map<std::string, std::string>			&	changeNameColumns,
			bool											rowCountChanged);
};

#endif // IMPORTER_H
