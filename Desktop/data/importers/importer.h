#ifndef IMPORTER_H
#define IMPORTER_H

#include <boost/function.hpp>
#include "../datasetpackage.h"
#include "importdataset.h"
#include "timers.h"

class ImportDataSet;
class ImportColumn;
#include <QCoreApplication>
#include "qutils.h"

///
/// Base class for all importers
/// These are always run in a different thread (through AsyncLoader) than the rest of the application
class Importer
{
	Q_DECLARE_TR_FUNCTIONS(Importer)
public:
	Importer() {}
	virtual ~Importer();
	void loadDataSet(const std::string &locator, boost::function<void (int)> progressCallback);
	void syncDataSet(const std::string &locator, boost::function<void (int)> progressCallback);

protected:
	virtual ImportDataSet* loadFile(const std::string &locator, boost::function<void(int)> progressCallback) = 0;

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	virtual void initColumn(QVariant colId, ImportColumn *importColumn);

	void initColumnWithStrings(QVariant colId, std::string newName, const std::vector<std::string> &values);

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	bool						initColumnAsNominalOrOrdinal(	QVariant colID,			std::string newName, const std::vector<int>			& values, bool is_ordinal = false)	{ return DataSetPackage::pkg()->initColumnAsNominalOrOrdinal(colID, newName, values, is_ordinal);				}

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	std::map<int, std::string>	initColumnAsNominalText(		QVariant colID,			std::string newName, const std::vector<std::string>	& values)							{ return DataSetPackage::pkg()->initColumnAsNominalText(colID, newName, values);									}

	///colID can be either an integer (the column index in the data) or a string (the (old) name of the column in the data)
	bool						initColumnAsScale(				QVariant colID,			std::string newName, const std::vector<double>		& values)							{ return DataSetPackage::pkg()->initColumnAsScale(colID, newName, values);										}

	void						storeInEmptyValues(std::string columnName, std::map<int, std::string> emptyValues)																{ DataSetPackage::pkg()->storeInEmptyValues(columnName, emptyValues);											}
	void						resetEmptyValues()																																{ DataSetPackage::pkg()->resetEmptyValues();																		}

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
