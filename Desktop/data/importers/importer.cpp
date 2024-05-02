#include "importer.h"
#include "utilities/qutils.h"
#include "log.h"
#include <QVariant>
#include "../datasetpackage.h"

Importer::Importer() 
{
	//Turns out that jasp importer and jaspiporter old are not Importers... Great...
	DataSetPackage::pkg()->setIsJaspFile(false);
}

Importer::~Importer() {}

void Importer::loadDataSet(const std::string &locator, std::function<void(int)> progressCallback)
{
	DataSetPackage::pkg()->beginLoadingData();

	JASPTIMER_RESUME(Importer::loadDataSet loadFile);
	ImportDataSet *importDataSet = loadFile(locator, progressCallback);
	JASPTIMER_STOP(Importer::loadDataSet loadFile);
	
	JASPTIMER_RESUME(Importer::loadDataSet createDataSetAndLoad);
	int columnCount = importDataSet->columnCount();

	if (columnCount > 0)
	{
		int rowCount = importDataSet->rowCount();

		DataSetPackage::pkg()->dataSet()->beginBatchedToDB();
		DataSetPackage::pkg()->dataSet()->setDescription(importDataSet->description());
		DataSetPackage::pkg()->setDataSetSize(columnCount, rowCount);


		int colNo = 0;
		for (ImportColumn *& importColumn : *importDataSet)
		{
			progressCallback(50 + 25 * colNo / columnCount);
			initColumn(colNo, importColumn);
			delete importColumn;
			importColumn = nullptr;
			colNo++;
		}

		DataSetPackage::pkg()->dataSet()->endBatchedToDB([&](float f){ progressCallback(75 + f * 25); });
	}
	JASPTIMER_STOP(Importer::loadDataSet createDataSetAndLoad);
	
	importDataSet->clearColumns();
	delete importDataSet;
	DataSetPackage::pkg()->endLoadingData();
}

void Importer::initColumn(QVariant colId, ImportColumn *importColumn)
{
	JASPTIMER_SCOPE(Importer::initColumn);
	initColumnWithStrings(colId, importColumn->name(),  importColumn->allValuesAsStrings(), importColumn->allLabelsAsStrings(), importColumn->title(), importColumn->getColumnType(), importColumn->allEmptyValuesAsStrings());
}

void Importer::initColumnWithStrings(QVariant colId, const std::string &newName, const std::vector<std::string> &values, const std::vector<std::string> &labels, const std::string & title, columnType desiredType, const stringset & emptyValues) 
{ 
	DataSetPackage::pkg()->initColumnWithStrings(colId, newName, values, labels, title, desiredType, emptyValues); 																																							 
}

void Importer::syncDataSet(const std::string &locator, std::function<void(int)> progress)
{
	ImportDataSet *	importDataSet	= loadFile(locator, progress);
	bool			rowCountChanged	= importDataSet->rowCount() != DataSetPackage::pkg()->dataRowCount();
	int				syncColNo		= 0;

	std::vector<std::pair<std::string, int> >	newColumns;
	std::vector<std::pair<int, std::string> >	changedColumns; //import col index and original column name
	strstrmap									changeNameColumns; //origname -> newname
	stringvec									orgColumnNames(DataSetPackage::pkg()->getColumnNames()),
												newOrder;
	stringset									missingColumns(orgColumnNames.begin(), orgColumnNames.end());

	//If the following gives errors trhen it probably should be somewhere else:
	for (const std::string & colName : orgColumnNames)
		if (DataSetPackage::pkg()->isColumnComputed(colName)) // make sure "missing" columns aren't actually computed columns
			missingColumns.erase(colName);

	for (ImportColumn *syncColumn : *importDataSet)
	{
		std::string syncColumnName = syncColumn->name();
		
		newOrder.push_back(syncColumnName);

		if (missingColumns.count(syncColumnName) == 0)
			newColumns.push_back(std::pair<std::string, int>(syncColumnName, syncColNo));
		else
		{
			missingColumns.erase(syncColumnName);

			if(DataSetPackage::pkg()->isColumnDifferentFromStringValues(syncColumnName, syncColumn->title(), syncColumn->allValuesAsStrings(), syncColumn->allLabelsAsStrings(), syncColumn->allEmptyValuesAsStrings()))
			{
				Log::log() << "Something changed in column: " << syncColumnName << std::endl;
				changedColumns.push_back(std::pair<int, std::string>(syncColNo, syncColumnName));
			}
		}

		syncColNo++;
	}

	if (missingColumns.size() > 0 && newColumns.size() > 0)
		for (const std::string & nameMissing : missingColumns)
			for (auto newColIt = newColumns.begin(); newColIt != newColumns.end(); ++newColIt)
			{
				const std::string	& newColName	= newColIt->first;
				ImportColumn		* newColumn		= importDataSet->getColumn(newColName);

				if(!DataSetPackage::pkg()->isColumnDifferentFromStringValues(nameMissing, newColumn->title(), newColumn->allValuesAsStrings(), newColumn->allLabelsAsStrings(), newColumn->allEmptyValuesAsStrings()))
				{
					changeNameColumns[nameMissing] = newColName;
					newColumns.erase(newColIt);
					break;
				}
			}

	for (auto & changeNameColumnIt : changeNameColumns)
		missingColumns.erase(changeNameColumnIt.first);

	if (newColumns.size() > 0 || changedColumns.size() > 0 || missingColumns.size() > 0 || changeNameColumns.size() > 0 || orgColumnNames != newOrder || rowCountChanged)
			_syncPackage(importDataSet, newColumns, changedColumns, missingColumns, changeNameColumns, newOrder, rowCountChanged);

	DataSetPackage::pkg()->setManualEdits(false);
	delete importDataSet;
}

void Importer::_syncPackage(
		ImportDataSet									*	syncDataSet,
		const std::vector<std::pair<std::string, int>>	&	newColumns,
		const std::vector<std::pair<int, std::string>>	&	changedColumns, // import col index and original (old) col name
		const stringset									&	missingColumns,
		const strstrmap									&	changeNameColumns, //origname -> newname
		const stringvec									&	newColumnOrder,
		bool											rowCountChanged)

{
	if( ! emit DataSetPackage::pkg()->checkDoSync())
		return;

	DataSetPackage::pkg()->beginSynchingData();

	stringvec		_changedColumns,
					_missingColumns;

	for (const auto & changeNameColumnIt : changeNameColumns)
	{
		const std::string	& oldColName = changeNameColumnIt.first,
							& newColName = changeNameColumnIt.second;

		Log::log() << "Column name changed, from: " << oldColName << " to " << newColName << std::endl;

		DataSetPackage::pkg()->renameColumn(oldColName, newColName);
	}

	int colNo = DataSetPackage::pkg()->columnCount();
	DataSetPackage::pkg()->setDataSetRowCount(syncDataSet->rowCount());

	for (const auto & indexColChanged : changedColumns)
	{
		Log::log() << "Column changed " << indexColChanged.second << std::endl;

		std::string colName	= indexColChanged.second;
		_changedColumns.push_back(colName);
		initColumn(tq(colName), syncDataSet->getColumn(indexColChanged.first));
	}

	if (newColumns.size() > 0)
	{
		for (auto it = newColumns.begin(); it != newColumns.end(); ++it, ++colNo)
		{
			DataSetPackage::pkg()->increaseDataSetColCount(syncDataSet->rowCount());
			Log::log() << "New column " << it->first << std::endl;

			initColumn(DataSetPackage::pkg()->dataColumnCount() - 1, syncDataSet->getColumn(it->first));
		}
	}

	if (missingColumns.size() > 0)
		for (const std::string & columnName : missingColumns)
			if(!DataSetPackage::pkg()->isColumnComputed(columnName))
			{
				Log::log() << "Column deleted " << columnName << std::endl;

				_missingColumns.push_back(columnName);
				DataSetPackage::pkg()->removeColumn(columnName);
			}

	DataSetPackage::pkg()->endSynchingData(_changedColumns, _missingColumns, changeNameColumns, rowCountChanged, newColumns.size() > 0);
	
	if(newColumnOrder.size() > 0)
		DataSetPackage::pkg()->columnsReorder(newColumnOrder);
}
