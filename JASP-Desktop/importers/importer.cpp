#include "importer.h"
#include "sharedmemory.h"
#include <iostream>

Importer::Importer(DataSetPackage *packageData)
{
	_packageData = packageData;
}

Importer::~Importer() {}

void Importer::loadDataSet(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback)
{
	ImportDataSet *importDataSet = loadFile(locator, progressCallback);

	int columnCount = importDataSet->columnCount();
	_packageData->setDataSet(SharedMemory::createDataSet()); // this is required incase the loading of the data fails so that the SharedMemory::createDataSet() can be later freed.

	if (columnCount == 0)
		return;
	int rowCount = importDataSet->rowCount();

	setDataSetSize(columnCount, rowCount);

	int colNo = 0;
	for (ImportColumn *importColumn : *importDataSet)
	{
		progressCallback("Loading Data Set", 50 + 50 * colNo / columnCount);
		initColumn(colNo, importColumn);
		colNo++;
	}

	delete importDataSet;
}

void Importer::syncDataSet(const std::string &locator, boost::function<void(const std::string &, int)> progress)
{
	ImportDataSet *importDataSet	= loadFile(locator, progress);
	DataSet *dataSet				= _packageData->dataSet();
	bool rowCountChanged			= importDataSet->rowCount() != dataSet->rowCount();

	std::vector<std::pair<std::string, int> >	newColumns;
	std::vector<std::pair<int, Column *> >		changedColumns;
	std::map<std::string, Column *>				missingColumns;

	Columns &orgColumns	= dataSet->columns();
	int syncColNo		= 0;

	for (Column &orgColumn : orgColumns)
		missingColumns[orgColumn.name()] = &orgColumn;

	for (ImportColumn *syncColumn : *importDataSet)
	{
		std::string syncColumnName = syncColumn->getName();

		if (missingColumns.find(syncColumnName) == missingColumns.end())
			newColumns.push_back(std::pair<std::string, int>(syncColumnName, syncColNo));
		else
		{
			missingColumns.erase(syncColumnName);

			Column &orgColumn	= orgColumns.get(syncColumnName);
			int orgRowCount		= orgColumn.rowCount();
			int syncRowCount	= syncColumn->size();

			if (orgRowCount != syncRowCount)
				changedColumns.push_back(std::pair<int, Column *>(syncColNo, &orgColumn));
			else
			{
				for (int r = 0; r < orgRowCount; r++)
					if (!syncColumn->isValueEqual(orgColumn, r))
					{
						std::cout << "Value Changed, col: " << syncColumnName << ", row " << (r+1) << std::endl;
						std::cout.flush();
						changedColumns.push_back(std::pair<int, Column *>(syncColNo, &orgColumn));
						break;
					}
			}
		}

		syncColNo++;
	}

	std::map<std::string, Column *> changeNameColumns;

	if (missingColumns.size() > 0 && newColumns.size()) {
		for (auto nameColMissing : missingColumns)
			for (auto newColIt = newColumns.begin(); newColIt != newColumns.end(); ++newColIt)
			{
				Column * missingColumn	= nameColMissing.second;
				std::string newColName	= newColIt->first;
				ImportColumn *newValues = importDataSet->getColumn(newColName);

				if (newValues->size()== missingColumn->rowCount())
				{
					bool same_values = true;
					for (size_t r = 0; r < newValues->size(); r++)
						if (!newValues->isValueEqual(*missingColumn, r))
						{
							same_values = false;
							break;
						}

					if (same_values)
					{
						changeNameColumns[newColName] = missingColumn;
						newColumns.erase(newColIt);
						break;
					}
				}
			}
	}

	_syncPackage(importDataSet, newColumns, changedColumns, missingColumns, changeNameColumns, rowCountChanged);

	delete importDataSet;
}

void Importer::fillSharedMemoryColumnWithStrings(const std::vector<std::string> &values, Column &column)
{
	// try to make the column nominal
	bool success = false;
	std::set<int> uniqueValues;
	std::vector<int> intValues;
	intValues.reserve(values.size());
	std::map<int, std::string> emptyValuesMap;

	if (ImportColumn::convertToInt(values, intValues, uniqueValues, emptyValuesMap) && uniqueValues.size() <= 24)
	{
		column.setColumnAsNominalOrOrdinal(intValues, uniqueValues);
		success = true;
	}


	if (!success)
	{
		// try to make the column scale
		std::vector<double> doubleValues;
		doubleValues.reserve(values.size());
		emptyValuesMap.clear();

		if (ImportColumn::convertToDouble(values, doubleValues, emptyValuesMap))
		{
			column.setColumnAsScale(doubleValues);
			success = true;
		}
	}

	if (!success)
	{
		// if it can't be made nominal numeric or scale, make it nominal-text
		emptyValuesMap = column.setColumnAsNominalText(values);
	}

	_packageData->storeInEmptyValues(column.name(), emptyValuesMap);
}

DataSet* Importer::setDataSetSize(int columnCount, int rowCount)
{
	DataSet *dataSet	= _packageData->dataSet();
	bool success		= true;
	do
	{
		try {
			dataSet->setColumnCount(columnCount);
			if (rowCount > 0)
				dataSet->setRowCount(rowCount);

			success = true;
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				std::cout << "Enlarge dataset " << std::endl;
				std::cout.flush();

				dataSet = SharedMemory::enlargeDataSet(dataSet);
				success = false;
			}
			catch (std::exception &e)
			{
				throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (std::exception &e)
		{
			std::cout << "Exception " << e.what() << "\n";
			std::cout.flush();
		}
		catch (...)
		{
			std::cout << "something else\n ";
			std::cout.flush();
		}
	}
	while ( ! success);

	_packageData->setDataSet(dataSet);
	return dataSet;
}

void Importer::initColumn(std::string colName, ImportColumn *importColumn)
{
	initColumn(_packageData->dataSet()->getColumnIndex(colName), importColumn);
}

void Importer::initColumn(int colNo, ImportColumn *importColumn)
{
	bool success = true;

	do {
		try {
			Column &column = _packageData->dataSet()->column(colNo);
			column.setName(importColumn->getName());
			fillSharedMemoryColumn(importColumn, column);
			success = true;
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {
				_packageData->setDataSet(SharedMemory::enlargeDataSet(_packageData->dataSet()));
				success = false;
			}
			catch (std::exception &e)	{ throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");	}
		}
		catch (std::exception e)	{ std::cout << "n " << e.what() << std::endl;		}
		catch (...)					{ std::cout << "something else\n " << std::endl;	}

	} while (success == false);
}


void Importer::_syncPackage(
		ImportDataSet								*syncDataSet,
		std::vector<std::pair<std::string, int>>	&newColumns,
		std::vector<std::pair<int, Column *>>		&changedColumns,
		std::map<std::string, Column *>				&missingColumns,
		std::map<std::string, Column *>				&changeNameColumns,
		bool										rowCountChanged)

{
	std::vector<std::string>			_changedColumns;
	std::vector<std::string>			_missingColumns;
	std::map<std::string, std::string>	_changeNameColumns;

	for (auto changeNameColumnIt : changeNameColumns)
	{
		std::string newColName	= changeNameColumnIt.first;
		Column *changedCol		= changeNameColumnIt.second;
		missingColumns.erase(changedCol->name());
		std::cout << "Column name changed, from: " << changedCol->name() << " to " << newColName << std::endl;
		_changeNameColumns[changedCol->name()] = newColName;
		changedCol->setName(newColName);
	}

	int colNo = _packageData->dataSet()->columnCount();

	if (changedColumns.size() > 0)
	{
		if (rowCountChanged)
			setDataSetSize(colNo, syncDataSet->rowCount());

		for (auto indexColChanged : changedColumns)
		{
			std::cout << "Column changed " << indexColChanged.first << std::endl;
			//Column &column		= _packageData->dataSet()->column(indexColChanged.first);
			std::string colName	= indexColChanged.second->name();
			_changedColumns.push_back(colName);
			initColumn(colName, syncDataSet->getColumn(colName));
		}
	}

	if (newColumns.size() > 0)
	{
		//setDataSetSize(colNo + newColumns.size(), syncDataSet->rowCount());

		for (auto it = newColumns.begin(); it != newColumns.end(); ++it, ++colNo)
		{
			increaseDataSetColCount(syncDataSet->rowCount());

			std::cout << "New column " << it->first << std::endl;
			initColumn(_packageData->dataSet()->columnCount() - 1, syncDataSet->getColumn(it->first));
		}
	}

	if (missingColumns.size() > 0)
	{

		for (auto misColIt = missingColumns.begin(); misColIt != missingColumns.end(); ++misColIt)
		{
			std::string columnName = misColIt->first;

			if(!_packageData->isColumnComputed(columnName))
			{
				std::cout << "Column deleted " << columnName << std::endl;
				_missingColumns.push_back(columnName);
				_packageData->removeColumn(columnName);
			}
		}
	}

	_packageData->dataChanged(_packageData, _changedColumns, _missingColumns, _changeNameColumns);

}
