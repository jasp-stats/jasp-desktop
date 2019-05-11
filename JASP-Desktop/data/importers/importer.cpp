#include "importer.h"
#include "sharedmemory.h"

#include "utilities/settings.h"
#include "log.h"

Importer::Importer(DataSetPackage *packageData)
{
	_packageData = packageData;
}

Importer::~Importer() {}

void Importer::loadDataSet(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback)
{
	bool enginesLoaded = !_packageData->enginesInitializing();

	if(enginesLoaded)
		_packageData->pauseEngines();

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
	if(enginesLoaded)
		_packageData->resumeEngines();
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
	{
		std::string colName = orgColumn.name();

		// make sure "missing" columns aren't actually computed columns
		if(!_packageData->isColumnComputed(colName))
			missingColumns[orgColumn.name()] = &orgColumn;
	}

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
						Log::log() << "Value Changed, col: " << syncColumnName << ", row " << (r+1) << std::endl;
						changedColumns.push_back(std::pair<int, Column *>(syncColNo, &orgColumn));
						break;
					}
			}
		}

		syncColNo++;
	}

	std::map<std::string, Column *> changeNameColumns;

	if (missingColumns.size() > 0 && newColumns.size() > 0) {
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

	int thresholdScale = Settings::defaultValue(Settings::THRESHOLD_SCALE).toInt();
	if (Settings::value(Settings::USE_CUSTOM_THRESHOLD_SCALE).toBool())
		thresholdScale = Settings::value(Settings::THRESHOLD_SCALE).toInt();

	if (ImportColumn::convertToInt(values, intValues, uniqueValues, emptyValuesMap) && uniqueValues.size() <= thresholdScale)
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

				Log::log() << "Enlarge dataset " << std::endl;

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
			Log::log() << "Exception " << e.what() << std::endl;
		}
		catch (...)
		{
			Log::log() << "something else" << std::endl;
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
			catch (std::exception &e)
			{
				throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (std::exception e)	{ Log::log() << "n " << e.what() << std::endl;		}
		catch (...)					{ Log::log() << "something else\n " << std::endl;	}

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
	bool enginesLoaded = !_packageData->enginesInitializing();

	if(enginesLoaded)
		_packageData->pauseEngines();
	_packageData->dataSet()->setSynchingData(true);

	std::vector<std::string>			_changedColumns;
	std::vector<std::string>			_missingColumns;
	std::map<std::string, std::string>	_changeNameColumns;

	for (auto changeNameColumnIt : changeNameColumns)
	{
		std::string newColName	= changeNameColumnIt.first;
		Column *changedCol		= changeNameColumnIt.second;
		missingColumns.erase(changedCol->name());

		Log::log() << "Column name changed, from: " << changedCol->name() << " to " << newColName << std::endl;

		_changeNameColumns[changedCol->name()] = newColName;
		changedCol->setName(newColName);
	}

	std::map<int, std::string> tempChangedNameList;
	if (changedColumns.size() > 0)
		for (auto indexColChanged : changedColumns)
			tempChangedNameList[indexColChanged.first] = indexColChanged.second->name();


	int colNo = _packageData->dataSet()->columnCount();
	setDataSetRowCount(syncDataSet->rowCount());

	if (changedColumns.size() > 0)
	{
		for (auto indexColChanged : changedColumns)
		{
			Log::log() << "Column changed " << indexColChanged.first << std::endl;

			//Column &column		= _packageData->dataSet()->column(indexColChanged.first);
			std::string colName	= tempChangedNameList[indexColChanged.first];//indexColChanged.second->name();
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
			Log::log() << "New column " << it->first << std::endl;

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
				Log::log() << "Column deleted " << columnName << std::endl;

				_missingColumns.push_back(columnName);
				_packageData->removeColumn(columnName);
			}
		}
	}

	_packageData->dataSet()->setSynchingData(false);
	_packageData->dataChanged(_packageData, _changedColumns, _missingColumns, _changeNameColumns, rowCountChanged);

	if(enginesLoaded)
		_packageData->resumeEngines();
}
