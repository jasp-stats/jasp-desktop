#include "importer.h"
#include "sharedmemory.h"
#include <iostream>

Importer::Importer(DataSetPackage *packageData)
{
	_packageData = packageData;
}

void Importer::loadDataSet(const string &locator, boost::function<void(const string &, int)> progressCallback)
{
	ImportDataSet *importDataSet = loadFile(locator, progressCallback);

	int columnCount = importDataSet->columnCount();
	_packageData->dataSet = SharedMemory::createDataSet(); // this is required incase the loading of the data fails so that the SharedMemory::createDataSet() can be later freed.

	if (columnCount == 0)
		return;
	int rowCount = importDataSet->rowCount();

	setDataSetSize(columnCount, rowCount);

	int colNo = 0;
	for (ImportColumns::iterator it = importDataSet->begin(); it != importDataSet->end(); ++it, ++colNo)
	{
		progressCallback("Loading Data Set", 50 + 50 * colNo / columnCount);
		initColumn(colNo, it->second);
	}

	delete importDataSet;
}

void Importer::syncDataSet(const string &locator, boost::function<void(const string &, int)> progress)
{
	DataSet *dataSet = _packageData->dataSet;
	ImportDataSet *importDataSet = loadFile(locator, progress);

	bool rowCountChanged = importDataSet->rowCount() != dataSet->rowCount();
	vector<pair<string, int> > newColumns;
	vector<pair<string, int> > changedColumns;
	map<string, Column *> missingColumns;

	Columns &orgColumns = dataSet->columns();
	int syncColNo = 0;

	for (Columns::iterator orgColIt = orgColumns.begin(); orgColIt != orgColumns.end(); ++orgColIt)
	{
		Column &orgColumn = *orgColIt;
		missingColumns[orgColumn.name()] = &orgColumn;
	}

	for (ImportColumns::iterator syncColumnIt = importDataSet->begin(); syncColumnIt != importDataSet->end(); ++syncColumnIt, ++syncColNo)
	{
		ImportColumn *syncColumn = syncColumnIt->second;
		string syncColumnName = syncColumn->getName();

		if (missingColumns.find(syncColumnName) == missingColumns.end()) {
			newColumns.push_back(pair<string, int>(syncColumnName, syncColNo));
		}
		else
		{
			Column &orgColumn = orgColumns.get(syncColumnName);
			missingColumns.erase(syncColumnName);
			int orgRowCount = orgColumn.rowCount();
			int syncRowCount = syncColumn->size();
			if (orgRowCount != syncRowCount)
			{
				changedColumns.push_back(pair<string, int>(syncColumnName, syncColNo));
			}
			else
			{
				for (int r = 0; r < orgRowCount; r++)
				{
					if (!syncColumn->isValueEqual(r, orgColumn[r]))
					{
						changedColumns.push_back(pair<string, int>(syncColumnName, syncColNo));
						break;
					}
				}
			}
		}
	}

	_syncPackage(importDataSet, newColumns, changedColumns, missingColumns, rowCountChanged);

	delete importDataSet;
}

DataSet* Importer::setDataSetSize(int columnCount, int rowCount)
{
	DataSet *dataSet = _packageData->dataSet;
	bool success = true;
	do
	{
		try {
			success = true;
			dataSet->setColumnCount(columnCount);
			if (rowCount > 0)
				dataSet->setRowCount(rowCount);

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				dataSet = SharedMemory::enlargeDataSet(dataSet);
				success = false;
			}
			catch (exception &e)
			{
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (exception &e)
		{
			cout << "n " << e.what() << "\n";
			cout.flush();
		}
		catch (...)
		{
			cout << "something else\n ";
			cout.flush();
		}
	}
	while ( ! success);

	_packageData->dataSet = dataSet;
	return dataSet;
}


void Importer::initColumn(int colNo, ImportColumn *importColumn)
{
	bool success;

	do {

		success = true;

		try {
			Column &column = _packageData->dataSet->column(colNo);
			initSharedMemoryColumn(importColumn, column);

		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {

				_packageData->dataSet = SharedMemory::enlargeDataSet(_packageData->dataSet);
				success = false;
			}
			catch (exception &e)
			{
				throw runtime_error("Out of memory: this data set is too large for your computer's available memory");
			}
		}
		catch (exception e)
		{
			cout << "n " << e.what();
			cout.flush();
		}
		catch (...)
		{
			cout << "something else\n ";
			cout.flush();
		}

	} while (success == false);
}


void Importer::_syncPackage(
		ImportDataSet *syncDataSet,
		vector<pair<string, int> > &newColumns,
		vector<pair<string, int> > &changedColumns,
		map<string, Column *> &missingColumns,
		bool rowCountChanged)

{
	map<string, Column *> changeNameColumns;
	DataSet *dataSet = _packageData->dataSet;

	if (missingColumns.size() > 0 && newColumns.size()) {
		for (map<string, Column *>::iterator misColIt = missingColumns.begin(); misColIt != missingColumns.end(); ++misColIt)
		{
			Column* missingColumn = misColIt->second;
			for (vector<pair<string, int> >::iterator newColIt = newColumns.begin(); newColIt != newColumns.end(); ++newColIt)
			{
				string newColName = newColIt->first;
				ImportColumn *newValues = syncDataSet->getColumn(newColName);
				if ((int)(newValues->size()) == missingColumn->rowCount())
				{
					bool same_values = true;
					for (int r = 0; r < newValues->size(); r++)
					{
						if (newValues->isValueEqual(r, (*missingColumn)[r]))
						{
							same_values = false;
							break;
						}
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
	}

	for (map<string, Column *>::iterator changeNameColumnIt = changeNameColumns.begin(); changeNameColumnIt != changeNameColumns.end(); ++changeNameColumnIt)
	{
		string newColName = changeNameColumnIt->first;
		Column *changedCol = changeNameColumnIt->second;
		missingColumns.erase(changedCol->name());
		changedCol->setName(newColName);
	}

	int colNo = _packageData->dataSet->columnCount();
	if (changedColumns.size() > 0)
	{
		if (rowCountChanged)
			dataSet = setDataSetSize(colNo, syncDataSet->rowCount());

		for (vector<pair<string, int> >::iterator it = changedColumns.begin(); it != changedColumns.end(); ++it)
			initColumn(it->second, syncDataSet->getColumn(it->first));
	}

	if (newColumns.size() > 0)
	{
		dataSet = setDataSetSize(colNo + newColumns.size(), syncDataSet->rowCount());
		for (vector<pair<string, int> >::iterator it = newColumns.begin(); it != newColumns.end(); ++it, ++colNo)
		{
			initColumn(colNo, syncDataSet->getColumn(it->first));
		}
	}

	if (missingColumns.size() > 0)
	{
		for (map<string, Column *>::iterator misColIt = missingColumns.begin(); misColIt != missingColumns.end(); ++misColIt)
		{
			_packageData->dataSet->removeColumn(misColIt->first);
		}
	}

}
