#include "importer.h"
#include "utilities/qutils.h"
#include "utilities/settings.h"
#include "log.h"
#include <QVariant>

Importer::~Importer() {}

void Importer::loadDataSet(const std::string &locator, boost::function<void(const std::string &, int)> progressCallback)
{
	DataSetPackage::pkg()->beginLoadingData();

	ImportDataSet *importDataSet = loadFile(locator, progressCallback);

	int columnCount = importDataSet->columnCount();
	DataSetPackage::pkg()->createDataSet(); // this is required in case the loading of the data fails so that the created dataset can later be freed

	if (columnCount > 0)
	{
		int rowCount = importDataSet->rowCount();

		DataSetPackage::pkg()->setDataSetSize(columnCount, rowCount);

		int colNo = 0;
		for (ImportColumn *importColumn : *importDataSet)
		{
			progressCallback(fq(tr("Loading Data Set")), 50 + 50 * colNo / columnCount);
			initColumn(colNo, importColumn);
			colNo++;
		}
	}

	delete importDataSet;
	DataSetPackage::pkg()->endLoadingData();
}

void Importer::initColumn(QVariant colId, ImportColumn *importColumn)
{
	initColumnWithStrings(colId, importColumn->name(),  importColumn->allValuesAsStrings());
}

void Importer::initColumnWithStrings(QVariant colId, std::string newName, const std::vector<std::string> &values)
{
	// interpret the column as a datatype
	std::set<int>				uniqueValues;
	std::vector<int>			intValues;
	std::vector<double>			doubleValues;
	std::map<int, std::string>	emptyValuesMap;

	//If less unique integers than the thresholdScale then we think it must be ordinal: https://github.com/jasp-stats/INTERNAL-jasp/issues/270
	bool	useCustomThreshold	= Settings::value(Settings::USE_CUSTOM_THRESHOLD_SCALE).toBool();
	size_t	thresholdScale		= (useCustomThreshold ? Settings::value(Settings::THRESHOLD_SCALE) : Settings::defaultValue(Settings::THRESHOLD_SCALE)).toUInt();

	bool valuesAreIntegers		= ImportColumn::convertVecToInt(values, intValues, uniqueValues, emptyValuesMap);

	auto isNominalInt			= [&](){ return valuesAreIntegers && uniqueValues.size() == 2; };
	auto isOrdinal				= [&](){ return valuesAreIntegers && uniqueValues.size() > 2 && uniqueValues.size() <= thresholdScale; };
	auto isScalar				= [&](){ return ImportColumn::convertVecToDouble(values, doubleValues, emptyValuesMap); };

	if		(isOrdinal())					initColumnAsNominalOrOrdinal(	colId,	newName,	intValues,		true	);
	else if	(isNominalInt())				initColumnAsNominalOrOrdinal(	colId,	newName,	intValues,		false	);
	else if	(isScalar())					initColumnAsScale(				colId,	newName,	doubleValues	);
	else				emptyValuesMap =	initColumnAsNominalText(		colId,	newName,	values			);

	storeInEmptyValues(newName, emptyValuesMap);
}

void Importer::syncDataSet(const std::string &locator, boost::function<void(const std::string &, int)> progress)
{
	ImportDataSet *importDataSet	= loadFile(locator, progress);
	bool rowCountChanged			= importDataSet->rowCount() != DataSetPackage::pkg()->dataRowCount();

	std::vector<std::pair<std::string, int> >	newColumns;
	std::vector<std::pair<int, std::string> >	changedColumns; //import col index and original column name
	std::map<std::string, std::string>			changeNameColumns; //origname -> newname
	std::vector<std::string>					orgColumnNames(DataSetPackage::pkg()->getColumnNames(false)); //Non-computed
	std::set<std::string>						missingColumns(orgColumnNames.begin(), orgColumnNames.end());

	int syncColNo		= 0;

	//If the following gives errors trhen it probably should be somewhere else:
	for (const std::string & colName : orgColumnNames)
		if (DataSetPackage::pkg()->isColumnComputed(colName)) // make sure "missing" columns aren't actually computed columns
			missingColumns.erase(colName);

	for (ImportColumn *syncColumn : *importDataSet)
	{
		std::string syncColumnName = syncColumn->name();

		if (missingColumns.count(syncColumnName) == 0)
			newColumns.push_back(std::pair<std::string, int>(syncColumnName, syncColNo));
		else
		{
			missingColumns.erase(syncColumnName);

			if(DataSetPackage::pkg()->isColumnDifferentFromStringValues(syncColumnName, syncColumn->allValuesAsStrings()))
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
				const std::string & newColName	= newColIt->first;
				ImportColumn *newValues = importDataSet->getColumn(newColName);

				if(!DataSetPackage::pkg()->isColumnDifferentFromStringValues(nameMissing, newValues->allValuesAsStrings()))
				{
					changeNameColumns[nameMissing] = newColName;
					newColumns.erase(newColIt);
					break;
				}
			}

	for (auto changeNameColumnIt : changeNameColumns)
		missingColumns.erase(changeNameColumnIt.first);

	if (newColumns.size() > 0 || changedColumns.size() > 0 || missingColumns.size() > 0 || changeNameColumns.size() > 0 || rowCountChanged)
			_syncPackage(importDataSet, newColumns, changedColumns, missingColumns, changeNameColumns, rowCountChanged);

	delete importDataSet;
}

void Importer::_syncPackage(
		ImportDataSet								*	syncDataSet,
		std::vector<std::pair<std::string, int>>	&	newColumns,
		std::vector<std::pair<int, std::string>>	&	changedColumns, // import col index and original (old) col name
		std::set<std::string>						&	missingColumns,
		std::map<std::string, std::string>			&	changeNameColumns, //origname -> newname
		bool											rowCountChanged)

{
	if(!DataSetPackage::pkg()->checkDoSync())
		return;

	DataSetPackage::pkg()->beginSynchingData();

	std::vector<std::string>			_changedColumns;
	std::vector<std::string>			_missingColumns;
	std::map<std::string, std::string>	_changeNameColumns;

	for (auto changeNameColumnIt : changeNameColumns)
	{
		std::string oldColName = changeNameColumnIt.first,
					newColName = changeNameColumnIt.second;

		Log::log() << "Column name changed, from: " << oldColName << " to " << newColName << std::endl;


		_changeNameColumns[oldColName] = newColName;
		DataSetPackage::pkg()->renameColumn(oldColName, newColName);
	}

	int colNo = DataSetPackage::pkg()->columnCount();
	DataSetPackage::pkg()->setDataSetRowCount(syncDataSet->rowCount());

	for (auto indexColChanged : changedColumns)
	{
		Log::log() << "Column changed " << indexColChanged.first << std::endl;

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


	DataSetPackage::pkg()->endSynchingData(_changedColumns, _missingColumns, _changeNameColumns, rowCountChanged, newColumns.size() > 0);
}
