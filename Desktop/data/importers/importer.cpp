#include "importer.h"
#include "utilities/qutils.h"
#include "log.h"
#include <QVariant>
#include "../datasetpackage.h"
#include "timers.h"
#include <QThreadPool>

Importer::Importer() 
{
	//Turns out that jasp importer and jaspiporter old are not Importers... Great...
	DataSetPackage::pkg()->setIsJaspFile(false);
}

Importer::~Importer() {}

class InitColumnTask : public QRunnable
{
public:
	InitColumnTask(ImportColumn * importColumn, Column * datasetColumn)
		: _importColumn(importColumn), _column(datasetColumn)
	{}
	
	void run() override
	{
		_column->initFromStrings(
					_importColumn->name(),
					_importColumn->allValuesAsStrings(),
					_importColumn->allLabelsAsStrings(),
					_importColumn->title(),
					_importColumn->getColumnType(),
					_importColumn->allEmptyValuesAsStrings(),
					DataSetPackage::thresholdScale(),
					DataSetPackage::orderByValueByDefault());
		
		_importColumn->finish();
	}
		
private:
	ImportColumn	*	_importColumn;
	Column			*	_column;
};

void Importer::importColumnFinished(ImportColumn * column)
{
	
	_serialFinishing.lock();
	
	try
	{
		_waitingFor.erase(column);
		_progressCallback(50 + 25 * (_importDataSet->columnCount() - _waitingFor.size()) / _importDataSet->columnCount());
		delete column;
		
	}
	catch(std::exception e)
	{
		Log::log() << "A problem occured during threaded loading: " << e.what() << std::endl;
		_serialFinishing.unlock();
		
		throw e;
	}
	
	_serialFinishing.unlock();

}

void Importer::loadDataSet(const std::string &locator, std::function<void(int)> progressCallback)
{
	long timeBeginS = Utils::currentSeconds();
	_progressCallback=progressCallback;
	
	DataSetPackage::pkg()->beginLoadingData();
	
	_synching = false;

	JASPTIMER_RESUME(Importer::loadDataSet loadFile);
	_importDataSet = loadFile(locator, progressCallback);
	JASPTIMER_STOP(Importer::loadDataSet loadFile);
	
	JASPTIMER_RESUME(Importer::loadDataSet createDataSetAndLoad);
	int columnCount = _importDataSet->columnCount();

	if (columnCount > 0)
	{
		int	rowCount		= _importDataSet->rowCount();
			_waitingFor		= std::set<ImportColumn*>(_importDataSet->begin(), _importDataSet->end());
		

		DataSetPackage::pkg()->dataSet()->beginBatchedToDB();
		DataSetPackage::pkg()->dataSet()->setDescription(_importDataSet->description());
		DataSetPackage::pkg()->setDataSetSize(columnCount, rowCount);
		
		for(int colNo=0; colNo<columnCount; colNo++)
		{
			ImportColumn	* importColumn	= _importDataSet->getColumn(colNo);
			Column			* dataSetColumn	= DataSetPackage::pkg()->dataSet()->column(colNo);
			InitColumnTask	* task			= new InitColumnTask(importColumn, dataSetColumn);
			
			connect(importColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
			
			QThreadPool::globalInstance()->start(task);
		}
		
		bool keepWaiting = true;
		while(keepWaiting)
		{
			QThread::sleep(1);	
			_serialFinishing.lock();
			keepWaiting = _waitingFor.size() > 0;
			_serialFinishing.unlock();
		}
		
		DataSetPackage::pkg()->dataSet()->endBatchedToDB([&](float f){ progressCallback(75 + f * 25); });
	}
	JASPTIMER_STOP(Importer::loadDataSet createDataSetAndLoad);
	
	_importDataSet->clearColumns();
	delete _importDataSet;
	DataSetPackage::pkg()->endLoadingData();
	
	long totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Loading '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}

void Importer::syncDataSet(const std::string &locator, std::function<void(int)> progress)
{
	_synching = true;
	long timeBeginS = Utils::currentSeconds();
	
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
	
	long totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Synching '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}

void Importer::initColumn(QVariant colIndex, ImportColumn *importColumn)
{
       JASPTIMER_SCOPE(Importer::initColumn);
	   
	   Column * column = colIndex.typeId() == QMetaType::Int 
				 ? DataSetPackage::pkg()->dataSet()->column(colIndex.toInt())
				 : DataSetPackage::pkg()->dataSet()->column(fq(colIndex.toString()));
	   
	   column->initFromStrings(
				   importColumn->name(),
				   importColumn->allValuesAsStrings(),
				   importColumn->allLabelsAsStrings(),
				   importColumn->title(),
				   importColumn->getColumnType(),
				   importColumn->allEmptyValuesAsStrings(),
				   DataSetPackage::thresholdScale(),
				   DataSetPackage::orderByValueByDefault());
	   
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
