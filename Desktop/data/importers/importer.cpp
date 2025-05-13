#include "importer.h"
#include "utilities/qutils.h"
#include "log.h"
#include <QVariant>
#include "../datasetpackage.h"
#include "timers.h"
#include <QThreadPool>

Importer::Importer() 
{
	//Turns out that jasp importer and jaspimporter old are not Importers... Great...
	DataSetPackage::pkg()->setIsJaspFile(false);
}

Importer::~Importer() {}

class InitColumnTask : public QRunnable
{
public:
	InitColumnTask(ImportColumn * importColumn, Column * datasetColumn, std::function<void(int)> progressCells = [](int){})
		: _importColumn(importColumn), _column(datasetColumn), _progressCells(progressCells)
	{
		setAutoDelete(true);
	}
	
	void run() override
	{
		int prevRowSent = 0,
			stepSize	= std::max(int(_importColumn->size())/100, 100);
		
		auto handleRow = [&](int curRow)
		{
			if(_progressCells && curRow - prevRowSent > stepSize || curRow >= _importColumn->size() - 1)
			{
				_progressCells(curRow - prevRowSent);
				prevRowSent = curRow;
			}
		};
		
		_column->initFromLookups(
					_importColumn->name(),
					_importColumn->size(),
					[this, &handleRow](size_t row) { handleRow(row); return _importColumn->valueLookup(row); },
					[this, &handleRow](size_t row) { handleRow(row); return _importColumn->labelLookup(row); },
					_importColumn->title(),
					_importColumn->getColumnType(),
					_importColumn->allEmptyValuesAsStrings(),
					DataSetPackage::thresholdScale(),
					DataSetPackage::orderByValueByDefault(),
					true); //Leave batched unfinished by neglecting to call endBatchedLabelsDB() for now, this we can just do all at the end in the dataset for all columns that are still in label batch mode
		
		_importColumn->finish(!_progressCells);
	}
		
private:
	ImportColumn			*	_importColumn	= nullptr;
	Column					*	_column			= nullptr;
	std::function<void(int)>	_progressCells	= nullptr;
};

void Importer::importColumnFinished(ImportColumn * column, bool doCallback)
{
	_serialFinishing.lock();
	
	try
	{
		_waitingFor.erase(column);
		if(doCallback)
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
	int64_t timeBeginS = Utils::currentSeconds();
	_progressCallback=progressCallback;
	
	DataSetPackage::pkg()->beginLoadingData();
	DataSetPackage::pkg()->createDataSet();
	
	_synching = false;

	JASPTIMER_RESUME(Importer::loadDataSet loadFile);
	_importDataSet = loadFile(locator, progressCallback);
	JASPTIMER_STOP(Importer::loadDataSet loadFile);
	
	JASPTIMER_RESUME(Importer::loadDataSet createDataSetAndLoad);
	int columnCount = _importDataSet->columnCount();

	if (columnCount > 0)
	{
		int		rowCount		= _importDataSet->rowCount(),
				totalCells		= rowCount * _importDataSet->columnCount(),
				processed		= 0,
				stepSize		= std::max(totalCells/100, 100);
				_waitingFor		= std::set<ImportColumn*>(_importDataSet->begin(), _importDataSet->end());
		QMutex	callMutex;
		
		auto totalCellsCallback = [&,this](int cells)
		{
			
			if((processed % stepSize) + cells > stepSize || processed + cells >= totalCells)
			{
				callMutex.lock();
				_progressCallback(50 + 25 * float(processed + cells) / float(totalCells));
				callMutex.unlock();
			}
			
			processed += cells;
		};
		

		DataSetPackage::pkg()->dataSet()->beginBatchedToDB();
		DataSetPackage::pkg()->dataSet()->setDescription(_importDataSet->description());
		DataSetPackage::pkg()->setDataSetSize(columnCount, rowCount);
		
		for(int colNo=0; colNo<columnCount; colNo++)
		{
			ImportColumn	* importColumn	= _importDataSet->getColumn(colNo);
			Column			* dataSetColumn	= DataSetPackage::pkg()->dataSet()->column(colNo);
			InitColumnTask	* task			= new InitColumnTask(importColumn, dataSetColumn, totalCellsCallback);
			
			connect(importColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
			
			QThreadPool::globalInstance()->start(task);
		}
		
		bool keepWaiting = true;
		while(keepWaiting)
		{
			QThread::sleep(std::chrono::nanoseconds(10000));
			_serialFinishing.lock();
			keepWaiting = _waitingFor.size() > 0;
			_serialFinishing.unlock();
		}
		
		DataSetPackage::pkg()->dataSet()->endBatchedToDB([&](float f){ progressCallback(75 + f * 25); });
	}
	JASPTIMER_STOP(Importer::loadDataSet createDataSetAndLoad);
	
	DataSetPackage::pkg()->endLoadingData();
	
	_importDataSet->clearColumns();
	delete _importDataSet;
	
	
	int64_t totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Loading '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}

void Importer::syncDataSet(const std::string &locator, std::function<void(int)> progress)
{
					_synching			= true;
					_progressCallback	= progress;
	int64_t			timeBeginS		= Utils::currentSeconds();
					_importDataSet	= loadFile(locator, progress);
	bool			rowCountChanged	= _importDataSet->rowCount() != DataSetPackage::pkg()->dataRowCount();
	int				syncColNo		= 0;
	size_t			newColCount		= 0;

	std::vector<std::pair<std::string, int> >	newColumns;
	std::vector<std::pair<int, std::string> >	changedColumns; //import col index and original column name
	strstrmap									changeNameColumns; //origname -> newname
	stringvec									orgColumnNames(DataSetPackage::pkg()->getColumnNames()),
												newOrder;
	stringset									missingColumns(orgColumnNames.begin(), orgColumnNames.end());

	//If the following gives errors trhen it probably should be somewhere else:
	for (const std::string & colName : orgColumnNames)
		if (DataSetPackage::pkg()->isColumnComputed(colName)) // make sure "missing" columns aren't actually computed columns
		{
			missingColumns.erase(colName);
			newColCount++; //Count the computed columns first
		}

	for (ImportColumn *syncColumn : *_importDataSet)
	{
		std::string syncColumnName = syncColumn->name();
		
		newOrder.push_back(syncColumnName);

		if (missingColumns.count(syncColumnName) == 0)
			newColumns.push_back(std::pair<std::string, int>(syncColumnName, syncColNo));
		else
		{
			missingColumns.erase(syncColumnName);

			if(DataSetPackage::pkg()->isColumnDifferentFromStringLookUps(syncColumnName, syncColumn->title(), syncColumn->size() ,[&syncColumn](size_t r){ return syncColumn->valueLookup(r); }, [&syncColumn](size_t r){ return syncColumn->labelLookup(r); }, syncColumn->allEmptyValuesAsStrings()))
			{
				//Log::log() << "Something changed in column: " << syncColumnName << std::endl;
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
				ImportColumn		* newColumn		= _importDataSet->getColumn(newColName);

				if(!DataSetPackage::pkg()->isColumnDifferentFromStringLookUps(nameMissing, newColumn->title(), newColumn->size(), [&newColumn](size_t r){ return newColumn->valueLookup(r); }, [&newColumn](size_t r){ return newColumn->labelLookup(r); }, newColumn->allEmptyValuesAsStrings()))
				{
					changeNameColumns[nameMissing] = newColName;
					newColumns.erase(newColIt);
					break;
				}
			}

	for (auto & changeNameColumnIt : changeNameColumns)
		missingColumns.erase(changeNameColumnIt.first);
	
	newColCount += newColumns.size() + changedColumns.size() + changeNameColumns.size();

	if (newColumns.size() > 0 || changedColumns.size() > 0 || missingColumns.size() > 0 || changeNameColumns.size() > 0 || orgColumnNames != newOrder || rowCountChanged)
			_syncPackage(newColumns, changedColumns, missingColumns, changeNameColumns, newOrder, rowCountChanged, newColCount, progress);

	DataSetPackage::pkg()->setManualEdits(false);
	delete _importDataSet;
	
	int64_t totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Synching '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}

void Importer::_syncPackage(
		const std::vector<std::pair<std::string, int>>	&	newColumns,
		const std::vector<std::pair<int, std::string>>	&	changedColumns, // import col index and original (old) col name
		const stringset									&	missingColumns,
		const strstrmap									&	changeNameColumns, //origname -> newname
		const stringvec									&	newColumnOrder,
		bool											rowCountChanged,
		size_t											newColCount,
		std::function<void(int)> progress)

{
	if(! emit DataSetPackage::pkg()->checkDoSync())
		return;

	DataSetPackage::pkg()->beginSynchingData();
	
	int		rowCount		= _importDataSet->rowCount(),
			totalCells		= rowCount * _importDataSet->columnCount(),
			processed		= 0,
			stepSize		= std::max(totalCells/100, 100);
			_waitingFor		= std::set<ImportColumn*>(_importDataSet->begin(), _importDataSet->end());
	QMutex	callMutex;
	
	auto totalCellsCallback = [&,this](int cells)
	{
		
		if((processed % stepSize) + cells > stepSize || processed + cells >= totalCells)
		{
			callMutex.lock();
			_progressCallback(50 + 25 * float(processed + cells) / float(totalCells));
			callMutex.unlock();
		}
		
		processed += cells;
	};
	
	DataSetPackage::pkg()->dataSet()->beginBatchedToDB();

	stringvec		_changedColumns,
					_missingColumns;

	for (const auto & changeNameColumnIt : changeNameColumns)
	{
		const std::string	& oldColName = changeNameColumnIt.first,
							& newColName = changeNameColumnIt.second;

		Log::log() << "Column name changed, from: " << oldColName << " to " << newColName << std::endl;

		DataSetPackage::pkg()->renameColumn(oldColName, newColName);
	}

	
	DataSetPackage::pkg()->setDataSetSize(newColCount, _importDataSet->rowCount());
	
	std::set<Column*> unusedColumns(DataSetPackage::pkg()->dataSet()->columns().begin(), DataSetPackage::pkg()->dataSet()->columns().end());
	//remove computed columns from unused list
	size_t compCols=0;
	for(Column * c : unusedColumns)
		if(c->isComputed())
			compCols++;
	
	for(size_t comp = 0; comp < compCols; comp++)
		for(Column * c : unusedColumns)
			if(c->isComputed())
			{
				unusedColumns.erase(c);
				break;
			}
	
	
	_waitingFor.clear();
	std::vector<InitColumnTask*> tasks;
	
	for (const auto & indexColChanged : changedColumns)
	{
		//Log::log() << "Column changed " << indexColChanged.second << std::endl;

		std::string colName	= indexColChanged.second;
		_changedColumns.push_back(colName);
		
		ImportColumn	* importColumn	= _importDataSet->getColumn(indexColChanged.first);
		Column			* dataSetColumn	= DataSetPackage::pkg()->dataSet()->column(colName);
		InitColumnTask	* task			= new InitColumnTask(importColumn, dataSetColumn, totalCellsCallback);
		
		connect(importColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
		
		tasks.push_back(task);
		_waitingFor.insert(importColumn);
		unusedColumns.erase(dataSetColumn);
	}
	
	if (newColumns.size() > 0)
	{
		for (auto it = newColumns.begin(); it != newColumns.end(); ++it)
		{
			Log::log() << "New column " << it->first << std::endl;
			
			ImportColumn	* importColumn	= _importDataSet->getColumn(it->first);
			Column			* dataSetColumn	= *unusedColumns.begin();
			InitColumnTask	* task			= new InitColumnTask(importColumn, dataSetColumn, totalCellsCallback);
			
			connect(importColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
			
			tasks.push_back(task);
			_waitingFor.insert(importColumn);
			unusedColumns.erase(dataSetColumn);
		}
	}
	
	if(unusedColumns.size() > 0)
		throw std::runtime_error("Somehow columns exist that are not being used?");
	
	for(InitColumnTask * task : tasks)
		QThreadPool::globalInstance()->start(task);
	
	bool keepWaiting = true;
	while(keepWaiting)
	{
		QThread::sleep(std::chrono::nanoseconds(10000));
		_serialFinishing.lock();
		keepWaiting = _waitingFor.size() > 0;
		_serialFinishing.unlock();
	}
	
	tasks.clear();
	
	DataSetPackage::pkg()->dataSet()->endBatchedToDB([&](float f){ progress(75 + f * 25); });

	if (missingColumns.size() > 0)
		for (const std::string & columnName : missingColumns) //already checked for not being computed column at creation list
			{
				Log::log() << "Column deleted " << columnName << std::endl;

				_missingColumns.push_back(columnName);
				DataSetPackage::pkg()->removeColumn(columnName);
			}
	
	DataSetPackage::pkg()->endSynchingData(_changedColumns, _missingColumns, changeNameColumns, rowCountChanged, newColumns.size() > 0);
	
	if(newColumnOrder.size() > 0)
		DataSetPackage::pkg()->columnsReorder(newColumnOrder);
}
