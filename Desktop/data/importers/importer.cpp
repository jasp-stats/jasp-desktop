#include "importer.h"
#include "utilities/qutils.h"
#include "log.h"
#include <QVariant>
#include "../datasetpackage.h"
#include "timers.h"
#include <QThreadPool>
#include <queue>

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
		assert(_column);
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

        _column->db().preloadInterfaceForThread();
		
		_column->initFromLookups(
					_importColumn->name(),
					_importColumn->size(),
					[this, &handleRow](size_t row) { handleRow(row); return _importColumn->valueLookup(row); },
					[this, &handleRow](size_t row) { handleRow(row); return _importColumn->labelLookup(row); },
					_importColumn->title(),
					_importColumn->getColumnType(),
					_importColumn->allEmptyValuesAsStrings(),
					DataSetPackage::thresholdScale(),
					DataSetPackage::orderByValueByDefault());
		
		_importColumn->finish(!_progressCells);
	}
		
private:
	ImportColumn			*	_importColumn	= nullptr;
	Column					*	_column			= nullptr;
	std::function<void(int)>	_progressCells	= nullptr;
};

typedef std::vector<InitColumnTask*> InitColumnTasks;

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
	ColumnSet		oldColumns;
	
	for(Column * c : DataSetPackage::pkg()->dataSet()->columns())
		if(!c->isComputed())
			oldColumns.insert(c);
	
	if(! emit DataSetPackage::pkg()->checkDoSync())
		return;
	
	stringvec       changedColumns,
					newOrder,
					missingColumns,
					newColumnOrder;
	strstrmap		changeNameColumns; //origname -> newname
	
	

	DataSetPackage::pkg()->beginSynchingData();
		
	int		rowCount		= _importDataSet->rowCount(),
			totalCells		= rowCount * _importDataSet->columnCount(),
			processed		= 0,
			stepSize		= std::max(totalCells/100, 100);
			_waitingFor		= {};
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

	
	DataSetPackage::pkg()->dataSet()->setRowCount(_importDataSet->rowCount());
	
	_waitingFor.clear();
	InitColumnTasks tasks;
	ImportColumns newColumns;
	
	//First we simply make sure that every column in the import dataset that has the same name as one in the data is linked to it.
	for (ImportColumn * importColumn : _importDataSet->columns())
	{
		newColumnOrder.push_back(importColumn->name());
		
		Column			* dataSetColumn	= DataSetPackage::pkg()->dataSet()->column(importColumn->name());
		
		if(dataSetColumn)
		{
			oldColumns	.erase(dataSetColumn);
			
			//Is there a difference?
			if(dataSetColumn->isColumnDifferentFromStringLookUps(
				importColumn->title(),
				importColumn->size(),
				[&importColumn](size_t r){ return importColumn->valueLookup(r); }, 
				[&importColumn](size_t r){ return importColumn->labelLookup(r); }, 
				importColumn->allEmptyValuesAsStrings()
				))
			{				
				InitColumnTask	* task			= new InitColumnTask(importColumn, dataSetColumn, totalCellsCallback);
				
				connect(importColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
				
				tasks		.push_back(task);
				_waitingFor	.insert(importColumn);
				
				changedColumns.push_back(dataSetColumn->name());
			}
			else if(rowCountChanged)
				changedColumns.push_back(dataSetColumn->name());
		}
		else
			newColumns	.push_back(importColumn);
	}
	
	//lets make sure to replace any changed columns by going through the columns in a predictable order:
	std::queue<Column*> oldColQ;
	for(Column * col : DataSetPackage::pkg()->dataSet()->columns())
		if(oldColumns.count(col))
			oldColQ.push(col);
	
	for(ImportColumn * newColumn : newColumns)
	{
		Log::log() << "New column " << newColumn->name() << std::endl;
		Column			* dataSetColumn	= oldColQ.size() > 0 ? oldColQ.front() : DataSetPackage::pkg()->dataSet()->newColumn(newColumn->name());
		InitColumnTask	* task			= new InitColumnTask(newColumn, dataSetColumn, totalCellsCallback);
		
		connect(newColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);
		
		if(oldColQ.size() > 0 && oldColQ.front() == dataSetColumn)
		{
			changeNameColumns[dataSetColumn->name()] = newColumn->name();
			oldColQ.pop();
		}
		
		tasks		.push_back(task);
		_waitingFor	.insert(newColumn);
		oldColumns	.erase(dataSetColumn); //cause were replacing it with whatever the new column is
	}
	
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

	for (Column * oldCol : oldColumns) //already checked for not being computed column at creation list
	{
		Log::log() << "Column deleted " << oldCol->name() << std::endl;

		missingColumns.push_back(oldCol->name());
		DataSetPackage::pkg()->dataSet()->removeColumn(oldCol->name());
	}
	
	DataSetPackage::pkg()->endSynchingData(changedColumns, missingColumns, changeNameColumns, rowCountChanged, newColumns.size() > 0);
	
	if(newColumnOrder.size() > 0)
		DataSetPackage::pkg()->columnsReorder(newColumnOrder);
	
	DataSetPackage::pkg()->setManualEdits(false);
	delete _importDataSet;
	
	int64_t totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Synching '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}
