//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
#include "importer.h"
#include "qutils.h"
#include "log.h"
#include <QVariant>
#include "../datasetpackage.h"
#include "timers.h"
#include <QThreadPool>
#include <queue>
#include "data/asyncloader.h"
#include "gui/preferencesmodel.h"

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

void Importer::loadDataSet(const std::string &locator, DataSet * dataSet, std::function<void(int)> progressCallback)
{
	int64_t timeBeginS = Utils::currentSeconds();
	_progressCallback=progressCallback;
	

	_synching = false;

	JASPTIMER_RESUME(Importer::loadDataSet loadFile);
	_importDataSet = loadFile(locator, progressCallback);
	JASPTIMER_STOP(Importer::loadDataSet loadFile);

	if (!_importDataSet)
		throw LoaderException("No data loaded", true);
	
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
		

		dataSet->beginBatchedToDB();
		dataSet->setDescription(_importDataSet->description());
		dataSet->setColumnCount(columnCount);
		dataSet->setRowCount(rowCount);
		
		for(int colNo=0; colNo<columnCount; colNo++)
		{
			ImportColumn	* importColumn	= _importDataSet->getColumn(colNo);
			Column			* dataSetColumn	= dataSet->column(colNo);
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
		
		dataSet->endBatchedToDB([&](float f){ progressCallback(75 + f * 25); });
	}
	JASPTIMER_STOP(Importer::loadDataSet createDataSetAndLoad);
	
	_importDataSet->clearColumns();
	delete _importDataSet;
	
	
	int64_t totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "Loading '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
}

void Importer::syncDataSet(const std::string &locator, DataSet * dataSet, std::function<void(int)> progress)
{
	Log::log() << "[Importer::syncDataSet] START: locator=" << locator << ", dataSetId=" << (dataSet ? dataSet->id() : -1) << std::endl;

					_synching			= true;
					_progressCallback	= progress;
	int64_t			timeBeginS		= Utils::currentSeconds();
					_importDataSet	= loadFile(locator, progress);
	Log::log() << "[Importer::syncDataSet] loadFile returned, _importDataSet=" << _importDataSet << std::endl;
	bool			rowCountChanged	= _importDataSet->rowCount() != dataSet->rowCount();
	Log::log() << "[Importer::syncDataSet] rowCountChanged=" << rowCountChanged << std::endl;
	ColumnSet		oldColumns;
	
	for(Column * c : dataSet->columns())
		if(!c->isComputed())
			oldColumns.insert(c);
	
	Log::log() << "[Importer::syncDataSet] Calling checkDoSync" << std::endl;
	if(! emit DataSetPackage::pkg()->checkDoSync())
	{
		Log::log() << "[Importer::syncDataSet] checkDoSync returned false, aborting" << std::endl;
		delete _importDataSet;	//was loaded just above; free it so this aborted sync doesn't leak it
		_importDataSet	= nullptr;
		_synching		= false;
		return;
	}
	Log::log() << "[Importer::syncDataSet] checkDoSync returned true, continuing" << std::endl;
	
	stringvec       changedColumns,
					newOrder,
					missingColumns,
					newColumnOrder;
	strstrmap		changeNameColumns; //origname -> newname
		
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
	
	dataSet->beginBatchedToDB();

	
	dataSet->setRowCount(_importDataSet->rowCount());
	
	_waitingFor.clear();
	InitColumnTasks tasks;
	ImportColumns newColumns;
	
	//First we simply make sure that every column in the import dataset that has the same name as one in the data is linked to it.
	for (ImportColumn * importColumn : _importDataSet->columns())
	{
		newColumnOrder.push_back(importColumn->name());
		
		Column			* dataSetColumn	= dataSet->column(importColumn->name());
		
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
	for(Column * col : dataSet->columns())
		if(oldColumns.count(col))
			oldColQ.push(col);
	
	//prefs() is absent when the importer runs without the GUI (unit tests, headless use);
	bool keepMissingColsWhenSyncing = PreferencesModel::prefs() && PreferencesModel::prefs()->keepMissingColsWhenSyncing();

	for(ImportColumn * newColumn : newColumns)
	{
		Log::log() << "New column " << newColumn->name() << std::endl;

		Column			* dataSetColumn	= (!keepMissingColsWhenSyncing && oldColQ.size() > 0) ? oldColQ.front() : dataSet->createColumn(newColumn->name());
		InitColumnTask	* task			= new InitColumnTask(newColumn, dataSetColumn, totalCellsCallback);

		connect(newColumn, &ImportColumn::finished, this, &Importer::importColumnFinished, Qt::DirectConnection);

		if(!keepMissingColsWhenSyncing && oldColQ.size() > 0 && oldColQ.front() == dataSetColumn)
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
	
	dataSet->endBatchedToDB([&](float f){ progress(75 + f * 25); });

	for (Column * oldCol : oldColumns) //already checked for not being computed column at creation list
	{
		if (keepMissingColsWhenSyncing)
		{
			//getColumnIndex() gives the column's position in the full dataset, which can exceed
			//newColumnOrder's size (it holds the imported columns plus any missing ones already re-inserted
			//here). Clamp to [0, size] so the insert iterator stays valid; out of range means append.
			int i = dataSet->getColumnIndex(oldCol->name());
			if (i < 0 || i > int(newColumnOrder.size()))
				i = int(newColumnOrder.size());
			stringvec emptyvalues(dataSet->rowCount());
			oldCol->overwriteDataAndType(emptyvalues, oldCol->type(), false);
			newColumnOrder.insert(newColumnOrder.begin() + i, oldCol->name());
		}
		else
		{
			Log::log() << "Column deleted " << oldCol->name() << std::endl;

			missingColumns.push_back(oldCol->name());
			dataSet->removeColumn(oldCol->name());
		}
	}
	
	emit dataSet->datasetChanged(dataSet->id(), tq(changedColumns), tq(missingColumns), tq(changeNameColumns), rowCountChanged, newColumns.size() > 0);
	
	if(newColumnOrder.size() > 0)
		dataSet->columnsReorder(newColumnOrder);
	
	DataSetPackage::pkg()->setManualEdits(false);
	delete _importDataSet;
	
	int64_t totalS = (Utils::currentSeconds() - timeBeginS);
	Log::log() << "[Importer::syncDataSet] Synching '" << locator << "' took " << totalS << "s or " << (totalS / 60) << "m" << std::endl;
	Log::log() << "[Importer::syncDataSet] END" << std::endl;
}
