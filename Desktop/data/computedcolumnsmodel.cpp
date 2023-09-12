#include "computedcolumnsmodel.h"
#include "utilities/jsonutilities.h"
#include "utilities/qutils.h"
#include "columnencoder.h"
#include "log.h"

ComputedColumnsModel * ComputedColumnsModel::_singleton = nullptr;

ComputedColumnsModel::ComputedColumnsModel()
	: QObject(DataSetPackage::pkg())
{
	assert(_singleton == nullptr);
	_singleton = this;

	_undoStack = DataSetPackage::pkg()->undoStack();

	connect(DataSetPackage::pkg(),	&DataSetPackage::dataSetChanged,				this,					&ComputedColumnsModel::onDataSetChanged						);

	connect(this,					&ComputedColumnsModel::refreshProperties,		this,					&ComputedColumnsModel::computeColumnJsonChanged				);
	connect(this,					&ComputedColumnsModel::refreshProperties,		this,					&ComputedColumnsModel::computeColumnRCodeChanged			);
	connect(this,					&ComputedColumnsModel::refreshProperties,		this,					&ComputedColumnsModel::computeColumnErrorChanged			);
	connect(this,					&ComputedColumnsModel::refreshProperties,		this,					&ComputedColumnsModel::computeColumnUsesRCodeChanged		);
	connect(this,					&ComputedColumnsModel::refreshProperties,		this,					&ComputedColumnsModel::computeColumnNameSelectedChanged		);
	connect(this,					&ComputedColumnsModel::refreshColumn,			DataSetPackage::pkg(),	&DataSetPackage::refreshColumn,								Qt::QueuedConnection);
	connect(this,					&ComputedColumnsModel::refreshData,				DataSetPackage::pkg(),	&DataSetPackage::refresh,									Qt::QueuedConnection);
	//connect(this,					&ComputedColumnsModel::headerDataChanged,		DataSetPackage::pkg(),	&DataSetPackage::headerDataChanged,							Qt::QueuedConnection);

	connect(Analyses::analyses(),	&Analyses::requestComputedColumnCreation,		DataSetPackage::pkg(),	&DataSetPackage::requestComputedColumnCreation,				Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestColumnCreation,				DataSetPackage::pkg(),	&DataSetPackage::requestColumnCreation,						Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestComputedColumnDestruction,	DataSetPackage::pkg(),	&DataSetPackage::requestComputedColumnDestruction,			Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::analysisRemoved,						this,					&ComputedColumnsModel::analysisRemoved						);
}

QString ComputedColumnsModel::computeColumnRCode()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->rCode());
}

QString ComputedColumnsModel::computeColumnRCodeCommentStripped()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->rCodeStripped());
}

bool ComputedColumnsModel::computeColumnUsesRCode()
{
	return _selectedColumn && _selectedColumn->codeType() == computedColumnType::rCode;
}

QString ComputedColumnsModel::computeColumnJson()
{
	QString json = !_selectedColumn ? "" : tq(_selectedColumn->constructorJsonStr());

	return json;
}

QString ComputedColumnsModel::computeColumnError()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->error());
}

QString ComputedColumnsModel::computeColumnNameSelected()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->name());;
}

void ComputedColumnsModel::setComputeColumnRCode(const QString & newCode)
{
	if(!_selectedColumn)
		return;

	if(_selectedColumn->setRCode(fq(newCode)))
		emit computeColumnRCodeChanged();

	_selectedColumn->invalidate();
}

void ComputedColumnsModel::setComputeColumnJson(const QString & newJson)
{
	if(!_selectedColumn)
		return;

	if(_selectedColumn->setConstructorJson(fq(newJson)))
		emit computeColumnJsonChanged();
}

void ComputedColumnsModel::selectColumn(Column * column)
{
	if(_selectedColumn != column)
	{
		_selectedColumn = column;
		emit refreshProperties();
	}
}

void ComputedColumnsModel::setComputeColumnNameSelected(const QString & newName)
{
	if (DataSetPackage::pkg()->dataSet() && (!_selectedColumn || _selectedColumn->name() != fq(newName)))
		selectColumn(DataSetPackage::pkg()->dataSet()->column(fq(newName)));
}

bool ComputedColumnsModel::areLoopDependenciesOk(const std::string & columnName)
{

	return areLoopDependenciesOk(columnName, dataSet()->column(columnName)->analysisId() != -1 ? "" : dataSet()->column(columnName)->rCodeStripped());
}

bool ComputedColumnsModel::areLoopDependenciesOk(const std::string & columnName, const std::string & code)
{
	try
	{
		dataSet()->column(columnName)->checkForLoopInDependencies(code);
	}
	catch(std::logic_error & e)
	{
		validate(tq(columnName)); //To stop loading gif

		if(		dataSet()->column(columnName)->setError(e.what())	&&
				dataSet()->column(columnName) == _selectedColumn	)
			emit computeColumnErrorChanged();


		return false;
	}

	return true;
}

void ComputedColumnsModel::emitSendComputeCode(const QString & columnName, const QString & code, columnType colType)
{
	if(code.isEmpty())
		return;

	if(areLoopDependenciesOk(columnName.toStdString(), code.toStdString()))
		emit sendComputeCode(columnName, code, colType);
}

void ComputedColumnsModel::sendCode(const QString & code, const QString & json)
{
	_undoStack->push(new SetComputedColumnCodeCommand(DataSetPackage::pkg(), _selectedColumn->name(), code, json));
}


void ComputedColumnsModel::sendCode(const QString & code)
{
	setComputeColumnRCode(code);
	emitSendComputeCode(tq(_selectedColumn->name()), computeColumnRCodeCommentStripped(), _selectedColumn->type());
}

void ComputedColumnsModel::validate(const QString & columnName)
{
	Column * col = dataSet()->column(fq(columnName));
	if(col)
		col->validate();

	emitHeaderDataChanged(columnName);
}

void ComputedColumnsModel::invalidate(const QString & columnName)
{
	Column * col = dataSet()->column(fq(columnName));
	if(col)
		col->invalidate();

	emitHeaderDataChanged(columnName);
}

void ComputedColumnsModel::invalidateDependents(const std::string & columnName)
{
	for(Column * col : computedColumns())
		if(col->dependsOn(columnName))
			invalidate(tq(col->name()));
}


void ComputedColumnsModel::emitHeaderDataChanged(const QString & name)
{
	try
	{
		int index = DataSetPackage::pkg()->getColumnIndex(name.toStdString());
		emit headerDataChanged(Qt::Horizontal, index, index);
	}
	catch(...){}
}

void ComputedColumnsModel::revertToDefaultInvalidatedColumns()
{
	for(Column * col : computedColumns())
		if(col->invalidated())
			DataSetPackage::pkg()->columnSetDefaultValues(col->name());
}

void ComputedColumnsModel::computeColumnSucceeded(QString columnNameQ, QString warningQ, bool dataChanged)
{
	std::string columnName	= columnNameQ.toStdString(),
				warning		= warningQ.toStdString();

	if(!dataSet())
		return;

	bool shouldNotifyQML = _selectedColumn && _selectedColumn->name() == columnName;

	Column * column = dataSet()->column(columnName);

	if(!column)
		return;

	//First check for any updates from engine-side as setError might call incRevision()	
	column->checkForUpdates();
	
	if(column->setError(warning) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	emit refreshColumn(columnNameQ);
	
	validate(columnNameQ);

	if(dataChanged)
		checkForDependentColumnsToBeSent(columnNameQ);
}

void ComputedColumnsModel::computeColumnFailed(QString columnNameQ, QString errorQ)
{
	std::string columnName	= columnNameQ.toStdString(),
				error		= errorQ.toStdString();

	if(!dataSet())
		return;
	
	
	bool shouldNotifyQML = _selectedColumn && _selectedColumn->name() == columnName;
	
	Column * column = dataSet()->column(columnName);

	if(!column)
		return;

	if(areLoopDependenciesOk(columnName) && column->setError(error) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	DataSetPackage::pkg()->columnSetDefaultValues(columnName, columnType::unknown, false);
	emit refreshColumn(columnNameQ);

	validate(tq(columnName));
	invalidateDependents(columnName);
}

///Called from datatype changed
void ComputedColumnsModel::recomputeColumn(QString columnName)
{
	std::string		colName = fq(columnName);
	Column		*	col		= dataSet()->column(colName);

	if(col && col->isComputed() && col->codeType() != computedColumnType::analysis && col->codeType() == computedColumnType::analysisNotComputed)
		DataSetPackage::pkg()->columnSetDefaultValues(colName);

	checkForDependentColumnsToBeSent(columnName, col && col->isComputed());
}

void ComputedColumnsModel::checkForDependentColumnsToBeSent(QString columnNameQ, bool refreshMe)
{
	std::string columnName = fq(columnNameQ);

	for(Column * col : computedColumns())
		if(	col->codeType() != computedColumnType::analysis				&&
			col->codeType() != computedColumnType::analysisNotComputed	&&
			(
					col->dependsOn(columnName) ||
					(refreshMe && col->name() == columnName)
			) )
			invalidate(tq(col->name()));

	for(Column * col : computedColumns())
		if(	col->codeType() != computedColumnType::analysis				&&
			col->codeType() != computedColumnType::analysisNotComputed	&&
			col->iShouldBeSentAgain() )
			emitSendComputeCode(tq(col->name()), tq(col->rCodeStripped()), col->type());

	checkForDependentAnalyses(columnName);
}

void ComputedColumnsModel::checkForDependentAnalyses(const std::string & columnName)
{
	Analyses::analyses()->applyToAll([&](Analysis * analysis)
		{
			std::set<std::string> usedCols = analysis->usedVariables();

			if(usedCols.count(columnName) > 0)
			{
				bool allColsValidated = true;

				for(Column * col : computedColumns())
					if(usedCols.count(col->name()) > 0 && col->invalidated())
						allColsValidated = false;

				if(allColsValidated)
					analysis->refresh();
			}
		});
}

void ComputedColumnsModel::removeColumn()
{
	if(!_selectedColumn)
		return;

	// TODO pass RemoveColumnCommand aab
	_undoStack->pushCommand(new RemoveColumnsCommand(DataSetPackage::pkg(), _selectedColumn->id(), 1));

	DataSetPackage::pkg()->requestComputedColumnDestruction(_selectedColumn->name());
	setComputeColumnNameSelected("");
	emit refreshData();
}

void ComputedColumnsModel::datasetChanged(	QStringList				changedColumns,
											QStringList				missingColumns,
											QMap<QString, QString>	changeNameColumns,
											bool					rowCountChanged,
											bool					/*hasNewColumns*/)
{
	if(!DataSetPackage::pkg()->dataSet()) //Can occur during closing of a workspace in rare occasions.
		return;
	
	std::string concatenatedMissings = fq(missingColumns.join(", "));

	for(Column * col : computedColumns())
	{
		bool invalidateMe = rowCountChanged;

		for(const QString & changed : changedColumns)
			if(col->dependsOn(fq(changed), false))
				invalidateMe = true;

		bool containsAChangedName = false;
		for(const auto & changedNames : changeNameColumns.keys())
			if(col->dependsOn(fq(changedNames), false))
			{
				containsAChangedName = true;
				break;
			}

		if(containsAChangedName)
		{
			auto stdChangeNameCols = fq(changeNameColumns);
			invalidateMe = true;
			col->setRCode(ColumnEncoder::replaceColumnNamesInRScript(col->rCode(), stdChangeNameCols));
			col->setConstructorJson(JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(col->constructorJson(), stdChangeNameCols));

			if(col == _selectedColumn)
			{
				emit computeColumnJsonChanged();
				emit computeColumnRCodeChanged();
			}
		}

		if(col->codeType() == computedColumnType::constructorCode)
		{
			if(col->setConstructorJson(JsonUtilities::removeColumnsFromDragNDropFilterJSON(col->constructorJson(), fq(missingColumns))))
			{
				//So some column was removed from the json
				invalidateMe = true;

				col->setRCode("stop('Certain columns where removed from the definition of this computed column.\nColumns that could`ve been here are: " + concatenatedMissings + "')");

				if(col == _selectedColumn)
				{
					emit computeColumnJsonChanged();
					emit computeColumnRCodeChanged();
				}
			}
		}
		else if(col->codeType() == computedColumnType::rCode &&
				col->setRCode(ColumnEncoder::removeColumnNamesFromRScript(col->rCode(), fq(missingColumns))))
			{
				invalidateMe = true;

				if(col == _selectedColumn)
					emit computeColumnRCodeChanged();
			}

		if(invalidateMe)
			invalidate(tq(col->name()));

	}

	for(Column * col : computedColumns())
	{
		col->findDependencies(); //columnNames might have changed right? so check it again

		if(col->iShouldBeSentAgain())
			emitSendComputeCode(tq(col->name()), tq(col->rCodeStripped()), col->type());
	}

	emit refreshData();
}

void ComputedColumnsModel::onDataSetChanged()
{
	if(_selectedColumn && !DataSetPackage::pkg()->columnExists(_selectedColumn))
		_selectedColumn = nullptr;
	
	emit refreshProperties();
}


Column * ComputedColumnsModel::createComputedColumn(const std::string & name, int colType, computedColumnType computeType, Analysis * analysis)
{
	bool success			= false;

	bool	createActualComputedColumn	= computeType != computedColumnType::analysisNotComputed,
			showComputedColumn			= computeType != computedColumnType::analysis			&& createActualComputedColumn;

	if (createActualComputedColumn)
		DataSetPackage::pkg()->undoStack()->pushCommand(new CreateComputedColumnCommand(DataSetPackage::pkg(), tq(name), colType, int(computeType)));
	else
		DataSetPackage::pkg()->createColumn(name, columnType(colType));

	Column  * createdColumn = DataSetPackage::pkg()->getColumn(name);

	if(analysis)
		createdColumn->setAnalysisId(analysis->id());

	if(!createActualComputedColumn)
		emit dataColumnAdded(tq(name));

	if(showComputedColumn)
		selectColumn(createdColumn);

	return createdColumn;
}


bool ComputedColumnsModel::showAnalysisFormForColumn(const QString & columnName)
{
	try
	{
		Column		* col		= dataSet() ? dataSet()->column(fq(columnName)) : nullptr;
		Analysis	* analysis	= col && col->analysisId() != -1 ? Analyses::analyses()->get(col->analysisId()) : nullptr;

		if(analysis)
		{
			emit showAnalysisForm(analysis);
			return true;
		}

	}
	catch(...) {}

	return false;
}

void ComputedColumnsModel::analysisRemoved(Analysis * analysis)
{
	if (!dataSet())
		return;

	std::set<std::string> colsToRemove;

	for(Column * col : computedColumns())
		if(col->analysisId() == analysis->id())
			colsToRemove.insert(col->name());

	for(const std::string & col : colsToRemove)
		DataSetPackage::pkg()->requestComputedColumnDestruction(col);
}
