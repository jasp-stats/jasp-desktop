#include "computedcolumnmodel.h"
#include "jsonutilities.h"
#include "utilities/qutils.h"
#include "columnencoder.h"
#include "analysis/analyses.h"
#include "variableinfo.h"

ComputedColumnModel * ComputedColumnModel::_singleton = nullptr;

ComputedColumnModel::ComputedColumnModel()
	: QObject(DataSetPackage::pkg())
{
	assert(_singleton == nullptr);
	_singleton = this;

	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::computeColumnJsonChanged, Qt::QueuedConnection			);
	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::computeColumnRCodeChanged			);
	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::computeColumnErrorChanged			);
	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::computeColumnUsesRCodeChanged		);
	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::computeColumnIconSourceChanged	);
	connect(this,					&ComputedColumnModel::refreshProperties,		this,					&ComputedColumnModel::columnTypeChanged					);
	
	connect(this,					&ComputedColumnModel::refreshColumn,			DataSetPackage::pkg(),	&DataSetPackage::refreshColumn,								Qt::QueuedConnection);
	connect(this,					&ComputedColumnModel::refreshData,				DataSetPackage::pkg(),	&DataSetPackage::refresh,									Qt::QueuedConnection);
	
	connect(Analyses::analyses(),	&Analyses::requestComputedColumnCreation,		DataSetPackage::pkg(),	&DataSetPackage::requestComputedColumnCreation,				Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestColumnCreation,				DataSetPackage::pkg(),	&DataSetPackage::requestColumnCreation,						Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestComputedColumnDestruction,	DataSetPackage::pkg(),	&DataSetPackage::requestComputedColumnDestruction,			Qt::UniqueConnection);
    connect(Analyses::analyses(),	&Analyses::analysisRemoved,						this,					&ComputedColumnModel::analysisRemoved						);
}

const Columns &ComputedColumnModel::computedColumns() const			
{ 
	static const Columns _thereIsNoData;
	DataSet * data = DataSetPackage::pkg()->dataSet();
	
	return data ? data->computedColumns() : _thereIsNoData;
}

QString ComputedColumnModel::computeColumnRCode()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->rCode());
}

QString ComputedColumnModel::computeColumnRCodeCommentStripped()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->rCodeStripped());
}

bool ComputedColumnModel::computeColumnUsesRCode()
{
	return _selectedColumn && _selectedColumn->codeType() == computedColumnType::rCode;
}

QString ComputedColumnModel::computeColumnJson()
{
	QString json = !_selectedColumn ? "" : tq(_selectedColumn->constructorJsonStr());

	return json;
}

int ComputedColumnModel::computedColumnColumnType()
{
	return int(!_selectedColumn ? columnType::unknown : _selectedColumn->type());
}

QString ComputedColumnModel::computeColumnError()
{
	return !_selectedColumn ? "" : tq(_selectedColumn->error());
}

void ComputedColumnModel::setComputeColumnRCode(const QString & newCode)
{
	if(!_selectedColumn)
		return;

	if(_selectedColumn->setRCode(fq(newCode)))
		emit computeColumnRCodeChanged();

	_selectedColumn->invalidate();
}

void ComputedColumnModel::setComputeColumnJson(const QString & newJson)
{
	if(!_selectedColumn)
		return;

	if(_selectedColumn->setConstructorJson(fq(newJson)))
		emit computeColumnJsonChanged();
}

void ComputedColumnModel::selectColumn(Column * column)
{
	if(_selectedColumn != column)
	{
		_selectedColumn = column;
		emit refreshProperties();
	}
}

bool ComputedColumnModel::areLoopDependenciesOk(const std::string & columnName)
{

	return areLoopDependenciesOk(columnName, dataSet()->column(columnName)->analysisId() != -1 ? "" : dataSet()->column(columnName)->rCodeStripped());
}

bool ComputedColumnModel::areLoopDependenciesOk(const std::string & columnName, const std::string & code)
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

void ComputedColumnModel::emitSendComputeCode(Column * column)
{
	const std::string code = column->rCodeStripped();
	if(code.empty())
		return;

	if(areLoopDependenciesOk(column->name(), code))
		emit sendComputeCode(tq(column->name()), tq(code), column->type());
}

void ComputedColumnModel::sendCode(const QString & code, const QString & json)
{
	DataSetPackage::pkg()->undoStack()->push(new SetComputedColumnCodeCommand(DataSetPackage::pkg(), _selectedColumn->name(), code, json));
}


void ComputedColumnModel::sendCode(const QString & code)
{
	setComputeColumnRCode(code);
	emitSendComputeCode(_selectedColumn);
}

void ComputedColumnModel::validate(const QString & columnName)
{
	Column * col = dataSet()->column(fq(columnName));
	if(col)
		col->validate();

	emitHeaderDataChanged(columnName);
}

void ComputedColumnModel::invalidate(const QString & columnName)
{
	Column * col = dataSet()->column(fq(columnName));
	if(col)
		col->invalidate();

	emitHeaderDataChanged(columnName);
}

void ComputedColumnModel::invalidateDependents(const std::string & columnName)
{
	for(Column * col : computedColumns())
		if(col->dependsOn(columnName))
			invalidate(tq(col->name()));
}


void ComputedColumnModel::emitHeaderDataChanged(const QString & name)
{
	try
	{
		int index = DataSetPackage::pkg()->getColumnIndex(name.toStdString());
		emit headerDataChanged(Qt::Horizontal, index, index);
	}
	catch(...){}
}

void ComputedColumnModel::revertToDefaultInvalidatedColumns()
{
	for(Column * col : computedColumns())
		if(col->invalidated())
			DataSetPackage::pkg()->columnSetDefaultValues(col->name());
}

void ComputedColumnModel::computeColumnRemoved(QString columnNameQ)
{
	std::string columnName	= columnNameQ.toStdString();

	if(!dataSet())
		return;
	
	dataSet()->checkForUpdates();
}


void ComputedColumnModel::computeColumnSucceeded(QString columnNameQ, QString warningQ, bool dataChanged)
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
	
	DataSetPackage::pkg()->labelFilterChanged(); //in case the user had enabled some labelfilter on the computed column?
}

void ComputedColumnModel::computeColumnFailed(QString columnNameQ, QString errorQ)
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
void ComputedColumnModel::recomputeColumn(QString columnName)
{
	std::string		colName = fq(columnName);
	Column		*	col		= dataSet()->column(colName);

	if(col && col->isComputed() && col->codeType() == computedColumnType::analysisNotComputed)
		DataSetPackage::pkg()->columnSetDefaultValues(colName);

	checkForDependentColumnsToBeSent(columnName, col && col->isComputed());
}

void ComputedColumnModel::checkForDependentColumnsToBeSent(QString columnNameQ, bool refreshMe)
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
			emitSendComputeCode(col);

	checkForDependentAnalyses(columnName);
}

void ComputedColumnModel::checkForDependentAnalyses(const std::string & columnName)
{
	Analyses::analyses()->applyToAll([&](Analysis * analysis)
	{
		stringset	usedCols	= analysis->usedVariables(),
					createdCols = analysis->createdVariables();

		//Dont create an infinite loop please, but do this only for non-computed columns created by an analysis (aka distributions, because otherwise it breaks things like planning from audit)
		if(usedCols.count(columnName) && (!createdCols.count(columnName) || !DataSetPackage::pkg()->isColumnAnalysisNotComputed(columnName)))
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

void ComputedColumnModel::removeColumn()
{
	if(!_selectedColumn)
		return;

	// TODO pass RemoveColumnCommand aab
	DataSetPackage::pkg()->undoStack()->pushCommand(new RemoveColumnsCommand(DataSetPackage::pkg(), _selectedColumn->id(), 1));
	
	emit refreshData();
}

void ComputedColumnModel::datasetChanged(	QStringList				changedColumns,
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
			emitSendComputeCode(col);
	}

	emit refreshData();
}

Column * ComputedColumnModel::createComputedColumn(const std::string & name, int colType, computedColumnType computeType, Analysis * analysis)
{
	bool	success						= false,
			createActualComputedColumn	= computeType != computedColumnType::analysisNotComputed,
			showComputedColumn			= computeType != computedColumnType::analysis			&& createActualComputedColumn;

	if (createActualComputedColumn)	DataSetPackage::pkg()->undoStack()->pushCommand(new CreateComputedColumnCommand(tq(name), colType, int(computeType)));
	else							DataSetPackage::pkg()->createColumn(name, columnType(colType));

	Column  * createdColumn = DataSetPackage::pkg()->getColumn(name);

	if(analysis)
		createdColumn->setAnalysisId(analysis->id());

	if(!createActualComputedColumn)
		emit dataColumnAdded(tq(name));

	if(showComputedColumn)
	{
		selectColumn(createdColumn);
		emit chooseColumn(tq(createdColumn->name()));
	}

	return createdColumn;
}

void ComputedColumnModel::createComputedColumn(const QString &name, int columnType, bool jsonPlease)	
{ 
	createComputedColumn(fq(name), columnType, jsonPlease ? computedColumnType::constructorCode : computedColumnType::rCode);	
}

bool ComputedColumnModel::showAnalysisFormForColumn(const QString & columnName)
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

void ComputedColumnModel::analysisRemoved(Analysis * analysis)
{
	if (!dataSet())
		return;

	std::set<std::string> colsToRemove;

	for(Column * col : computedColumns())
		if(col->analysisId() == analysis->id() && col->codeType() != computedColumnType::analysisNotComputed)
			colsToRemove.insert(col->name());

	for(const std::string & col : colsToRemove)
		DataSetPackage::pkg()->requestComputedColumnDestruction(col, analysis);
}

QString ComputedColumnModel::computeColumnIconSource() const
{
	return VariableInfo::getIconFile(!_selectedColumn ? columnType::unknown : _selectedColumn->type(), VariableInfo::DefaultIconType);
}
