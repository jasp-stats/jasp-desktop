#include "computedcolumnsmodel.h"
#include "utilities/jsonutilities.h"
#include "sharedmemory.h"
#include "log.h"

ComputedColumnsModel::ComputedColumnsModel(Analyses * analyses, QObject * parent)
	: QObject(parent), _analyses(analyses)
{
	connect(this,		&ComputedColumnsModel::datasetLoadedChanged,	this, &ComputedColumnsModel::computeColumnJsonChanged			);
	connect(this,		&ComputedColumnsModel::datasetLoadedChanged,	this, &ComputedColumnsModel::computeColumnRCodeChanged			);
	connect(this,		&ComputedColumnsModel::datasetLoadedChanged,	this, &ComputedColumnsModel::computeColumnErrorChanged			);
	connect(this,		&ComputedColumnsModel::datasetLoadedChanged,	this, &ComputedColumnsModel::computeColumnUsesRCodeChanged		);
	connect(this,		&ComputedColumnsModel::datasetLoadedChanged,	this, &ComputedColumnsModel::computeColumnNameSelectedChanged	);
	connect(_analyses,	&Analyses::requestComputedColumnCreation,		this, &ComputedColumnsModel::requestComputedColumnCreation,		Qt::UniqueConnection);
	connect(_analyses,	&Analyses::requestColumnCreation,				this, &ComputedColumnsModel::requestColumnCreation,				Qt::UniqueConnection);
	connect(_analyses,	&Analyses::requestComputedColumnDestruction,	this, &ComputedColumnsModel::requestComputedColumnDestruction,	Qt::UniqueConnection);
	connect(_analyses,	&Analyses::analysisRemoved,						this, &ComputedColumnsModel::analysisRemoved					);

}

QString ComputedColumnsModel::computeColumnRCode()
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return "";

	return QString::fromStdString(_computedColumns->getRCode(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnRCodeCommentStripped()
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return "";

	return QString::fromStdString(_computedColumns->getRCodeCommentStripped(_currentlySelectedName.toStdString()));
}

bool ComputedColumnsModel::computeColumnUsesRCode()
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return "";

	return _computedColumns->usesRCode(_currentlySelectedName.toStdString());
}

QString ComputedColumnsModel::computeColumnJson()
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return "";

	return QString::fromStdString(_computedColumns->getConstructorJson(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnError()
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return "";

	return QString::fromStdString(_computedColumns->getError(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnNameSelected()
{
	return _currentlySelectedName;
}

void ComputedColumnsModel::setComputeColumnRCode(QString newCode)
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return;

	if(_computedColumns->setRCode(_currentlySelectedName.toStdString(), newCode.toStdString()))
		emit computeColumnRCodeChanged();

	invalidate(_currentlySelectedName);
}


void ComputedColumnsModel::setComputeColumnJson(QString newJson)
{
	if(_currentlySelectedName == "" || _computedColumns == nullptr)
		return;

	if(_computedColumns->setConstructorJson(_currentlySelectedName.toStdString(), newJson.toStdString()))
		emit computeColumnJsonChanged();
}

void ComputedColumnsModel::setComputeColumnNameSelected(QString newName)
{
	if(_currentlySelectedName != newName)
	{
		_currentlySelectedName = newName;

		emit computeColumnJsonChanged();
		emit computeColumnRCodeChanged();
		emit computeColumnErrorChanged();
		emit computeColumnUsesRCodeChanged();
		emit computeColumnNameSelectedChanged();
	}
}

bool ComputedColumnsModel::areLoopDependenciesOk(std::string columnName)
{

	return areLoopDependenciesOk(columnName, (*_computedColumns)[columnName].analysis() != nullptr ? "" : (*_computedColumns)[columnName].rCodeCommentStripped());
}

bool ComputedColumnsModel::areLoopDependenciesOk(std::string columnName, std::string code)
{
	try
	{
		(*_computedColumns)[columnName].checkForLoopInDepenedencies(code);
	}
	catch(std::logic_error e)
	{
		validate(QString::fromStdString(columnName)); //To stop loading gif

		if(_computedColumns->setError(columnName, e.what()) && _currentlySelectedName.toStdString() == columnName)
			emit computeColumnErrorChanged();


		return false;
	}

	return true;
}

void ComputedColumnsModel::emitSendComputeCode(QString columnName, QString code, Column::ColumnType colType)
{
	if(areLoopDependenciesOk(columnName.toStdString(), code.toStdString()))
		emit sendComputeCode(columnName, code, colType);
}

void ComputedColumnsModel::sendCode(QString code, QString json)
{
	setComputeColumnJson(json);

	if(code == "")
		setComputeColumnRCode(code);
	else
		sendCode(code);
}


void ComputedColumnsModel::sendCode(QString code)
{
	std::string columnName = _currentlySelectedName.toStdString();
	setComputeColumnRCode(code);
	emitSendComputeCode(_currentlySelectedName, computeColumnRCodeCommentStripped(), (*_computedColumns)[columnName].columnType());
}

void ComputedColumnsModel::validate(QString columnName)
{
	try
	{
		(*_computedColumns)[columnName.toStdString()].validate();
		emitHeaderDataChanged(columnName);
	} catch(columnNotFound & ){}
}

void ComputedColumnsModel::invalidate(QString columnName)
{
	try
	{
		(*_computedColumns)[columnName.toStdString()].invalidate();
		emitHeaderDataChanged(columnName);
	} catch(columnNotFound & ){}
}

void ComputedColumnsModel::invalidateDependents(std::string columnName)
{
	for(ComputedColumn * col : *_computedColumns)
		if(col->dependsOn(columnName))
			invalidate(QString::fromStdString(col->name()));
}


void ComputedColumnsModel::emitHeaderDataChanged(QString name)
{
	try
	{
		int index = _package->dataSet()->getColumnIndex(name.toStdString());
		emit headerDataChanged(Qt::Horizontal, index, index);
	}
	catch(...){}
}

void ComputedColumnsModel::setDataSetPackage(DataSetPackage * package)
{
	DataSetPackage * oldPackage = _package;

	_package = package;
	_computedColumns = _package == nullptr ? nullptr : _package->computedColumnsPointer();

	if(oldPackage != _package)
		emit datasetLoadedChanged();
}

void ComputedColumnsModel::revertToDefaultInvalidatedColumns()
{
	for(ComputedColumn * col : *_computedColumns)
		if(col->isInvalidated())
		{
			try
			{
				col->column()->setDefaultValues();

				emit refreshColumn(col->column());
				emitHeaderDataChanged(QString::fromStdString(col->name()));

			}
			catch(...){}
		}
}

void ComputedColumnsModel::computeColumnSucceeded(QString columnNameQ, QString warningQ, bool dataChanged)
{
	std::string columnName	= columnNameQ.toStdString(),
				warning		= warningQ.toStdString();

	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(_computedColumns->setError(columnName, warning) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	if(dataChanged)
		try{
			emit refreshColumn((*_computedColumns)[columnName].column());
		}
		catch(...){}

	validate(QString::fromStdString(columnName));

	if(dataChanged)
		checkForDependentColumnsToBeSent(columnName);
}

void ComputedColumnsModel::computeColumnFailed(QString columnNameQ, QString errorQ)
{
	std::string columnName	= columnNameQ.toStdString(),
				error		= errorQ.toStdString();

	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(areLoopDependenciesOk(columnName) && _computedColumns->setError(columnName, error) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	clearColumn(columnName);

	validate(QString::fromStdString(columnName));
	invalidateDependents(columnName);

}

void ComputedColumnsModel::clearColumn(std::string columnName)
{
	try
	{
		ComputedColumn * col = &((*_computedColumns)[columnName]);
		col->column()->setDefaultValues();

		emit refreshColumn(col->column());
		emitHeaderDataChanged(QString::fromStdString(col->name()));
	}
	catch(columnNotFound e){}


}

void ComputedColumnsModel::recomputeColumn(std::string columnName)
{
	clearColumn(columnName);
	_computedColumns->findAllColumnNames();
	try
	{
		ComputedColumn * col = &((*_computedColumns)[columnName]);
		col->findDependencies();
	}
	catch(columnNotFound e){}

	checkForDependentColumnsToBeSent(columnName, true);
}

void ComputedColumnsModel::checkForDependentColumnsToBeSent(std::string columnName, bool refreshMe)
{
	for(ComputedColumn * col : *_computedColumns)
		if(col->dependsOn(columnName) || (refreshMe && col->name() == columnName))
			invalidate(QString::fromStdString(col->name()));

	for(ComputedColumn * col : *_computedColumns)
		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCodeCommentStripped()), col->columnType());

	checkForDependentAnalyses(columnName);
}

void ComputedColumnsModel::checkForDependentAnalyses(std::string columnName)
{
	assert(_analyses != nullptr);

	_analyses->applyToAll([&](Analysis * analysis)
		{
			std::set<std::string> usedCols = analysis->usedVariables();

			if(usedCols.count(columnName) > 0)
			{
				bool allColsValidated = true;

				for(ComputedColumn * col : *_computedColumns)
					if(usedCols.count(col->name()) > 0 && col->isInvalidated())
						allColsValidated = false;

				if(allColsValidated)
					analysis->refresh();
			}
		});
}

void ComputedColumnsModel::removeColumn()
{
	if(_currentlySelectedName == "")
		return;

	requestComputedColumnDestruction(_currentlySelectedName);

	setComputeColumnNameSelected("");
}

void ComputedColumnsModel::packageSynchronized(const std::vector<std::string> & changedColumns, const std::vector<std::string> & missingColumns, const std::map<std::string, std::string> & changeNameColumns, bool rowCountChanged)
{
	_computedColumns->refreshColumnPointers();

	for(ComputedColumn * col : *_computedColumns)
	{
		bool invalidateMe = rowCountChanged;

		for(const std::string & changed : changedColumns)
			if(col->dependsOn(changed, false))
				invalidateMe = true;

		bool containsAChangedName = false;
		for(const auto & changedNames : changeNameColumns)
			if(col->dependsOn(changedNames.first, false))
				containsAChangedName = true;

		if(containsAChangedName)
		{
			invalidateMe = true;
			col->replaceChangedColumnNamesInRCode(changeNameColumns);
			col->setConstructorJson(JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(col->constructorJson(), changeNameColumns));

			if(col->name() == _currentlySelectedName.toStdString())
			{
				emit computeColumnJsonChanged();
				emit computeColumnRCodeChanged();
			}
		}

		for(const std::string & missing : missingColumns)
			if(col->dependsOn(missing, false))
			{
				invalidateMe = true;
				col->setConstructorJson(JsonUtilities::removeColumnsFromDragNDropFilterJSON(col->constructorJson(), missing));
				if(col->codeType() == ComputedColumn::computedType::constructorCode)	col->setRCode("stop('Certain columns where removed from the definition of this computed column.')");

				if(col->name() == _currentlySelectedName.toStdString())
				{
					emit computeColumnJsonChanged();
					emit computeColumnRCodeChanged();
				}
			}



		if(invalidateMe)
			invalidate(QString::fromStdString(col->name()));

	}

	_computedColumns->findAllColumnNames();

	for(ComputedColumn * col : *_computedColumns)
	{
		col->findDependencies(); //columnNames might have changed right? so check it again

		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCodeCommentStripped()), col->columnType());
	}
}


ComputedColumn * ComputedColumnsModel::createComputedColumn(QString name, int columnType, ComputedColumn::computedType computeType, Analysis * analysis)
{
	bool success			= false;
	DataSet	*theData		= _package->dataSet();
	size_t newColumnIndex	= theData->columnCount();

	do
	{
		try {
			theData->setColumnCount(newColumnIndex + 1);
			success = true;
		}
		catch (boost::interprocess::bad_alloc &e)
		{
			try {	theData = SharedMemory::enlargeDataSet(theData);	}
			catch (std::exception &e)	{	throw std::runtime_error("Out of memory: this data set is too large for your computer's available memory");	}
		}
		catch (std::exception e)	{	Log::log() << "ComputedColumnsModel::createComputedColum std::exception: " << e.what()	<< std::endl; 	}
		catch (...)					{	Log::log() << "ComputedColumnsModel::createComputedColum some other exception\n "		<< std::endl;	}
	}
	while (!success);

	bool createActualComputedColumn = computeType != ComputedColumn::computedType::analysisNotComputed;

	ComputedColumn  * createdColumn = nullptr;

	if(createActualComputedColumn)
	{
		createdColumn = computedColumnsPointer()->createComputedColumn(name.toStdString(), (Column::ColumnType)columnType, computeType);
		createdColumn->setAnalysis(analysis);
	}
	else
		computedColumnsPointer()->createColumn(name.toStdString(), (Column::ColumnType)columnType);

	emit dataSetChanged(_package->dataSet());
	emit refreshData();

	if(createActualComputedColumn)		setLastCreatedColumn(name);
	else								emit dataColumnAdded(name);

	return createdColumn;
}

ComputedColumn *	ComputedColumnsModel::requestComputedColumnCreation(QString columnName, Analysis * analysis)
{
	if(!_package->isColumnNameFree(columnName.toStdString()))
		return nullptr;

	return createComputedColumn(columnName, (int)Column::ColumnTypeScale, ComputedColumn::computedType::analysis, analysis);
}

void ComputedColumnsModel::requestColumnCreation(QString columnName, Analysis * analysis, int columnType)
{
	if(_package->isColumnNameFree(columnName.toStdString()))
		createComputedColumn(columnName, columnType, ComputedColumn::computedType::analysisNotComputed, analysis);
}


void ComputedColumnsModel::requestComputedColumnDestruction(QString columnNameQ)
{
	if(columnNameQ == "")
		return;

	std::string columnName = columnNameQ.toStdString();

	int index = _package->dataSet()->getColumnIndex(columnName);

	_computedColumns->removeComputedColumn(columnName);

	emit headerDataChanged(Qt::Horizontal, index, _package->dataSet()->columns().columnCount() + 1);

	_analyses->applyToAll([&](Analysis * analysis)
		{ analysis->removeUsedVariable(columnName); } );

	checkForDependentColumnsToBeSent(columnName);

	if(columnNameQ == lastCreatedColumn())
		setLastCreatedColumn("");
}

bool ComputedColumnsModel::showAnalysisFormForColumn(QString columnName)
{
	try
	{
		ComputedColumn * col = &(*_computedColumns)[columnName.toStdString()];

		if(col->analysis() != nullptr)
		{
			emit showAnalysisForm(col->analysis());
			return true;
		}

	}
	catch(...) {}

	return false;
}

void ComputedColumnsModel::setLastCreatedColumn(QString lastCreatedColumn)
{
	if (_lastCreatedColumn == lastCreatedColumn)
		return;

	_lastCreatedColumn = lastCreatedColumn;
	emit lastCreatedColumnChanged(_lastCreatedColumn);
}

void ComputedColumnsModel::analysisRemoved(Analysis * analysis)
{
	if(_computedColumns == nullptr)
		return;

	std::set<QString> colsToRemove;

	for(auto * col : *_computedColumns)
		if(col->analysis() == analysis)
			colsToRemove.insert(QString::fromStdString(col->name()));

	for(const QString & col : colsToRemove)
		requestComputedColumnDestruction(col);
}
