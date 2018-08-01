#include "computedcolumnsmodel.h"
#include "utilities/jsonutilities.h"
#include "sharedmemory.h"

ComputedColumnsModel::ComputedColumnsModel(Analyses * analyses, QObject * parent) : QObject(parent), _analyses(analyses)
{
	connect(_analyses,	&Analyses::requestComputedColumnCreation,		this,	&ComputedColumnsModel::requestComputedColumnCreation,		Qt::UniqueConnection);
	connect(_analyses,	&Analyses::requestComputedColumnDestruction,	this,	&ComputedColumnsModel::requestComputedColumnDestruction,	Qt::UniqueConnection);
}

QString ComputedColumnsModel::computeColumnRCode()
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
		return "";

	return QString::fromStdString(_computedColumns->getRCode(_currentlySelectedName.toStdString()));
}

bool ComputedColumnsModel::computeColumnUsesRCode()
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
		return "";

	return _computedColumns->usesRCode(_currentlySelectedName.toStdString());
}

QString ComputedColumnsModel::computeColumnJson()
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
		return "";

	return QString::fromStdString(_computedColumns->getConstructorJson(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnError()
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
		return "";

	return QString::fromStdString(_computedColumns->getError(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnNameSelected()
{
	return _currentlySelectedName;
}

void ComputedColumnsModel::setComputeColumnRCode(QString newCode)
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
		return;

	if(_computedColumns->setRCode(_currentlySelectedName.toStdString(), newCode.toStdString()))
		emit computeColumnRCodeChanged();

	invalidate(_currentlySelectedName);
}


void ComputedColumnsModel::setComputeColumnJson(QString newJson)
{
	if(_currentlySelectedName == "" || _computedColumns == NULL)
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

	return areLoopDependenciesOk(columnName, (*_computedColumns)[columnName].analysis() != NULL ? "" : (*_computedColumns)[columnName].rCode());
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
	emitSendComputeCode(_currentlySelectedName, code, (*_computedColumns)[columnName].columnType());
}


void ComputedColumnsModel::invalidate(QString name, bool setDefaultsVals)
{
	(*_computedColumns)[name.toStdString()].invalidate();

	if(setDefaultsVals)
		revertToDefaultInvalidatedColumns();
}

void ComputedColumnsModel::validate(QString name)
{
	(*_computedColumns)[name.toStdString()].validate();
	emitHeaderDataChanged(name);
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
	_computedColumns = _package == NULL ? NULL : _package->computedColumnsPointer();

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

void ComputedColumnsModel::computeColumnSucceeded(std::string columnName, std::string warning)
{

	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(_computedColumns->setError(columnName, warning) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	try{
		emit refreshColumn((*_computedColumns)[columnName].column());
	}
	catch(...){}

	validate(QString::fromStdString(columnName));
	checkForDependentColumnsToBeSent(columnName);
}

void ComputedColumnsModel::computeColumnFailed(std::string columnName, std::string error)
{
	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(areLoopDependenciesOk(columnName) && _computedColumns->setError(columnName, error) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	validate(QString::fromStdString(columnName));
}

void ComputedColumnsModel::checkForDependentColumnsToBeSent(std::string columnName, bool refreshMe)
{
	for(ComputedColumn * col : *_computedColumns)
		if(col->dependsOn(columnName) || (refreshMe && col->name() == columnName))
			invalidate(QString::fromStdString(col->name()), false);

	for(ComputedColumn * col : *_computedColumns)
		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCode()), col->columnType());

	checkForDependentAnalyses(columnName);
}

void ComputedColumnsModel::checkForDependentAnalyses(std::string columnName)
{
	assert(_analyses != NULL);

	for(Analysis * analysis : *_analyses)
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
	}
}

void ComputedColumnsModel::removeColumn()
{
	if(_currentlySelectedName == "")
		return;

	requestComputedColumnDestruction(_currentlySelectedName.toStdString());

	setComputeColumnNameSelected("");
}

void ComputedColumnsModel::packageSynchronized(const std::vector<std::string> & changedColumns, const std::vector<std::string> & missingColumns, const std::map<std::string, std::string> & changeNameColumns)
{
	for(ComputedColumn * col : *_computedColumns)
	{
		bool invalidateMe = false;

		for(const std::string & changed : changedColumns)
			if(col->dependsOn(changed))
				invalidateMe = true;

		bool containsAChangedName = false;
		for(const auto & changedNames : changeNameColumns)
			if(col->dependsOn(changedNames.first))
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
			if(col->dependsOn(missing))
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
			invalidate(QString::fromStdString(col->name()), false);

	}

	_computedColumns->findAllColumnNames();

	for(ComputedColumn * col : *_computedColumns)
	{
		col->findDependencies(); //columnNames might have changed right? so check it again

		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCode()), col->columnType());
	}
}


ComputedColumn * ComputedColumnsModel::createComputedColumn(QString name, int columnType, ComputedColumn::computedType computeType)
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
		catch (std::exception e)	{	std::cout << "ComputedColumnsModel::createComputedColum std::exception: " << e.what()	<< std::endl; 	}
		catch (...)					{	std::cout << "ComputedColumnsModel::createComputedColum some other exception\n "		<< std::endl;	}
	}
	while (!success);

	if(theData != _package->dataSet())
		emit dataSetChanged(_package->dataSet());

	ComputedColumn  * createdColumn = computedColumnsPointer()->createComputedColumn(name.toStdString(), (Column::ColumnType)columnType, computeType);
	emit refreshData();

	return createdColumn;
}

ComputedColumn *	ComputedColumnsModel::requestComputedColumnCreation(std::string columnName, Analysis * analysis)
{
	if(!_package->isColumnNameFree(columnName))
		return NULL;

	ComputedColumn * result = createComputedColumn(QString::fromStdString(columnName), (int)Column::ColumnTypeScale, ComputedColumn::computedType::analysis);
	result->setAnalysis(analysis);

	return result;
}


void ComputedColumnsModel::requestComputedColumnDestruction(std::string columnName)
{
	if(columnName == "")
		return;

	int index = _package->dataSet()->getColumnIndex(columnName);

	_computedColumns->removeComputedColumn(columnName);

	emit headerDataChanged(Qt::Horizontal, index, _package->dataSet()->columns().columnCount() + 1);

	for(Analysis * analysis : *_analyses)
		analysis->removeUsedVariable(columnName);

	checkForDependentColumnsToBeSent(columnName);
}

bool ComputedColumnsModel::showAnalysisFormForColumn(QString columnName)
{
	try
	{
		ComputedColumn * col = &(*_computedColumns)[columnName.toStdString()];

		if(col->analysis() != NULL)
		{
			emit showAnalysisForm(col->analysis());
			return true;
		}

	}
	catch(...) {}

	return false;
}
