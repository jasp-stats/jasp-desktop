#include "computedcolumnsmodel.h"

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

void ComputedColumnsModel::emitSendComputeCode(QString columnName, QString code, Column::ColumnType colType)
{
	try
	{
		(*_computedColumns)[columnName.toStdString()].checkForLoopInDepenedencies(code.toStdString());
	}
	catch(std::logic_error e)
	{
		if(_computedColumns->setError(columnName.toStdString(), e.what()) && _currentlySelectedName == columnName)
			emit computeColumnErrorChanged();

		return;
	}

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

	if(_computedColumns->setError(columnName, error) && shouldNotifyQML)
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
		std::set<std::string> usedCols = analysis->usedColumnNames();

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

	int index = _package->dataSet()->getColumnIndex(_currentlySelectedName.toStdString());

	_computedColumns->removeComputedColumn(_currentlySelectedName.toStdString(), &_package->dataSet()->columns());

	emit headerDataChanged(Qt::Horizontal, index, _package->dataSet()->columns().columnCount());

	for(Analysis * analysis : *_analyses)
		analysis->removeUsedColumnName(_currentlySelectedName.toStdString());

	checkForDependentColumnsToBeSent(_currentlySelectedName.toStdString());

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

		for(const std::string & missing : missingColumns)
			if(col->dependsOn(missing))
				invalidateMe = true;

		for(const auto & changedNames : changeNameColumns)
			if(col->dependsOn(changedNames.first))
				invalidateMe = true;

		if(invalidateMe)
			invalidate(QString::fromStdString(col->name()), false);
	}

	for(ComputedColumn * col : *_computedColumns)
		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCode()), col->columnType());
}
