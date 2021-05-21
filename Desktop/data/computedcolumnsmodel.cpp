#include "computedcolumnsmodel.h"
#include "utilities/jsonutilities.h"
#include "utilities/qutils.h"
#include "columnencoder.h"
#include "sharedmemory.h"
#include "log.h"

ComputedColumnsModel * ComputedColumnsModel::_singleton = nullptr;

ComputedColumnsModel::ComputedColumnsModel()
	: QObject(DataSetPackage::pkg())
{
	assert(_singleton == nullptr);
	_singleton = this;

	//Is it really dataSetChanged that needs to connect to datasetLoadedChanged?
	connect(DataSetPackage::pkg(),	&DataSetPackage::dataSetChanged,				this,					&ComputedColumnsModel::datasetLoadedChanged					);

	connect(this,					&ComputedColumnsModel::datasetLoadedChanged,	this,					&ComputedColumnsModel::computeColumnJsonChanged				);
	connect(this,					&ComputedColumnsModel::datasetLoadedChanged,	this,					&ComputedColumnsModel::computeColumnRCodeChanged			);
	connect(this,					&ComputedColumnsModel::datasetLoadedChanged,	this,					&ComputedColumnsModel::computeColumnErrorChanged			);
	connect(this,					&ComputedColumnsModel::datasetLoadedChanged,	this,					&ComputedColumnsModel::computeColumnUsesRCodeChanged		);
	connect(this,					&ComputedColumnsModel::datasetLoadedChanged,	this,					&ComputedColumnsModel::computeColumnNameSelectedChanged		);
	connect(this,					&ComputedColumnsModel::refreshColumn,			DataSetPackage::pkg(),	&DataSetPackage::refreshColumn,								Qt::QueuedConnection);
	connect(this,					&ComputedColumnsModel::headerDataChanged,		DataSetPackage::pkg(),	&DataSetPackage::headerDataChanged,							Qt::QueuedConnection);
	connect(this,					&ComputedColumnsModel::refreshData,				DataSetPackage::pkg(),	&DataSetPackage::refresh,									Qt::QueuedConnection);

	connect(Analyses::analyses(),	&Analyses::requestComputedColumnCreation,		this,					&ComputedColumnsModel::requestComputedColumnCreation,		Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestColumnCreation,				this,					&ComputedColumnsModel::requestColumnCreation,				Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::requestComputedColumnDestruction,	this,					&ComputedColumnsModel::requestComputedColumnDestruction,	Qt::UniqueConnection);
	connect(Analyses::analyses(),	&Analyses::analysisRemoved,						this,					&ComputedColumnsModel::analysisRemoved						);

}

QString ComputedColumnsModel::computeColumnRCode()
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return "";

	return QString::fromStdString(computedColumns()->getRCode(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnRCodeCommentStripped()
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return "";

	return QString::fromStdString(computedColumns()->getRCodeCommentStripped(_currentlySelectedName.toStdString()));
}

bool ComputedColumnsModel::computeColumnUsesRCode()
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return "";

	return computedColumns()->usesRCode(_currentlySelectedName.toStdString());
}

QString ComputedColumnsModel::computeColumnJson()
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return "";

	return QString::fromStdString(computedColumns()->getConstructorJson(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnError()
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return "";

	return QString::fromStdString(computedColumns()->getError(_currentlySelectedName.toStdString()));
}

QString ComputedColumnsModel::computeColumnNameSelected()
{
	return _currentlySelectedName;
}

void ComputedColumnsModel::setComputeColumnRCode(QString newCode)
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return;

	if(computedColumns()->setRCode(_currentlySelectedName.toStdString(), newCode.toStdString()))
		emit computeColumnRCodeChanged();

	invalidate(_currentlySelectedName);
}


void ComputedColumnsModel::setComputeColumnJson(QString newJson)
{
	if(_currentlySelectedName == "" || computedColumns() == nullptr)
		return;

	if(computedColumns()->setConstructorJson(_currentlySelectedName.toStdString(), newJson.toStdString()))
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

	return areLoopDependenciesOk(columnName, (*computedColumns())[columnName].analysis() != nullptr ? "" : (*computedColumns())[columnName].rCodeCommentStripped());
}

bool ComputedColumnsModel::areLoopDependenciesOk(std::string columnName, std::string code)
{
	try
	{
		(*computedColumns())[columnName].checkForLoopInDepenedencies(code);
	}
	catch(std::logic_error & e)
	{
		validate(QString::fromStdString(columnName)); //To stop loading gif

		if(computedColumns()->setError(columnName, e.what()) && _currentlySelectedName.toStdString() == columnName)
			emit computeColumnErrorChanged();


		return false;
	}

	return true;
}

void ComputedColumnsModel::emitSendComputeCode(QString columnName, QString code, columnType colType)
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
	emitSendComputeCode(_currentlySelectedName, computeColumnRCodeCommentStripped(), DataSetPackage::pkg()->getColumnType(columnName));
}

void ComputedColumnsModel::validate(QString columnName)
{
	try
	{
		(*computedColumns())[columnName.toStdString()].validate();
		emitHeaderDataChanged(columnName);
	} catch(columnNotFound & ){}
}

void ComputedColumnsModel::invalidate(QString columnName)
{
	try
	{
		(*computedColumns())[columnName.toStdString()].invalidate();
		emitHeaderDataChanged(columnName);
	} catch(columnNotFound & ){}
}

void ComputedColumnsModel::invalidateDependents(std::string columnName)
{
	for(ComputedColumn * col : *computedColumns())
		if(col->dependsOn(columnName))
			invalidate(QString::fromStdString(col->name()));
}


void ComputedColumnsModel::emitHeaderDataChanged(QString name)
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
	for(ComputedColumn * col : *computedColumns())
		if(col->isInvalidated())
			DataSetPackage::pkg()->columnSetDefaultValues(col->name());
}

void ComputedColumnsModel::computeColumnSucceeded(QString columnNameQ, QString warningQ, bool dataChanged)
{
	std::string columnName	= columnNameQ.toStdString(),
				warning		= warningQ.toStdString();

	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(computedColumns()->setError(columnName, warning) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	if(dataChanged)
			emit refreshColumn(tq(columnName));

	validate(QString::fromStdString(columnName));

	if(dataChanged)
		checkForDependentColumnsToBeSent(columnName);
}

void ComputedColumnsModel::computeColumnFailed(QString columnNameQ, QString errorQ)
{
	std::string columnName	= columnNameQ.toStdString(),
				error		= errorQ.toStdString();

	bool shouldNotifyQML = _currentlySelectedName.toStdString() == columnName;

	if(areLoopDependenciesOk(columnName) && computedColumns()->setError(columnName, error) && shouldNotifyQML)
		emit computeColumnErrorChanged();

	DataSetPackage::pkg()->columnSetDefaultValues(columnName);

	validate(QString::fromStdString(columnName));
	invalidateDependents(columnName);

}

///Called from datatype changed
void ComputedColumnsModel::recomputeColumn(QString columnName)
{
	std::string colName = columnName.toStdString();
	if(!DataSetPackage::pkg()->isColumnComputed(colName))
		return;

	//It will be found because we just checked for it in isColumnComputed
	ComputedColumn * col = &((*computedColumns())[colName]);

	if(col->codeType() == ComputedColumn::computedType::analysis || col->codeType() == ComputedColumn::computedType::analysisNotComputed)
		return;

	DataSetPackage::pkg()->columnSetDefaultValues(colName);
	computedColumns()->findAllColumnNames();


	checkForDependentColumnsToBeSent(colName, true);
}

void ComputedColumnsModel::checkForDependentColumnsToBeSent(std::string columnName, bool refreshMe)
{
	for(ComputedColumn * col : *computedColumns())
		if(	col->codeType() != ComputedColumn::computedType::analysis				&&
			col->codeType() != ComputedColumn::computedType::analysisNotComputed	&&
			(
					col->dependsOn(columnName) ||
					(refreshMe && col->name() == columnName)
			) )
			invalidate(QString::fromStdString(col->name()));

	for(ComputedColumn * col : *computedColumns())
		if(	col->codeType() != ComputedColumn::computedType::analysis				&&
			col->codeType() != ComputedColumn::computedType::analysisNotComputed	&&
			col->iShouldBeSentAgain() )
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCodeCommentStripped()), DataSetPackage::pkg()->getColumnType(columnName));

	checkForDependentAnalyses(columnName);
}

void ComputedColumnsModel::checkForDependentAnalyses(std::string columnName)
{
	Analyses::analyses()->applyToAll([&](Analysis * analysis)
		{
			std::set<std::string> usedCols = analysis->usedVariables();

			if(usedCols.count(columnName) > 0)
			{
				bool allColsValidated = true;

				for(ComputedColumn * col : *computedColumns())
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

	requestComputedColumnDestruction(fq(_currentlySelectedName));

	setComputeColumnNameSelected("");

	emit refreshData();
}

void ComputedColumnsModel::datasetChanged(	QStringList				changedColumns,
											QStringList				missingColumns,
											QMap<QString, QString>	changeNameColumns,
											bool					rowCountChanged,
											bool					/*hasNewColumns*/)
{
	computedColumns()->findAllColumnNames();

	std::string concatenatedMissings = fq(missingColumns.join(", "));

	for(ComputedColumn * col : *computedColumns())
	{
		bool invalidateMe = rowCountChanged;

		for(const QString & changed : changedColumns)
			if(col->dependsOn(fq(changed), false))
				invalidateMe = true;

		bool containsAChangedName = false;
		for(const auto & changedNames : changeNameColumns.keys())
			if(col->dependsOn(fq(changedNames), false))
				containsAChangedName = true;

		if(containsAChangedName)
		{
			auto stdChangeNameCols = fq(changeNameColumns);
			invalidateMe = true;
			col->setRCode(ColumnEncoder::replaceColumnNamesInRScript(col->rCode(), stdChangeNameCols));
			col->setConstructorJson(JsonUtilities::replaceColumnNamesInDragNDropFilterJSON(col->constructorJson(), stdChangeNameCols));

			if(col->name() == _currentlySelectedName.toStdString())
			{
				emit computeColumnJsonChanged();
				emit computeColumnRCodeChanged();
			}
		}

		if(col->codeType() == ComputedColumn::computedType::constructorCode)
		{
			if(col->setConstructorJson(JsonUtilities::removeColumnsFromDragNDropFilterJSON(col->constructorJson(), fq(missingColumns))))
			{
				//So some column was removed from the json
				invalidateMe = true;

				col->setRCode("stop('Certain columns where removed from the definition of this computed column.\nColumns that could`ve been here are: " + concatenatedMissings + "')");

				if(col->name() == _currentlySelectedName.toStdString())
				{
					emit computeColumnJsonChanged();
					emit computeColumnRCodeChanged();
				}
			}
		}
		else if(col->codeType() == ComputedColumn::computedType::rCode &&
				col->setRCode(ColumnEncoder::removeColumnNamesFromRScript(col->rCode(), fq(missingColumns))))
			{
				invalidateMe = true;

				if(col->name() == _currentlySelectedName.toStdString())
					emit computeColumnRCodeChanged();
			}

		if(invalidateMe)
			invalidate(QString::fromStdString(col->name()));

	}

	computedColumns()->findAllColumnNames();

	for(ComputedColumn * col : *computedColumns())
	{
		col->findDependencies(); //columnNames might have changed right? so check it again

		if(col->iShouldBeSentAgain())
			emitSendComputeCode(QString::fromStdString(col->name()), QString::fromStdString(col->rCodeCommentStripped()), DataSetPackage::pkg()->getColumnType(col->name()));
	}
}


ComputedColumn * ComputedColumnsModel::createComputedColumn(const std::string& name, int colType, ComputedColumn::computedType computeType, Analysis * analysis)
{
	bool success			= false;

	bool	createActualComputedColumn	= computeType != ComputedColumn::computedType::analysisNotComputed,
			showComputedColumn			= createActualComputedColumn && computeType != ComputedColumn::computedType::analysis;

	ComputedColumn  * createdColumn = nullptr;

	if(createActualComputedColumn)
	{
		createdColumn = computedColumns()->createComputedColumn(name, columnType(colType), computeType);
		createdColumn->setAnalysis(analysis);
	}
	else
		computedColumns()->createColumn(name, columnType(colType));

	emit refreshData();

	QString nameQ = tq(name);

	if(createActualComputedColumn)		setLastCreatedColumn(nameQ);
	else								emit dataColumnAdded(nameQ);
	if(showComputedColumn)				setShowThisColumn(nameQ);

	return createdColumn;
}

ComputedColumn *	ComputedColumnsModel::requestComputedColumnCreation(const std::string& columnName, Analysis * analysis)
{
	if(!DataSetPackage::pkg()->isColumnNameFree(columnName))
		return nullptr;

	return createComputedColumn(columnName, int(columnType::scale), ComputedColumn::computedType::analysis, analysis);
}

void ComputedColumnsModel::requestColumnCreation(const std::string& columnName, Analysis * analysis, columnType type)
{
	if(DataSetPackage::pkg()->isColumnNameFree(columnName))
		createComputedColumn(columnName, int(type), ComputedColumn::computedType::analysisNotComputed, analysis);
}


void ComputedColumnsModel::requestComputedColumnDestruction(const std::string& columnName)
{
	if(columnName.empty())
		return;


	int index = DataSetPackage::pkg()->getColumnIndex(columnName);

	computedColumns()->removeComputedColumn(columnName);

	if (DataSetPackage::pkg()->hasDataSet())
		emit headerDataChanged(Qt::Horizontal, index, DataSetPackage::pkg()->columnCount() + 1);

	checkForDependentColumnsToBeSent(columnName);

	QString columnNameQ = tq(columnName);
	if(columnNameQ == lastCreatedColumn())
		setLastCreatedColumn("");

	if(columnNameQ == showThisColumn())
		setShowThisColumn("");
}

bool ComputedColumnsModel::showAnalysisFormForColumn(QString columnName)
{
	try
	{
		ComputedColumn * col = &(*computedColumns())[columnName.toStdString()];

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
	if(computedColumns() == nullptr)
		return;

	std::set<QString> colsToRemove;

	for(auto * col : *computedColumns())
		if(col->analysis() == analysis)
			colsToRemove.insert(QString::fromStdString(col->name()));

	for(const QString & col : colsToRemove)
		requestComputedColumnDestruction(fq(col));
}

void ComputedColumnsModel::setShowThisColumn(QString showThisColumn)
{
	if (_showThisColumn == showThisColumn)
		return;

	_showThisColumn = showThisColumn;
	emit showThisColumnChanged(_showThisColumn);
}
