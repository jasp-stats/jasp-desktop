#include "log.h"
#include <cassert>
#include "filter.h"
#include "timers.h"
#include "qutils.h"
#include "dataset.h"
#include "dataenums.h"
#include "filtereddata.h"
#include "columnencoder.h"
#include "jsonutilities.h"
#include "varinfomodelproxy.h"
#include "databaseinterface.h"
#include "labelfiltergenerator.h"

Filter::Filter(DataSet * data)
: DataSetBaseNode(dataSetBaseNodeType::filter, data),
  VariableInfoProvider(this),
  _data(				data), 
  _name(				DEFAULT_FILTER_NAME),
  _constructorJson(		DEFAULT_FILTER_JSON),
  _generatedFilter(		DEFAULT_FILTER_GEN)
{ 
	assert(_data);
	
	_rFilter			= fq(defaultRFilter());
	_labelGen			= new LabelFilterGenerator(this);
	
	connectionCreation();
}

Filter::Filter(DataSet * data, const std::string & name, bool createIfMissing)
: DataSetBaseNode(dataSetBaseNodeType::filter),
  VariableInfoProvider(this),
  _data(data), _name(name)
{
	assert(_name != "");
	assert(_data);
	
	_rFilter			= fq(defaultRFilter()); //Might get overwritten, that is fine
	_generatedFilter	= DEFAULT_FILTER_GEN;
	
	if(db().filterGetId(_data->id(), _name) > -1)	dbLoad();
	else if(createIfMissing)						dbCreate();
	else											throw std::runtime_error("Filter by name '" + _name + "' but it doesnt exist and createIfMissing=false!\nAre you sure this filter should exist?");
	
	//Named filters intentionally do NOT create a LabelFilterGenerator: it is only needed for the
	//(single, unnamed) default filter, which owns the label-level filtering generated from the
	//label checkboxes. Named filters carry their own rFilter/constructorR instead, so _labelGen
	//stays null for them and setConstructorR() below falls back to DEFAULT_FILTER_GEN.
	//_labelGen			= new LabelFilterGenerator(this);

	connectionCreation();
}


void Filter::connectionCreation()
{
	connect(this,	&Filter::dataSetShouldRefresh,	_data,	&DataSet::refresh			);
	connect(this,	&Filter::refreshAllAnalyses,	_data,	&DataSet::refreshAllAnalyses);
	connect(this,	&Filter::refreshAllCompCols,	_data,	&DataSet::refreshAllCompCols);
	connect(_data,	&DataSet::datasetChanged,		this,	&Filter::datasetChanged		);
	connect(this,	&Filter::nameChanged,			_data,	[&](){_data->incRevision();});
		
	_data->registerFilter(this);
	
		//NOTE: no longer relaying the default filter's generatedFilterChanged to named filters: it
		//fired a signal whose value was unchanged (the named filter's own generatedFilter is separate),
		//so it only caused spurious refreshes, and nothing consumes Filter::generatedFilterChanged.
	
	
	connect(_data,	&DataSet::labelsReordered,				infoSignaller(),	&VarInfoSignaller::labelsReordered			);
	connect(this,	&Filter::modelReset,					infoSignaller(),	&VarInfoSignaller::refresh					);
	
	connect(data(),			&DataSet::columnTypeChanged,				infoSignaller(),	[&](QString name){ Column * col = data() ? data()->column(name) : nullptr; infoSignaller()->variableTypeChanged(name, col ? col->type() : columnType::unknown); });
	connect(data(),			&DataSet::labelChanged,						infoSignaller(),	&VarInfoSignaller::labelChanged			);
	connect(data(),			&DataSet::labelsReordered,					infoSignaller(),	&VarInfoSignaller::labelsReordered		);
	connect(data(),			&DataSet::datasetChanged,					infoSignaller(),	&VarInfoSignaller::dataSetChanged		);
	connect(data(),			&DataSet::emptyValuesChanged,				infoSignaller(),	&VarInfoSignaller::dataSetChanged		);
	connect(data(),			&DataSet::modelReset,						infoSignaller(),	&VarInfoSignaller::refresh				);
	connect(data(),			&DataSet::dataChanged,						infoSignaller(),	&VarInfoSignaller::refresh				);
	

	connect(this,			&Filter::columnsInserted,					varInfo(),			&VariableInfo::rowCountChanged		);
	connect(this,			&Filter::columnsRemoved,					varInfo(),			&VariableInfo::rowCountChanged		);
	
}

void Filter::dbCreate()
{
	assert(_id == -1);
	_id = db().filterInsert(_data->id(), _rFilter, _generatedFilter, _constructorJson, _constructorR, _name);
}

void Filter::dbUpdate(bool writeFiltered)
{
	JASPTIMER_SCOPE(Filter::dbUpdate);

	assert(_id != -1);

	if(!_data->writeBatchedToDB())
	{
		db().transactionWriteBegin();
		db().filterUpdate(_id, _rFilter, _generatedFilter, _constructorJson, _constructorR, _name, _invalidated);

		if(writeFiltered)
			db().filterWrite(_id, _filtered);

		incRevision();
		db().transactionWriteEnd();
	}
}

void Filter::dbUpdateErrorMsg()
{
	assert(_id != -1);
	
	if(!_data->writeBatchedToDB())
	{
		auto oldError = _errorMsg;
		db().transactionWriteBegin();
		db().filterUpdateErrorMsg(_id, _errorMsg);
		if(oldError != _errorMsg)
			emit filterErrorMsgChanged();
		
		incRevision();
		db().transactionWriteEnd();
	}
}

bool Filter::dbLoad()
{
	if(_id == -1)
		_id = _name == "" ? db().filterGetId(_data->id()) : db().filterGetId(_data->id(),_name);

	if(_id == -1)
		return false;

	db().transactionReadBegin();
	
	auto	oldRFilter			= _rFilter,
			oldGeneratedFilter	= _generatedFilter,
			oldConstructorJson	= _constructorJson,
			oldConstructorR		= _constructorR;

	db().filterLoad(_id, _rFilter, _generatedFilter, _constructorJson, _constructorR, _revision, _name, _invalidated);

	rescanForColumns();

	bool changed = false;

	if(oldRFilter			!= _rFilter)		{ emit rFilterChanged();			changed = true; }
	if(oldGeneratedFilter	!= _generatedFilter){ emit generatedFilterChanged();	changed = true; }
	if(oldConstructorJson	!= _constructorJson){ emit constructorJsonChanged();	changed = true; }
	if(oldConstructorR		!= _constructorR)	{ emit constructorRChanged();		changed = true; }
	
	//If the results (or their error) also changed, the filter genuinely changed.
	//Syncing our own cache with a recompute we just triggered ourselves is NOT a
	//change: treat identical results as no-change so callers do not see this as
	//a dataset update (which would re-trigger the filter and loop forever).
	changed = dbLoadResultAndError() || changed;

	db().transactionReadEnd();

	return changed;
}

bool Filter::setFilterVector(const boolvec & filterResult)
{
	bool changed = false;

	//The engine result is authoritative for the whole (current) dataset, so the cached vector must
	//match its length exactly. Only the first call may hit the empty-cache fast path; afterwards we
	//resize to the result length (grow with filtered=false / shrink) instead of stopping at the old
	//size, which previously dropped new rows and kept stale tail rows.
	if(_filtered.size() == 0)
	{
		_filtered = filterResult;
		changed = true;
	}
	else
	{
		if(_filtered.size() != filterResult.size())
		{
			_filtered.resize(filterResult.size());
			changed = true;
		}

		for(size_t i=0; i<filterResult.size(); i++)
			if(_filtered[i] != filterResult[i])
			{
				changed = true;
				_filtered[i] = filterResult[i];
			}
	}

	if(!_data->writeBatchedToDB())
		db().filterWrite(_id, _filtered);

	calculateFilteredRowCount();

	if(changed)
		incRevision();

	return changed;
}

void Filter::setFilterValueNoDB(size_t row, bool val)
{
	_filtered[row] = val;
}

void Filter::setRowCount(size_t rows)
{
	_filtered.resize(rows);
	calculateFilteredRowCount();
}

bool Filter::dbLoadResultAndError()
{
	assert(_id != -1);
	
	std::string newError		= db().filterLoadErrorMsg(_id);
	bool		errorChanged	= newError != _errorMsg;
				_errorMsg		= newError;
	bool		changed			= db().filterSelect(_id, _filtered) || errorChanged;
	
	
	if(errorChanged)
	   emit filterErrorMsgChanged();
	
	if(changed)
		emit filteredChanged();
	 
	 calculateFilteredRowCount();
	 
	 return changed;
}

void Filter::dbDelete()
{
	assert(_id != -1);

	db().filterDelete(_id);
	_id = -1;
}

void Filter::incRevision()
{
	assert(_id != -1);
	
	if(!_data->writeBatchedToDB())
	{
		_revision = db().filterIncRevision(_id);
		checkForChanges();
	}
}

bool Filter::checkForUpdates()
{
	if(_id == -1)
	{
		_id = db().dataSetGetDefaultFilter(_data->id());

		assert(_name == DEFAULT_FILTER_NAME);
		
		if(_id == -1)
			return false;
	}
	else if(_revision >= db().filterGetRevision(_id))
		return false;

	if(_data->id() != -1 && _id != -1)
	{
		//Only report an actual change. Returning true here merely because the engine
		//bumped the revision (e.g. as the direct result of our own filterByName request)
		//used to make DataSet::checkForUpdates emit datasetChanged for every reply,
		//which re-triggered the filter in ListModelFilteredDataEntry and caused an
		//infinite filterByName loop for named filters such as the audit data-entry.
		return dbLoad();
	}
	else
		return false;
}

void Filter::setName(const std::string &name)
{
	//"---" is the separator sentinel used by the filter dropdown lists; a real filter must never take
	//this name or it would be indistinguishable from a separator (and unselectable/ambiguous there).
	if(name == "---")
		return;

	bool	wasChange	=_name != name;
			_name		= name;

	dbUpdate();

	if(wasChange)
		emit nameChanged();
}

void Filter::setRFilter(const std::string &rFilter)
{
	bool	wasChange	=_rFilter != rFilter;
			_rFilter	= rFilter;

	rescanForColumns();

	dbUpdate();

	if(wasChange)
	{
		emit rFilterChanged();
		setInvalidated(true);
	}
}

void Filter::calculateFilteredRowCount()
{
	int newRowCount = 0;
	for(bool f : _filtered)
		if(f)
			newRowCount++;

	bool wasChange = newRowCount != _filteredRowCount;
	_filteredRowCount = newRowCount;

	if(wasChange)
		emit filteredRowCountChanged();
}

void Filter::setGeneratedFilter(const std::string &generatedFilter)
{
	bool	wasChange			=_generatedFilter != generatedFilter;
			_generatedFilter	= generatedFilter;

	dbUpdate();

	if(wasChange)
	{
		setInvalidated(true);
		emit generatedFilterChanged();
	}
}


void Filter::setConstructorJson(const std::string &constructorJson)	
{ 
	bool	wasChange					=_constructorJson != constructorJson;
			_constructorJson			= constructorJson;

	rescanForColumns();

	dbUpdate(); 
	
	

	if(wasChange)
	{
		setInvalidated(true);
		emit constructorJsonChanged();
	}
}

void Filter::setConstructorR(const std::string &constructorR)
{
	bool	wasChange		=_constructorR != constructorR;
			_constructorR	= constructorR;

	if(!_labelGen)
		_generatedFilter = _constructorR == "" ? DEFAULT_FILTER_GEN : "generatedFilter <- " + _constructorR;
			
	dbUpdate();
	
	if(wasChange)
	{
		setInvalidated(true);
		emit constructorRChanged();
	}
	
}

void Filter::setInvalidated(bool invalidated)
{
	bool	wasChange		=_invalidated != invalidated;
			_invalidated	= invalidated;

	dbUpdate();

	if(wasChange)
		emit invalidatedChanged();

	//Re-queue a run whenever a filter is (re)invalidated. Since DataSet::filterByNameDone
	//now clears the flag when a reply for the latest request is consumed, this flag is
	//truthful and this emit is no longer load-bearing-with-weird-semantics: it simply
	//ensures a run gets queued for the current inputs. Gating this on wasChange is
	//tempting but WRONG: an input change while a request is already in flight would be
	//suppressed, and the in-flight reply (computed from the older inputs) would then
	//mark the filter fresh with outdated results. Duplicates are harmless: the newer
	//request computes the newer inputs and EngineRepresentation drops the older reply.
	//The historical infinite loop came from feedback paths that re-invalidated a filter
	//on every reply; those are closed now (Filter::checkForUpdates reports only actual
	//changes, initTableTerms skips no-op rebinds, runFilters is column-aware and the
	//empty-result retry is bounded).
	if(_invalidated)
		emit _data->sendFilterByName(data()->id(), nameQ(), "*");
}

void Filter::setErrorMsg(const std::string &errorMsg)
{
	bool	wasChange	= _errorMsg != errorMsg;
			_errorMsg	= errorMsg;

	dbUpdateErrorMsg();

	if(wasChange)
		emit filterErrorMsgChanged();
}

stringset Filter::columnsUsedInConstructor() const
{
	return _columnsInConstructorJson;
}

stringset Filter::columnsUsedInRFilter() const
{
	return _columnsUsedInRFilter;
}

bool Filter::filterNameIsFree(DataSet * dataSet, const std::string &filterName)
{
	if(!dataSet)
		return true;

	return -1 == DatabaseInterface::singleton()->filterGetId(dataSet->id(), filterName);
}

void Filter::reset()
{
	if(!_data->writeBatchedToDB())
		db().filterClear(_id);

	incRevision();
	_filtered = boolvec(_data->rowCount(), true);
	calculateFilteredRowCount();
}

DatabaseInterface		& Filter::db()			{ return *DatabaseInterface::singleton(); }
const DatabaseInterface & Filter::db() const	{ return *DatabaseInterface::singleton(); }

FilteredData *Filter::rowFilteredData()
{
	if(!_rowFilteredData)
	{
		_rowFilteredData = new FilteredData(this);
		_rowFilteredData->setSourceModel(_data);
	}
	
	return _rowFilteredData;
}

VarInfoModelProxy *Filter::rowFilteredVarInfo()
{
	if(!_rowFilteredVarInfo)
		_rowFilteredVarInfo = new VarInfoModelProxy(rowFilteredData());
	
	return _rowFilteredVarInfo;
}

FilteredData *Filter::rowFilteredData() const
{
	return _rowFilteredData;
}

VariableInfo *Filter::varInfo() const
{
	return _varInfo;
}

VariableInfo *Filter::varInfo()
{
	if(!_varInfo)
		_varInfo = new VariableInfo(this);
	
	return _varInfo;
}

VarInfoModelProxy *Filter::rowFilteredVarInfo() const
{
	return _rowFilteredVarInfo;
}

QAbstractItemModel *Filter::providerModel()
{
	return rowFilteredVarInfo();
}



QVariant Filter::provideInfo(varInfoType info, const QString& colName, int row) const
{
	try
	{
		switch(info)
		{
		case varInfoType::VariableNames:			return	tq(data()->getColumnNames());
		case varInfoType::DataSetRowCount:			return  rowFilteredData()->rowCount();
		case varInfoType::DataAvailable:			return	bool(data());
		case varInfoType::DataSetPointer:			return	QVariant::fromValue<void*>(data());
		default:									break;
		}

		int colIndex = data()->getColumnIndex(fq(colName));
		if (colIndex < 0)
			return QVariant();

		QModelIndex qColIndex	= index(colIndex, 0),
					tableCIndex	= rowFilteredData()->index(0, colIndex),
					tableVIndex	= rowFilteredData()->index(row, colIndex);

		switch(info)
		{
		case varInfoType::VariableType:				return	rowFilteredVarInfo()	->data(qColIndex, VarInfoModelProxy::ColumnTypeRole).toInt();
		case varInfoType::NameRole:					return	rowFilteredVarInfo()	->data(qColIndex, VarInfoModelProxy::NameRole);

		case varInfoType::DoubleValues:				return	rowFilteredData()		->	data(tableCIndex,						int(dataPkgRoles::valuesDblList));
		case varInfoType::TotalNumericValues:		return	rowFilteredData()		->	data(tableCIndex,						int(dataPkgRoles::nonFilteredNumericValuesCount));
		case varInfoType::TotalLevels:				return	rowFilteredData()		->	data(tableCIndex,						int(dataPkgRoles::nonFilteredLevels)).toStringList().length();
		case varInfoType::Labels:					return	rowFilteredData()		->	data(tableCIndex,						int(dataPkgRoles::nonFilteredLevels));
		case varInfoType::DataSetValues:			return	rowFilteredData()		->	data(tableCIndex,						int(dataPkgRoles::valuesStrList));
		case varInfoType::DataSetValue:				return	rowFilteredData()		->	data(tableVIndex,						int(dataPkgRoles::value));

		case varInfoType::MaxWidth:					return	rowFilteredData()		->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::maxColString)).toInt();
		case varInfoType::PreviewScale:				return	rowFilteredData()		->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewScale));
		case varInfoType::PreviewOrdinal:			return	rowFilteredData()		->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewOrdinal));
		case varInfoType::PreviewNominal:			return	rowFilteredData()		->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::previewNominal));
		case varInfoType::ColumnDescription:		return	rowFilteredData()		->headerData(colIndex, Qt::Horizontal,	int(dataPkgRoles::description));
		case varInfoType::SignalsBlocked:			throw std::runtime_error("????");
		default:									break;
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return QVariant();
}

bool Filter::absorbInfo(varInfoType info, const QString &colName, int row, QVariant value)
{
	try
	{
		int colIndex = data()->getColumnIndex(fq(colName));

		if (colIndex < 0)
			return false;

		QModelIndex qColIndex	= rowFilteredData()->index(0, colIndex),
					qValIndex	= rowFilteredData()->index(row, colIndex);

		switch(info)
		{
		default:										return	false;
		case varInfoType::DataSetValue:					return	rowFilteredData()->setData(qValIndex, value,	int(dataPkgRoles::value));
		case varInfoType::DataSetValues:				return	rowFilteredData()->setData(qColIndex, value,	int(dataPkgRoles::valuesStrList));
		}
	}
	catch(std::exception & e)
	{
		Log::log() << "AnalysisForm::requestInfo had an exception! " << e.what() << std::flush;
		throw e;
	}

	return false;
}

void Filter::rescanForColumns()
{
	_columnsUsedInRFilter		= data()->findUsedColumnNames(_rFilter);
	_columnsInConstructorJson	= JsonUtilities::convertDragNDropFilterJSONToSet(_constructorJson);
}

void Filter::datasetChanged(int, QStringList changedColumns, QStringList missingColumns, QMap<QString, QString> changeNameColumns, bool rowCountChanged, bool hasNewColumns)
{
	bool invalidateMe = rowCountChanged;

	if(!invalidateMe)
		for(const QString & changed : changedColumns)
			if(_columnsUsedInRFilter.count(fq(changed)) > 0 || _columnsInConstructorJson.count(fq(changed)) > 0)
			{
				invalidateMe = true;
				break;
			}

	auto iUseOneOfTheseColumns = [&](std::vector<std::string> cols) -> bool
	{
		for(const std::string & col : cols)
			if(_columnsUsedInRFilter.count(col) > 0 || _columnsInConstructorJson.count(col) > 0)
				return true;

		return false;
	};

	if(iUseOneOfTheseColumns(fq(changeNameColumns.keys())))
	{
		std::map<std::string, std::string> stdChangeNameCols(fq(changeNameColumns));

		invalidateMe = true;

		setRFilter(			ColumnEncoder::replaceColumnNamesInRScript(rFilter(),							stdChangeNameCols));
		setConstructorJson( JsonUtilities::replaceColumnNamesInDragNDropFilterJSONStr(constructorJson(),		stdChangeNameCols));
	}

	auto missingStd = fq(missingColumns);
	if(iUseOneOfTheseColumns(missingStd))
	{
		setRFilter(ColumnEncoder::removeColumnNamesFromRScript(rFilter(), missingStd));

		setConstructorJson( JsonUtilities::removeColumnsFromDragNDropFilterJSONStr( constructorJson(), missingStd));

		invalidateMe = false; //Actually, if stuff is removed from the filter it won't work will it now?

		//Just reset the filter result to everything true while the user gets the change to fix their now broken filter
		reset();

		emit refreshAllAnalyses(this);
		data()->resetFilterCounters();
		updateStatusBar();

		//The following errormsg is overwritten immediately but that is because constructorJson changed triggers qml which triggers (some vents later) a send event. So yeah...
		//Ill leave it here though because it would be nice to show this friendlier msg then "null not found"
		setFilterErrorMsgQ(tr("Some columns were removed from the data and your filter(s)!"));
	}

	if(invalidateMe)
	{
		//Keep the cached filter vector length in sync with the dataset: on a row-count change the
		//engine result is (re)computed asynchronously, so until it lands we must not serve a vector
		//that is shorter than the dataset (drops rows) or longer (reads stale tail rows).
		if(rowCountChanged)
		{
			_filtered.resize(_data->rowCount());
			calculateFilteredRowCount();
		}

		setInvalidated(true);
	}


	//Do stuff for variable info provider:
	
   if(! (missingColumns.size() > 0 || hasNewColumns))
   {
		   if (changeNameColumns.size() > 0)
				   emit infoSignaller()->variableNamesChanged(changeNameColumns);
		   else if (changedColumns.size() > 0 || rowCountChanged)
		   {
				   if (rowCountChanged)
				   {
						   changedColumns.clear();
						   for (int i = 0; i < rowFilteredVarInfo()->rowCount(); i++)
								   changedColumns.push_back(rowFilteredVarInfo()->data(index(i, 0), VarInfoModelProxy::NameRole).toString());
				   }
				   emit infoSignaller()->variablesChanged(changedColumns);
		   }
   }
		
}

int Filter::rowCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : filtered().size();
}

int Filter::columnCount(const QModelIndex &parent) const
{
	return parent.isValid() ? 0 : 1;
}

QVariant Filter::data(const QModelIndex &index, int role) const
{
	if(!index.isValid())
		return QVariant();
	
	
	if(index.row() >= rowCount() || index.column() >= columnCount())
		return QVariant(); // if there is no data then it doesn't matter what role we play
	
	switch(role)
	{
	case Qt::DisplayRole:									
	case int(dataPkgRoles::filter):							return QVariant(filtered()[index.row()]);
	}
	
	return QVariant();
}

const std::string &Filter::generatedFilter() const 
{ 
	return _generatedFilter;
}

QString Filter::constructorRQ() const
{
	return tq(constructorR());
}

QString Filter::rFilterQ() const
{
	return tq(rFilter());
}


QString Filter::nameQ() const
{
	return tq(name());
}

QString Filter::filterErrorMsgQ() const
{
	return tq(errorMsg());
}

QString Filter::generatedFilterQ() const
{
	return tq(generatedFilter());
}

QString Filter::constructorJsonQ() const
{
	return tq(constructorJson());
}

bool Filter::columnUsed(const QString &name) const
{
	return _columnsInConstructorJson.count(fq(name)) || _columnsUsedInRFilter.count(fq(name));
}

const QString & Filter::defaultRFilter()
{
	static QString defaultFilter;

	const QString forceTranslatedStuffToAlwaysBeAComment =
		tr(
			"Above you see the code that JASP generates for both value filtering and the drag&drop filter."					"\n"
			"This default result is stored in 'generatedFilter' and can be replaced or combined with a custom filter."		"\n"
			"To combine you can append clauses using '&': 'generatedFilter & customFilter & perhapsAnotherFilter'"			"\n"
			"Click the (i) icon in the lower right corner for further help."												"\n");

	defaultFilter = "# " + tq(stringUtils::replaceBy(fq(forceTranslatedStuffToAlwaysBeAComment), "\n", "\n# ") + "\n\ngeneratedFilter");

	return defaultFilter;
}

bool Filter::hasFilter() const
{
	return rFilter() != defaultRFilter() || constructorJson() != DEFAULT_FILTER_JSON; 
}

void Filter::setRFilterQ(const QString &newRFilter) 
{
	setRFilter(			fq(newRFilter));			
}

void Filter::setConstructorRQ(const QString &newConstructorR) 
{ 
	setConstructorR(	fq(newConstructorR));	
}

void Filter::setGeneratedFilterQ(const QString &newGeneratedFilter) 
{ 
	setGeneratedFilter(	fq(newGeneratedFilter));	
}

void Filter::setConstructorJsonQ(const QString &newconstructorJson) 
{ 
	setConstructorJson(		fq(newconstructorJson));	
}

void Filter::setFilterErrorMsgQ(const QString &newFilterErrorMsg) 
{ 
	setErrorMsg(fq(newFilterErrorMsg));		
}

void Filter::setStatusBarText(const QString &newStatusBarText)
{
	_statusBarText  = newStatusBarText;
}

void Filter::checkFilterResults()
{
	//Load new filter values from database
	if(dbLoadResultAndError())
	{
		emit filterErrorMsgChanged();
		emit refreshAllAnalyses(this);
		emit refreshAllCompCols(this);
		data()->resetFilterCounters(); //Should really be part of filter
		updateStatusBar();
		emit dataSetShouldRefresh();
	}
}
