#include <QCoreApplication>
#include <QSet>
#include "workspace.h"
#include "qutils.h"
#include "log.h"
#include "undostack.h"

Workspace * Workspace::_singleton = nullptr;

Workspace::Workspace(QObject *parent)
	: DataSetBaseNode{dataSetBaseNodeType::workspace, parent},
	  _varInfo(new VariableInfo(nullptr, this))
{
	assert(!_singleton);
	_singleton = this;

	//The input-filter list (for the shown computed dataset) depends on the set of datasets, their
	//filters, and which dataset is shown; forward all of those so QML's `values` binding stays reactive.
	connect(this, &Workspace::dataSetCreated,			this, &Workspace::inputFilterDropDownListChanged);
	connect(this, &Workspace::dataSetRemoved,			this, &Workspace::inputFilterDropDownListChanged);
	connect(this, &Workspace::shownDataSetChanged,		this, &Workspace::inputFilterDropDownListChanged);
	connect(this, &Workspace::filtersCountChanged,		this, &Workspace::inputFilterDropDownListChanged);

	//Same reactivity for the dataset/filter selection list, plus dataset renames because they change its labels.
	connect(this, &Workspace::dataSetCreated,			this, &Workspace::dataSetFilterDropDownListChanged);
	connect(this, &Workspace::dataSetRemoved,			this, &Workspace::dataSetFilterDropDownListChanged);
	connect(this, &Workspace::dataSetTitleChanged,		this, &Workspace::dataSetFilterDropDownListChanged);
	connect(this, &Workspace::filtersCountChanged,		this, &Workspace::dataSetFilterDropDownListChanged);
}

Workspace::~Workspace()
{
	assert(_singleton == this);
	
	for(auto & idData : _dataSets)
		unregisterNode(idData.second);
	
	_dataSets.clear();
	
	_singleton = nullptr;
}


DatabaseInterface &Workspace::db()	
{ 
	return *DatabaseInterface::singleton(); 
}

const DatabaseInterface &Workspace::db() const
{ 
	return *DatabaseInterface::singleton(); 
}

QVariant Workspace::data(const QModelIndex &index, int role) const
{
	if(!index.isValid())
		return QVariant();
	
	if(index.row() >= rowCount() || index.column() >= columnCount())
		return QVariant(); // if there is no data then it doesn't matter what role we play
	
	DataSets sets = dataSets();
	DataSet * cur = sets[index.row()];

	switch(role)
	{
	case Qt::DisplayRole:									
	case int(dataPkgRoles::label):							 
	case int(dataPkgRoles::value):							return cur->descriptionQ();
	case int(dataPkgRoles::name):							return cur->name();//tq(cur->db().dataSetName(cur->id()));
	case int(dataPkgRoles::title):							return cur->title();
	case int(dataPkgRoles::description):					return cur->descriptionQ();
	case int(dataPkgRoles::id):								return cur->id();
	case int(dataPkgRoles::computedColumnType):				return int(cur->codeType());
	case int(dataPkgRoles::columnIsComputed):				return cur->isComputed();
	}
	
	return QVariant();
}

void Workspace::setDataMode(bool mode)			
{
	if(_dataMode == mode)
		return;
	
	_dataMode = mode; 
	emit dataModeChanged(_dataMode);
	refresh();
}

void Workspace::setShowRSyntax(bool showRSyntax)					
{ 
	_showRSyntax		= showRSyntax;			
	dbUpdate();
	
	emit showRSyntaxChanged(_showRSyntax);
}

void Workspace::dbLoad(std::function<void(float)> progressCallback, Version doUpgradeFrom)
{
	intset	dataSets = db().dataSetIds();
	
	int		numLoaded = 0;
	
	for(int id : dataSets)
	{
		auto progressCallbackPerData = [&](float p)
		{
			float	d = dataSets.size(),
					i = 1.0 / d;
			
			progressCallback((numLoaded * i) + (p * i));	
		};
		
		
		_dataSets[id] = new DataSet(this, 0);
		_dataSets[id]->dbLoad(id, progressCallbackPerData, doUpgradeFrom);
		numLoaded++;
		emit dataSetCreated(id);
		
		if(!_shownDataSet)
			setShownDataSet(_dataSets[id]);
	}
	
	bool prev = _showRSyntax;
	db().workspaceLoad(_showRSyntax);
	
	if(prev != _showRSyntax)
		emit showRSyntaxChanged(_showRSyntax);
}

void Workspace::dbUpdate()
{
	db().workspaceUpdate(_showRSyntax);
}

void Workspace::dbDelete()
{
	for(auto & idData : _dataSets)
		idData.second->dbDelete();
	
	db().truncateAllTables();
}

//Should be merged with dbLoad() probably?
bool Workspace::checkForUpdates(std::function<void(float)> progressCallback)
{
	intset	dataSets = db().dataSetIds(),
			missing;

	bool aChange = false;
	
	int numChecked = 0;
	float d = std::max(1, (int)dataSets.size());

	for(int id : dataSets)
		if(_dataSets.count(id))
		{
			if(_dataSets.at(id)->checkForUpdates([&](float p){ progressCallback((numChecked + p) / d); }))
				aChange = true;
			numChecked++;
		}
		else
		{
			_dataSets[id] = new DataSet(this, id);
			aChange = true;
			emit dataSetCreated(id);
			
			if(!_shownDataSet)
				setShownDataSet(_dataSets[id]); //Full setter so encoder/undoStack/varInfo stay consistent
			numChecked++;
			progressCallback(numChecked / d);
		}
	
	for(auto & idDataSet : _dataSets)
		if(!dataSets.count(idDataSet.first))
			missing.insert(idDataSet.first);
	
	for(int id : missing)
	{
		if(_shownDataSet == _dataSets[id])
			_shownDataSet = nullptr;
		delete _dataSets[id];
		_dataSets.erase(id);
		emit dataSetRemoved(id);
		aChange = true;
	}
	
	//Never leave even a dangling pointer to a deleted (previously shown) dataset.
	if(!_shownDataSet)
	{
		if(_dataSets.empty())
			_varInfo->setProvider(nullptr);
		else
			setShownDataSet(_dataSets.begin()->second);
	}
	
	bool prev = _showRSyntax;
	db().workspaceLoad(_showRSyntax);
	
	if(prev != _showRSyntax)
		emit showRSyntaxChanged(_showRSyntax);
	
	return aChange;
}


DataSet *Workspace::shownDataSet() const
{
	return _shownDataSet;
}

void Workspace::setShownDataSet(DataSet *dataSet)
{
	if(_shownDataSet == dataSet)
		return;
	
	assert(dataSet->workspace() == this);
	
	disconnect(_shownDataSet, &DataSet::shownColumnChanged, this, &Workspace::shownColumnChanged);
	disconnect(_shownDataSet, &DataSet::shownFilterChanged, this, &Workspace::shownFilterChanged);
	
	_shownDataSet = dataSet;
	
	UndoStack::setCurrent(_shownDataSet->undoStack());

	//Column-name encoding/decoding (and computed-column dependency resolution) on the desktop must
	//reflect the shown dataset. Consumers obtain the dataset's own encoder via the provider
	//(provider->columnEncoder()); we only (re)populate its name map here, and never touch the
	//process-global current encoder (that global is only meaningful inside the engine's request context).
	_shownDataSet->encoder().setCurrentNames(_shownDataSet->getColumnTypesMap());
	
	connect(_shownDataSet, &DataSet::shownColumnChanged, this, &Workspace::shownColumnChanged, Qt::UniqueConnection);
	connect(_shownDataSet, &DataSet::shownFilterChanged, this, &Workspace::shownFilterChanged, Qt::UniqueConnection);
	
	_varInfo->setProvider(_shownDataSet->shownFilter());
			
	emit shownDataSetChanged(_shownDataSet);
	
	refresh();
}

void Workspace::setShownDataSet(int dataSetId)
{
	if(_dataSets.count(dataSetId))
		setShownDataSet(_dataSets.at(dataSetId));
	else
		Log::log() << "setShownDataSet(" << dataSetId << ") can't find the dataSet!" << std::endl;
}

void Workspace::deleteShownDataSet()
{
	if(!_shownDataSet)
		return;

	const int deletedId = _shownDataSet->id();

	//Computed datasets that used a filter of the deleted dataset as their input would otherwise keep a
	//dangling defaultInputFilterId and attempt to run against a dataset that no longer exists. Clear the
	//input and surface an error so the user knows why the computed dataset can no longer be produced
	//instead of silently recomputing against the dataset's own (empty) data.
	for (const auto & idDataSet : _dataSets)
	{
		DataSet * ds = idDataSet.second;
		if (ds != _shownDataSet && ds->isComputed() && ds->defaultInputDataSet() && ds->defaultInputDataSet()->id() == deletedId)
		{
			ds->setDefaultInputFilterId(-1);
			ds->setError("The filter used as input for this computed dataset belonged to a dataset that was removed.");
		}
	}

	_dataSets.erase(deletedId);
	emit dataSetRemoved(deletedId);
	_shownDataSet->dbDelete();
	UndoStack::setCurrent(nullptr);
	delete _shownDataSet;
		
	_shownDataSet = nullptr;
	
	DataSet * newShown = nullptr;
	
	for(auto & idDataSet : _dataSets)
	{
		newShown = idDataSet.second;
		break;
	}
	
	if(newShown)	setShownDataSet(newShown);
	else
	{
		_varInfo->setProvider(nullptr); //No dataset left: don't leave the provider pointing at a destroyed filter
		refresh();
	}
}

void Workspace::showFilter(int id)
{
	Filter * f = filterById(id);
	
	if(f)
	{
		setShownDataSet(f->data());
		f->data()->showFilter(f);
		_varInfo->setProvider(f);
		refresh();
	}
}

void Workspace::onShownFilterChanged(DataSet *dataSet)
{
	setShownDataSet(dataSet);
	emit shownFilterChanged();
}

void Workspace::refreshAllCompCols(Filter *f)
{
	assert(f);
	
	f->data()->invalidateAllComputedColumns();
}

void Workspace::setShownDataSet(QString name)
{
	for(auto & idData : _dataSets)
		if(idData.second->name() == name)
		{
			setShownDataSet(idData.second);
			return;
		}
}

DataSets Workspace::dataSets() const
{
	DataSets out;
	
	for(auto & idData : _dataSets)
		out.push_back(idData.second);
	
	return out;
}

DataSet *Workspace::dataSetById(int id) const
{
	if(_dataSets.count(id))
		return _dataSets.at(id);
	
	return nullptr;
}

DataSet *Workspace::dataSetByName(const std::string & name) const
{
	for(auto & idData : _dataSets)
		if(idData.second->name().toStdString() == name)
			return idData.second;

	return nullptr;
}

QString Workspace::makeDataSetTitleUnique(const QString & title, DataSet * exclude) const
{
	QSet<QString> takenTitles;
	for(const auto & idData : _dataSets)
		if(idData.second != exclude)
			takenTitles.insert(idData.second->title());

	if(!takenTitles.contains(title))
		return title;

	int suffix = 2;
	QString candidate;
	do
		candidate = title + " (" + QString::number(suffix++) + ")";
	while(takenTitles.contains(candidate));

	return candidate;
}

Filter *Workspace::filterById(int id) const
{
	for(auto & idDataSet : _dataSets)
		if(idDataSet.second->filter(id))
			return idDataSet.second->filter(id);
	return nullptr;
}

Column *Workspace::shownColumn() const
{
	return shownDataSet() ? shownDataSet()->shownColumn() : nullptr;
}

Filter *Workspace::shownFilter() const
{
	return shownDataSet() ? shownDataSet()->shownFilter()	: nullptr;
}

void Workspace::setShownColumn(Column *newShownColumn)
{
	newShownColumn->data()->setShownColumn(newShownColumn); // Will also set shownDataSet en passant
}

void Workspace::setShownFilter(Filter *newShownFilter)
{
	newShownFilter->data()->showFilter(newShownFilter);
	setShownDataSet(newShownFilter->data());
}


DataSet * Workspace::createDataSet()
{
	bool shownDataSetExistsAndIsEmpty = 
			_shownDataSet && 
			(_shownDataSet->columnCount() == 0 // Simple case
			|| (_shownDataSet->columnCount() == 1 && _shownDataSet->rowCount() == 1 && _shownDataSet->data(_shownDataSet->index(0, 0)) == QVariant())); //Single empty cell
	
	if(shownDataSetExistsAndIsEmpty)
		return _shownDataSet;

	DataSet * newSet = new DataSet(this);

	if(!_shownDataSet)
		setShownDataSet(newSet);

	_dataSets[newSet->id()] = newSet;

	emit dataSetCreated(newSet->id());
	emit filtersCountChanged(); //Triggers filterDropDownListChanged in filtermodel

	return newSet;
}

Column *Workspace::createComputedColumn(const std::string &name, int dataSetId, int analysisId, columnType type, computedColumnType desiredType)
{
	if(_dataSets.count(dataSetId))
		return _dataSets.at(dataSetId)->createComputedColumn(name, type, desiredType, analysisId);
	
	return nullptr;
}

DataSet *Workspace::createComputedDataSet(const std::string &name, int defaultInputFilterId, computedColumnType desiredType)
{
	DataSet * newSet = createDataSet();

	if(!newSet)
		return nullptr;

	newSet->setTitle(tq(name));
	newSet->setCodeType(desiredType);
	newSet->setDefaultInputFilterId(defaultInputFilterId);
	newSet->invalidate();

	setShownDataSet(newSet);

	return newSet;
}

QStringList Workspace::dataSetNames() const
{
	QStringList names;

	for(const auto & idData : _dataSets)
		names.push_back(idData.second->name());

	return names;
}

QVariantList Workspace::inputFilterDropDownList() const
{
	//The filters available as *input* for the currently-shown computed dataset: every dataset's
	//filters except the shown dataset's own. A computed dataset must not read from its own output,
	//and setDefaultInputFilterId would refuse it as a loop anyway, so hide it here too.
	return _filterDropDownList(_shownDataSet ? _shownDataSet->id() : -1);
}

QVariantList Workspace::dataSetFilterDropDownList() const
{
	//Every filter of every dataset, for the multi-dataset aware selection: choosing a dataset means
	//choosing one of its filters, so these are the entries the user picks between.
	return _filterDropDownList(-1);
}

QVariantList Workspace::_filterDropDownList(int excludeDataSetId) const
{
	typedef QMap<QString, QVariant> localMap;

	QVariantList out;

	for(const auto & idData : _dataSets)
	{
		DataSet * dataSet = idData.second;

		if(dataSet->id() == excludeDataSetId)
			continue;

		//out.append(localMap{std::make_pair("value", tq("-")), std::make_pair("label", dataSet->title() + ":")});

		if(dataSet->defaultFilter())
			out.append(localMap{std::make_pair("value", tq(std::to_string(dataSet->defaultFilter()->id()))), std::make_pair("label", dataSet->title() + " - " + dataSet->defaultFilter()->title())});

		for(const Filter * f : dataSet->filters())
			if(f != dataSet->defaultFilter())
				out.append(localMap{std::make_pair("value", tq(std::to_string(f->id()))), std::make_pair("label", dataSet->title() + " - " + f->title())});
	}

	return out;
}

bool Workspace::wouldCreateComputedDataSetLoop(DataSet * me, DataSet * target) const
{
	std::set<int> visited;
	DataSet * cur = target;
	while (cur)
	{
		if (cur == me)
			return true;

		if (!visited.insert(cur->id()).second)
			return false; //Reached a node we have seen; not a loop involving me.

		if (!cur->isComputed() || !cur->defaultInputDataSet())
			return false;

		cur = cur->defaultInputDataSet();
	}

	return false;
}

bool Workspace::computedDataSetsHaveLoop(std::string & errorMessage) const
{
	std::set<int> globalVisited;

	for (const auto & idData : _dataSets)
	{
		DataSet * ds = idData.second;
		if (!ds->isComputed() || globalVisited.count(ds->id()))
			continue;

		std::set<int> chain;
		DataSet * cur = ds;

		while (cur && cur->isComputed())
		{
			if (!chain.insert(cur->id()).second)
			{
				errorMessage = "A loop was found between your computed datasets and their input datasets. Change one of the input selections to break the circle.";
				return true;
			}

			if (!cur->defaultInputDataSet())
				break;

			cur = cur->defaultInputDataSet();
		}

		globalVisited.insert(chain.begin(), chain.end());
	}

	return false;
}

void Workspace::setDataSetComputed(const QString & name, bool computed)
{
	DataSet * ds = dataSetByName(fq(name));
	if(!ds || computed == ds->isComputed())
		return;

	if(ds != shownDataSet())
		setShownDataSet(ds);

	if(computed)
	{
		ds->setCodeType(computedColumnType::rCode);

		if(!ds->defaultInputDataSet())
		{
			//Pick the first other dataset whose default filter does not close a loop (e.g. another
			//computed dataset that (in)directly depends on this one must not be chosen, or we would
			//create A <- B <- A).
			DataSet * candidate = nullptr;
			for(const auto & idData : _dataSets)
				if(idData.second != ds && !wouldCreateComputedDataSetLoop(ds, idData.second))
				{
					candidate = idData.second;
					break;
				}

			if(candidate && candidate->defaultFilter())
				ds->setDefaultInputFilterId(candidate->defaultFilter()->id());
		}

		if(ds->defaultInputDataSet() && wouldCreateComputedDataSetLoop(ds, ds->defaultInputDataSet()))
			ds->setError("The filter chosen as input for this computed dataset would create a loop between the computed datasets.");
	}
	else
		ds->setCodeType(computedColumnType::notComputed);

	DataSets sets = dataSets();

	for(int i = 0; i < sets.size(); ++i)
		if(sets[i] == ds)
		{
			emit dataChanged(index(i, 0), index(i, 0), { int(dataPkgRoles::columnIsComputed), int(dataPkgRoles::computedColumnType), int(dataPkgRoles::id) });
			break;
		}
}

void Workspace::refresh()
{
	//Skip nested/re-entrant refreshes entirely (e.g. a dataset refresh emitting a signal that
	//triggers Workspace::refresh again): doing beginResetModel/endResetModel while a reset is
	//already in progress is undefined behaviour in Qt.
	//RAII guard (unlike a plain counter) so the flag is cleared even if a signal handler throws.
	struct RefreshGuard
	{
		bool &	_inRefresh;
		explicit RefreshGuard(bool & inRefresh) : _inRefresh(inRefresh) { _inRefresh = true; }
		~RefreshGuard()                             { _inRefresh = false; }
	};

	//instance flag (see _inRefresh in workspace.h), not static, so it cannot suppress refreshes
	//across separate Workspace instances.
	if (!_inRefresh)
	{

		RefreshGuard guard(_inRefresh);
		beginResetModel();
	
		for(auto & idData : _dataSets)
			idData.second->refresh();
	
		emit dataModeChanged(dataMode());
		emit showRSyntaxChanged(showRSyntax());
		endResetModel();
	}

	//Emit the "shown" signals only after the reset is complete: these connect into QML/other models
	//that may re-query the Workspace model, which is not allowed while a reset is still active.
	emit shownDataSetChanged(shownDataSet());
	emit shownColumnChanged();
	emit shownFilterChanged();
}


void Workspace::initializeComputedColumns()
{
	for(auto & idDataSet : _dataSets)
		for(Column * col : idDataSet.second->columns())
			col->checkForDependentColumnsToBeSent();
}

void Workspace::initializeComputedDatasets()
{
	for(auto & idDataSet : _dataSets)
		if(idDataSet.second->isComputed() && idDataSet.second->iShouldBeSentAgain())
			idDataSet.second->tryAndRunComputedDataset();
}

void Workspace::computedDataSetSucceeded(int dataSetId, QString warning, bool dataChanged)
{
	DataSet * dataSet = dataSetById(dataSetId);

	if(!dataSet)
		return;

	dataSet->checkForUpdates();
	dataSet->setError(warning.isEmpty() ? std::string() : fq(warning));

	//A failed computation leaves the dataset invalidated so it stays marked as needing a (re)run;
	//only a successful computation validates it and lets the datasets depending on it proceed.
	if(!warning.isEmpty())
		return;

	//Never cascade into a cycle (A <- B <- A): if the computed-dataset graph has a loop, do not keep
	//recomputing; surface the error and leave the datasets invalidated so the user fixes the inputs.
	std::string loopError;
	if (computedDataSetsHaveLoop(loopError))
	{
		for (const auto & idData : _dataSets)
			if (idData.second->isComputed() && idData.second->invalidated())
				idData.second->setError(loopError);
		return;
	}

	dataSet->validate();
	dataSet->checkForDependentDatasetsToBeSent();
}

void Workspace::updateComputedColumnDependenciesForAnalysis(int analysisId, const stringset & usedVariables)
{
	for(DataSet * dataSet : dataSets())
		for(Column * col : dataSet->columns())
			if(col->isComputedByAnalysis(analysisId))
				col->setDependsOn(usedVariables);
}

void Workspace::computedColumnSucceeded(int dataSetId, QString columnNameQ, QString warning, bool dataChanged)
{
	DataSet * dataSet = dataSetById(dataSetId);

	if(!dataSet)
		return;

	std::string	columnName	= fq(columnNameQ);
	//The engine may report the encoded name; translate it defensively so the lookup works either way.
	try { columnName = dataSet->encoder().decode(columnName); } catch(...) {}
	Column	*	column		= dataSet->column(columnName);

	if(!column)
		return;

	//The engine wrote the freshly computed values to the shared database, so pick those up.
	column->checkForUpdates();
	column->setError(warning.isEmpty() ? std::string() : fq(warning));

	//A failed computation leaves the column invalidated so it stays marked as needing a (re)run;
	//only a successful computation validates the column and lets the columns depending on it proceed.
	if(!warning.isEmpty())
		return;

	column->validate();
	column->checkForDependentColumnsToBeSent();

	//Any filter that uses this computed column needs to be recomputed as well, otherwise it will
	//silently keep the (now outdated) result for the whole dataset it belongs to.
	for(Filter * f : dataSet->filters())
		if(f->columnUsed(columnNameQ))
			f->setInvalidated(true);
}
