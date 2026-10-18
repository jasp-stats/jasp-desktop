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
	
	if(_shownDataSet)
	{
		disconnect(_shownDataSet, &DataSet::shownColumnChanged, this, &Workspace::shownColumnChanged);
		disconnect(_shownDataSet, &DataSet::shownFilterChanged, this, &Workspace::shownFilterChanged);

		//Unhook the shown-dataset relays (see the signals block in workspace.h); a dataset that is
		//deleted outright disconnects these itself, but switching to another one has to clean up here.
		disconnect(_shownDataSet,					&DataSet::dataFileSynchChanged,	this,	&Workspace::dataFileSynchChanged);
		disconnect(_shownDataSet,					&DataSet::dataFileChanged,		this,	&Workspace::dataFileChanged);
		disconnect(_shownDataSet,					&DataSet::manualEditsChanged,	this,	&Workspace::manualEditsChanged);
	}
	
	_shownDataSet = dataSet;
	
	UndoStack::setCurrent(_shownDataSet->undoStack());

	//Column-name encoding/decoding (and computed-column dependency resolution) on the desktop must
	//reflect the shown dataset. Consumers obtain a filter's namespace view via the provider
	//(provider->columnEncoder()); we only (re)mint the shown dataset's canonical here - the blocks of
	//all its live filter views follow automatically (mds-plan D1.4) - and never touch the
	//process-global current encoder (that global is only meaningful inside the engine's request context).
	_shownDataSet->encoder().setCurrentNames(_shownDataSet->getColumnTypesMap());
	
	connect(_shownDataSet, &DataSet::shownColumnChanged, this, &Workspace::shownColumnChanged, Qt::UniqueConnection);
	connect(_shownDataSet, &DataSet::shownFilterChanged, this, &Workspace::shownFilterChanged, Qt::UniqueConnection);

	//And hook them up again for the new one, so DataSet & friends on the desktop side only have to
	//connect to the Workspace once (see DataSetPackage::connectWorkspace).
	connect(_shownDataSet,					&DataSet::dataFileSynchChanged,	this,	&Workspace::dataFileSynchChanged,	Qt::UniqueConnection);
	connect(_shownDataSet,					&DataSet::dataFileChanged,		this,	&Workspace::dataFileChanged,		Qt::UniqueConnection);
	connect(_shownDataSet,					&DataSet::manualEditsChanged,	this,	&Workspace::manualEditsChanged,		Qt::UniqueConnection);
	
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

DataSet *Workspace::dataSetByTitle(const QString & title) const
{
	//Exact match on the user facing title (DataSet::setTitle keeps those unique); used by the
	//syntax-mode dataset selection, where the user refers to datasets by name.
	for(auto & idData : _dataSets)
		if(idData.second->title() == title)
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

Filter *Workspace::filterById(int id)
{
	for(auto & idDataSet : _dataSets)
		if(idDataSet.second->filter(id))
			return idDataSet.second->filter(id);

	//A reopened .jasp materialises only default filters: a named filter nobody has selected yet
	//lives on purely as a db row. Attach to that row instead of resolving to nothing - saved
	//provenance and the slice queue point at these ids, and the row's id is the invariant.
	const int         dataSetId = db().filterGetDataSetId(id);
	DataSet         * dataSet   = dataSetById(dataSetId);
	const std::string name      = dataSet ? db().filterGetName(id) : std::string();

	if(dataSet && !name.empty() && db().filterGetId(dataSetId, name) == id)
	{
		Filter * f = dataSet->loadStoredFilter(name);	//dbLoad by name keeps the row id
		if(f && f->id() == id)
			return f;
	}

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

// --- mds-plan C4/RESOLVER: the dataset-selection syntax surface, see workspace.h -------------
// Ids stay the internal wire format; these maps are how the human strings of syntax text get
// onto the wire and back. Exact lookup only, never parsing: dataset titles may themselves
// contain " - " (or ":"), so splitting would silently mis-resolve.

QMap<QString, int> Workspace::datasetSelectionSurfaceMap() const
{
	QMap<QString, int> map;

	//Every dropdown entry ("title - filterName" -> filter id), default filters included: their
	//label embeds the translatable "Default filter", which is precisely why scripts are pointed
	//at the bare titles below instead - but the label works too, in the language it was written.
	for(const QVariant & entry : dataSetFilterDropDownList())
	{
		const QVariantMap mapEntry	= entry.toMap();
		bool			  ok		= false;
		const int		  filterId	= mapEntry.value("value").toString().toInt(&ok);

		if(ok)
			map.insert(mapEntry.value("label").toString(), filterId);
	}

	//Bare dataset title -> that dataset's default filter id (language-independent spelling of
	//"the data as loaded"). Bare titles WIN label collisions - a dataset titled like another
	//pair's dropdown label is addressed by its title, because titles are the primary handle:
	for(const auto & idData : _dataSets)
	{
		const DataSet * dataSet = idData.second;

		if(!dataSet || !dataSet->defaultFilter())
			continue;

		if(map.contains(dataSet->title()) && map.value(dataSet->title()) != dataSet->defaultFilter()->id())
			Log::log() << "Workspace: dataset title '" << dataSet->title()
			           << "' collides with a dataset/filter dropdown label, the bare title wins (-> default filter "
			           << dataSet->defaultFilter()->id() << ")" << std::endl;

		map.insert(dataSet->title(), dataSet->defaultFilter()->id());
	}

	return map;
}

QString Workspace::datasetSelectionSurface(int filterId) const
{
	if(filterId <= 0)
		return QString();

	for(const auto & idData : _dataSets)
	{
		const DataSet * dataSet = idData.second;

		if(!dataSet)
			continue;

		if(dataSet->defaultFilter() && dataSet->defaultFilter()->id() == filterId)
			return dataSet->title();									//bare: no translatable fragment in syntax

		for(const Filter * f : dataSet->filters())
			if(f != dataSet->defaultFilter() && f->id() == filterId)
				return dataSet->title() + " - " + f->title();			//user strings all the way
	}

	return QString();													//unknown id renders empty (and the empty selection then fails loudly on re-parse)
}

bool Workspace::resolveDataSetSelectionValue(const QString &optionName, const Json::Value &value, int &filterId, QString &error) const
{
	filterId = -1;

	QString text;

	if(value.isString())
		text = tq(value.asString());
	else if(value.isIntegral())
		text = QString::number(value.asInt64());
	else if(value.isObject() && value.isMember("value") && (value["value"].isString() || value["value"].isIntegral()))
	{
		//Desktop-shaped {types,value}: unwrap the plain string/int inside (the type tag says
		//nothing about a dataset selection); the bare rules below then apply to it.
		const Json::Value & inner = value["value"];
		text = inner.isString() ? tq(inner.asString()) : QString::number(inner.asInt64());
	}
	else if(!value.isNull())
	{
		error = "The dataset selection option '" + optionName + "' holds a value that is not a plain string: pass the dataset title directly (e.g. \"DataSetA\" or \"DataSetA - myfilter\")";
		return false;
	}

	text = text.trimmed();

	const QMap<QString, int> surfaceMap = datasetSelectionSurfaceMap();

	if(text.isEmpty())
	{
		error = "The dataset selection option '" + optionName + "' must name one of the loaded datasets (available: " + surfaceMap.keys().join(", ") + ")";
		return false;
	}

	if(surfaceMap.contains(text))
	{
		filterId = surfaceMap.value(text);
		return true;
	}

	//No numeric-id path, ever: a number resolves only when a dataset is actually titled that
	//number (M4's title-shadows-id disappears together with the id-acceptance it shadowed).
	error = "The dataset selection option '" + optionName + "' holds '" + text + "', which matches no dataset selection string (available: " + surfaceMap.keys().join(", ") + ")";
	return false;
}

// --- mds-plan D7.0a: the user-qualified column-reference grammar, see workspace.h ------------
// (plan §5; the parse/render surface mirrors the selection surface above: human strings,
// exact resolution against the live workspace, typed rejections with ready-to-show messages.)

//The accepted type-segment spellings are THE columnType names (case-insensitive, canonical on
//output), built from the enum's own map so the grammar can never drift from it.
static bool columnTypeFromSurfaceName(const QString & raw, columnType & out)
{
	static const std::map<QString, columnType> spellings = []()
	{
		std::map<QString, columnType> map;
		for(const auto & idName : columnTypeToStringMap())
			map.emplace(QString::fromStdString(idName.second).toLower(), columnType(idName.first));
		return map;
	}();

	const auto it = spellings.find(raw.toLower());

	if(it == spellings.end())
		return false;

	out = it->second;
	return true;
}

static QStringList columnTypeSurfaceSpellings()
{
	QStringList spellings;
	for(const auto & idName : columnTypeToStringMap())
		spellings.push_back(QString::fromStdString(idName.second));
	return spellings;
}

QString QualifiedColumnRef::escapeSegment(const QString & raw)
{
	QString escaped;
	escaped.reserve(raw.size());

	for(const QChar & c : raw)
	{
		if(c == '"' || c == '\\')
			escaped += '\\';
		escaped += c;
	}

	return escaped;
}

QString QualifiedColumnRef::render() const
{
	QStringList parts;

	if(!dataSetName.isEmpty())
		parts.push_back(dataSetName);
	if(!filterName.isEmpty())
		parts.push_back(filterName);
	parts.push_back(columnName); //always present

	QString out;

	for(const QString & part : parts)
	{
		if(!out.isEmpty())
			out += '.';
		out += '"' + escapeSegment(part) + '"';
	}

	if(!typeName.isEmpty())
		out += "(\"" + escapeSegment(typeName) + "\")";

	return out;
}

bool QualifiedColumnRef::parse(const QString & text, QualifiedColumnRef & out, QString & error)
{
	out		= QualifiedColumnRef();
	error.clear();

	int			pos		= 0;
	const int	size	= text.size();

	auto skipSpaces = [&pos, &text, size]()
	{
		while(pos < size && text[pos].isSpace())
			++pos;
	};

	//One quoted segment; `pos` sits on the opening quote. Exactly two escapes exist (" and
	//\); a backslash before anything else is rejected, which keeps parse(render(x)) == x and
	//leaves no ambiguity about what a backslash inside a name means.
	auto parseQuotedSegment = [&](QString & value) -> bool
	{
		++pos; //opening quote

		while(pos < size)
		{
			const QChar c = text[pos];

			if(c == '\\')
			{
				if(pos + 1 >= size)
				{
					error = "a trailing backslash at the end of the reference is not a complete escape";
					return false;
				}

				const QChar escaped = text[pos + 1];

				if(escaped != '"' && escaped != '\\')
				{
					error = "invalid escape '\\" + QString(escaped) + "' in a quoted segment (only \\\" and \\\\ exist)";
					return false;
				}

				value += escaped;
				pos += 2;
			}
			else if(c == '"')
			{
				++pos; //closing quote
				return true;
			}
			else
			{
				value += c;
				++pos;
			}
		}

		error = "unterminated quoted segment (missing closing '\"')";
		return false;
	};

	skipSpaces();

	if(pos >= size)
	{
		error = "The column reference is empty";
		return false;
	}

	if(text[pos] != '"')
	{
		//The bare production: the whole (trimmed) text is the unqualified column name. Dots
		//are never split here - bare handling is the callers' context fallback (§4 boundary).
		out.columnName = text.trimmed();
		return true;
	}

	QStringList segments;
	QString		typeSegment;

	QString segment;
	if(!parseQuotedSegment(segment))
		return false;
	segments.push_back(segment);

	for(;;)
	{
		skipSpaces();

		if(pos >= size)
			break;

		if(text[pos] == '.')
		{
			++pos;
			skipSpaces();

			if(pos >= size || text[pos] != '"')
			{
				error = "expected a quoted segment after '.'";
				return false;
			}

			if(segments.size() >= 3)
			{
				error = "a column reference has at most three segments: \"Dataset\".\"Filter\".\"Column\"";
				return false;
			}

			segment.clear();
			if(!parseQuotedSegment(segment))
				return false;
			segments.push_back(segment);
		}
		else if(text[pos] == '(')
		{
			++pos;
			skipSpaces();

			if(pos >= size || text[pos] != '"')
			{
				error = "the type segment must be a quoted column type name, e.g. (\"scale\")";
				return false;
			}

			segment.clear();
			if(!parseQuotedSegment(segment))
				return false;

			skipSpaces();
			if(pos >= size || text[pos] != ')')
			{
				error = "expected ')' after the type segment";
				return false;
			}
			++pos;

			skipSpaces();
			if(pos < size)
			{
				error = "unexpected text after the type segment: '" + text.mid(pos) + "'";
				return false;
			}

			typeSegment = segment;
			break;
		}
		else
		{
			error = "unexpected character '" + QString(text[pos]) + "' after a segment: expected '.', '(\"Type\")' or the end of the reference";
			return false;
		}
	}

	if(!typeSegment.isEmpty())
	{
		columnType parsed = columnType::unknown;

		if(!columnTypeFromSurfaceName(typeSegment, parsed))
		{
			error = "unknown column type \"" + typeSegment + "\" (known: " + columnTypeSurfaceSpellings().join(", ") + ")";
			return false;
		}

		out.typeName = tq(columnTypeToString(parsed)); //canonical spelling
	}

	if(segments.size() == 1)
	{
		//A quoted bare reference: still unqualified (the quotes change nothing about context).
		out.columnName = segments[0];
		return true;
	}

	out.dataSetName	= segments[0];
	out.columnName	= segments.back();

	if(segments.size() == 3)
		out.filterName = segments[1];

	return true;
}

QualifiedRefError Workspace::resolveQualifiedColumnRef(const QualifiedColumnRef & ref, ResolvedQualifiedColumn & out, QString & error) const
{
	out		= ResolvedQualifiedColumn();
	error.clear();

	if(ref.isBare())
	{
		error = "The column reference '" + ref.columnName + "' is unqualified: a bare name resolves through its context (the filter's own dataset, the selected input filter or the analysis host), not through the workspace";
		return QualifiedRefError::unqualifiedReference;
	}

	DataSet * dataSet	= nullptr;
	Filter	* filter	= nullptr;

	if(!ref.filterName.isEmpty())
	{
		//Full form "Dataset"."Filter"."Column": dataset by title, filter by name inside it
		//(DataSet::filter matches named filters and the default filter by its internal
		//DEFAULT_FILTER_NAME spelling).
		dataSet = dataSetByTitle(ref.dataSetName);

		if(!dataSet)
		{
			QStringList titles;
			for(const auto & idData : _dataSets)
				titles.push_back(idData.second->title());
			error = "No loaded dataset is titled '" + ref.dataSetName + "' (loaded: " + titles.join(", ") + ")";
			return QualifiedRefError::unknownDataSet;
		}

		filter = dataSet->filter(fq(ref.filterName));

		if(!filter)
		{
			QStringList filterNames;
			for(const Filter * f : dataSet->filters())
				filterNames.push_back(f->nameQ());

			error = "Dataset '" + ref.dataSetName + "' has no filter named '" + ref.filterName + "' (its filters: " + filterNames.join(", ") + ")";
			return QualifiedRefError::unknownFilterInDataSet;
		}
	}
	else
	{
		//Two-segment form "X"."Column": X is a dataset title first (its default filter), and
		//only when no dataset is titled X is it a filter name - and then only when UNIQUE
		//across all loaded datasets (plan §5). No clever fall-through when the title matches:
		//dataset-title precedence is exact, like the selection surface above.
		dataSet = dataSetByTitle(ref.dataSetName);

		if(dataSet)
		{
			filter = dataSet->defaultFilter();

			if(!filter)
			{
				error = "Dataset '" + ref.dataSetName + "' has no default filter (yet)";
				return QualifiedRefError::unknownFilterInDataSet;
			}
		}
		else
		{
			QList<DataSet *> candidates;
			for(const auto & idData : _dataSets)
				if(idData.second->filter(fq(ref.dataSetName)))
					candidates.push_back(idData.second);

			if(candidates.size() > 1)
			{
				QStringList titles;
				for(const DataSet * candidate : candidates)
					titles.push_back(candidate->title());

				error = "The filter name '" + ref.dataSetName + "' is used in several datasets (" + titles.join(", ") + "): qualify it with the dataset, e.g. \"" + candidates.first()->title() + "\".\"" + ref.dataSetName + "\".\"" + ref.columnName + "\"";
				return QualifiedRefError::ambiguousFilterName;
			}

			if(candidates.empty())
			{
				QStringList titles;
				for(const auto & idData : _dataSets)
					titles.push_back(idData.second->title());
				error = "'" + ref.dataSetName + "' matches no loaded dataset title and no filter name (loaded datasets: " + titles.join(", ") + ")";
				return QualifiedRefError::filterNameMatchesNothing;
			}

			dataSet	= candidates.first();
			filter	= dataSet->filter(fq(ref.dataSetName));
		}
	}

	Column * column = dataSet->column(fq(ref.columnName));

	if(!column)
	{
		QStringList columnNames;
		for(const Column * c : dataSet->columns())
			columnNames.push_back(tq(c->name()));

		error = "Dataset '" + ref.dataSetName + "' has no column named '" + ref.columnName + "' (its columns: " + columnNames.join(", ") + ")";
		return QualifiedRefError::unknownColumn;
	}

	out.dataSetId	= dataSet->id();
	out.filterId	= filter->id();
	out.columnName	= fq(ref.columnName);
	out.type		= column->type();

	if(!ref.typeName.isEmpty())
	{
		columnType stamped = columnType::unknown;

		if(!columnTypeFromSurfaceName(ref.typeName, stamped))
		{
			error = "Unknown column type \"" + ref.typeName + "\" in the reference";
			return QualifiedRefError::unknownTypeName;
		}

		out.type = stamped; //the type segment wins over the lookup (plan §5)
	}

	return QualifiedRefError::none;
}

QString Workspace::renderQualifiedColumnRef(int contextFilterId, int filterId, const std::string & columnName, columnType type)
{
	if(filterId < 0)
	{
		Log::log() << "Workspace::renderQualifiedColumnRef: no filter id to qualify against, rendering nothing" << std::endl;
		return QString();
	}

	Filter * target = filterById(filterId);

	if(!target || !target->data())
	{
		//Dead/unresolvable slice: render empty and fail loudly downstream, exactly like
		//datasetSelectionSurface's unknown-id handling.
		Log::log() << "Workspace::renderQualifiedColumnRef: filter id " << filterId << " resolves to nothing, rendering nothing" << std::endl;
		return QString();
	}

	//Inside the caller's default namespace: the plain name IS the reference (the callers'
	//bare-name fallback machinery takes it from here, §4). Filter ids are globally unique,
	//so id equality means the very same namespace.
	Filter * context = contextFilterId >= 0 ? filterById(contextFilterId) : nullptr;

	if(context && context->id() == target->id())
		return tq(columnName);

	QualifiedColumnRef ref;
	ref.columnName	= tq(columnName);

	if(type != columnType::unknown)
		ref.typeName = tq(columnTypeToString(type));

	DataSet * dataSet = target->data();

	if(dataSet->defaultFilter() && dataSet->defaultFilter()->id() == filterId)
	{
		ref.dataSetName = dataSet->title(); //"DS"."Col" - a default filter needs no filter segment
		return ref.render();
	}

	//"F"."Col" only when the filter name is unique across all loaded datasets - the minimal
	//form must roundtrip through resolveQualifiedColumnRef, so an ambiguous name is never
	//emitted (it would not resolve again).
	int matches = 0;
	for(const auto & idData : _dataSets)
		if(idData.second->filter(target->name()))
			++matches;

	if(matches == 1)
	{
		ref.filterName = target->nameQ();
		return ref.render();
	}

	ref.dataSetName	= dataSet->title();
	ref.filterName	= target->nameQ();
	return ref.render();
}

std::vector<QualifiedRefOccurrence> QualifiedColumnRef::scanText(const std::string & code)
{
	//mds-plan D7.3: the in-code scanner of the plan §5 surface forms. The grammar is
	//ASCII-structural (quotes, '.', '(' ')'), so the scan runs on bytes; segment CONTENT may
	//be any UTF-8 - each candidate's exact byte range is re-parsed by the strict parse()
	//before it counts, so the scanner and the parser can never disagree about what a
	//reference is. The D7.0a whitespace tolerance is honored between segments.
	std::vector<QualifiedRefOccurrence> occurrences;

	const size_t size = code.size();
	size_t pos = 0;

	auto skipSpaces = [&](size_t & p)
	{
		while(p < size && (code[p] == ' ' || code[p] == '\t' || code[p] == '\n' || code[p] == '\r'))
			++p;
	};

	//Skip one R string literal (either quote style) starting at `p` (code[p] is the opening
	//quote); returns the position after the closing quote, or `size` when unterminated. An
	//escaped anything cannot close the literal.
	auto skipRStringLiteral = [&](size_t p) -> size_t
	{
		const char delim = code[p++];
		while(p < size)
		{
			if(code[p] == '\\')		{ p += 2; continue; }
			if(code[p] == delim)	return p + 1;
			++p;
		}
		return size;
	};

	while(pos < size)
	{
		const char c = code[pos];

		if(c == '#')	//an R comment: nothing inside it is a reference
		{
			while(pos < size && code[pos] != '\n')
				++pos;
			continue;
		}

		if(c != '"' && c != '\'')	{ ++pos; continue; }	//everything else: a candidate can only start at a double quote

		if(c == '\'')
		{
			//A single-quoted R string literal: skipped wholesale, exactly like a double-quoted
			//one - a surface form inside it is user text, not a reference.
			pos = skipRStringLiteral(pos);
			continue;
		}

		//Candidate: quoted segment, then ('.' quoted segment){1,2} and the optional
		//('("Type")') tail, whitespace tolerated between the pieces.
		size_t										p			= pos;
		std::vector<std::pair<size_t, size_t>>		segRanges;	//[begin, end) incl. the quotes
		bool										valid		= true;

		auto trySegment = [&]() -> bool
		{
			if(p >= size || code[p] != '"')
				return false;

			const size_t segStart = p++;

			while(p < size)
			{
				if(code[p] == '\\')			{ if(p + 1 >= size) return false; p += 2; continue; }
				if(code[p] == '"')			{ segRanges.push_back({segStart, ++p}); return true; }
				++p;
			}

			return false; //unterminated
		};

		if(trySegment())
			for(;;)
			{
				size_t q = p;
				skipSpaces(q);

				if(q < size && code[q] == '.')
				{
					if(segRanges.size() >= 3)	{ valid = false; p = q; break; }	//4 segments: not our grammar, and the rest must not rescan as a shifted ref

					++q;
					skipSpaces(q);
					p = q;

					if(!trySegment())			{ valid = false; break; }
					continue;
				}

				if(q < size && code[q] == '(')	//the optional type tail - only when it is ("...")
				{
					++q;
					skipSpaces(q);

					if(q < size && code[q] == '"')
					{
						p = q;
						if(!trySegment())		{ valid = false; break; }
						skipSpaces(p);
						if(p < size && code[p] == ')')	++p;
						else							{ valid = false; break; }
					}
					//an '(' with no quote behind it belongs to the R code (mean("B"."x")): the
					//candidate simply ends before it
				}

				break; //no more continuation
			}
		else
			valid = false;

		if(valid && segRanges.size() >= 2)
		{
			QualifiedColumnRef ref;
			QString error;
			//The strict parser validates the candidate (escape discipline, type spelling,
			//segment count); only a parse-accepted >=2-segment shape is a reference.
			if(QualifiedColumnRef::parse(QString::fromUtf8(code.data() + pos, int(p - pos)), ref, error) && !ref.isBare())
			{
				occurrences.push_back({ pos, p, ref });
				pos = p;
				continue;
			}
		}

		//Not a reference: skip the whole consumed candidate span. For a plain string literal
		//that is the same as skipping the literal (the bare production IS one); for failed
		//shapes it matters that the REMAINDER must not rescan as a shifted reference (a
		//4-segment shape would otherwise degrade into a 3-segment one). On VALID R code this
		//cannot swallow a real reference: an unescaped '."' after a string's closing quote is
		//a syntax error in R, so any candidate spanning a string boundary only arises in
		//already-invalid code - the run fails loudly there either way.
		pos = p > pos ? p : pos + 1;
	}

	return occurrences;
}

std::set<int> Workspace::referencedFilterIdsInCode(const std::string & code, QString * firstError, QualifiedRefError * firstErrorKind) const
{
	std::set<int> filterIds;

	if(firstError)		firstError->clear();
	if(firstErrorKind)	*firstErrorKind = QualifiedRefError::none;

	for(const QualifiedRefOccurrence & occurrence : QualifiedColumnRef::scanText(code))
	{
		ResolvedQualifiedColumn resolved;
		QString error;
		const QualifiedRefError kind = resolveQualifiedColumnRef(occurrence.ref, resolved, error);

		if(kind != QualifiedRefError::none)
		{
			//Unresolvable here: it contributes no slice, and the omission is logged - never
			//silent. The engine re-scans the very same code at run time and fails the run
			//LOUDLY with this kind + message when it still cannot resolve (the D7.5 error
			//surface; the loud-failure doctrine forbids a silent fallback to nothing).
			Log::log() << "Workspace::referencedFilterIdsInCode: the reference '" << occurrence.ref.render().toStdString()
					   << "' does not resolve: " << error.toStdString() << " (contributes no slice; the engine will fail the run loudly if it cannot resolve it at run time either)" << std::endl;

			if(firstErrorKind && *firstErrorKind == QualifiedRefError::none)
				*firstErrorKind = kind;
			if(firstError && firstError->isEmpty())	//the FIRST error only (scan order)
				*firstError = error;

			continue;
		}

		filterIds.insert(resolved.filterId);
	}

	return filterIds;
}

// --- mds-plan D7.1/D7.2: the editor-side surface, see workspace.h ---------------------------

QVariantList Workspace::qualifiedColumnCandidates(int contextFilterId)
{
	//Non-const on purpose (see header): renderQualifiedColumnRef may materialize named
	//filters of a reopened file through filterById.
	QVariantList out;

	Filter * context = contextFilterId >= 0 ? filterById(contextFilterId) : nullptr;

	//The context's own dataset first (the picker's "here" section), the rest in id order.
	std::vector<DataSet *> ordered;
	if(context && context->data())
		ordered.push_back(context->data());
	for(const auto & idData : _dataSets)
	{
		DataSet * dataSet = idData.second;
		if(dataSet && (!context || !context->data() || dataSet != context->data()))
			ordered.push_back(dataSet);
	}

	for(DataSet * dataSet : ordered)
	{
		//Columns are the dataset's (a filter filters ROWS, not columns), minted names are the
		//bare originals; the same column is offered once per filter, because each filter is
		//its own namespace (C3) and the rendered form differs per namespace.
		const DataSet::colTypeMap columns = dataSet->getColumnTypesMap();

		for(Filter * filter : dataSet->filters())
		{
			const bool isDefault = dataSet->defaultFilter() == filter;

			for(const auto & [columnName, type] : columns)
			{
				//The type segment is deliberately not emitted here: insertion emits the
				//minimal valid form (plan §5 render rule) so stored code stays readable;
				//a type-tail is something the user adds by hand when disambiguation needs it.
				const QString code = renderQualifiedColumnRef(contextFilterId, filter->id(), columnName, columnType::unknown);

				if(code.isEmpty())	//dead/unresolvable slice: never offer it (loud-failure doctrine)
					continue;

				QVariantMap entry;
				entry["code"]			= code;
				entry["dataSetTitle"]	= dataSet->title();
				entry["filterName"]		= tq(filter->name());
				entry["filterTitle"]	= filter->title();
				entry["columnName"]		= tq(columnName);
				entry["filterId"]		= filter->id();
				entry["isBare"]			= code == tq(columnName);	//the context's own namespace: the plain name
				entry["label"]			= entry["isBare"].toBool()
										? tq(columnName)
										: dataSet->title() + " - " + (isDefault ? QString() : filter->title() + " - ") + tq(columnName);

				out.append(entry);
			}
		}
	}

	return out;
}

QString Workspace::qualifiedRefErrorInCode(const QString & code) const
{
	//Exactly the D7.3 scanner's typed first error; empty string = all references resolve.
	//Bare names are never errors here (§4: the editors' context fallback is not this helper's
	//business - it is codified at each editor's insertion context).
	QString error;
	referencedFilterIdsInCode(code.toStdString(), &error);
	return error;
}

int Workspace::filterIdByName(int dataSetId, const QString & filterName)
{
	DataSet * dataSet = dataSetById(dataSetId);

	if(!dataSet)
		return -1;

	//DataSet::filter(name) also answers the default filter for the "DEFAULT_FILTER" spelling,
	//which is what Column::computeFilter reports when no filter was picked.
	Filter * filter = dataSet->filter(fq(filterName));

	return filter ? filter->id() : -1;
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
	//The engine may report the encoded name; translate it defensively so the lookup works either
	//way. Decoding is context-free (mds-plan C4): the global table answers, unknown stays raw.
	columnName = ColumnEncoder::decode(columnName);
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
