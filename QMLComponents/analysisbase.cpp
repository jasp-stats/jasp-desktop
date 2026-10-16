#include "log.h"
#include "filter.h"
#include "workspace.h"
#include "columnencoder.h"
#include "analysisbase.h"
#include "analysisform.h"
#include "utilities/qmlutils.h"

const std::string AnalysisBase::emptyString;
const stringvec AnalysisBase::emptyStringVec;

AnalysisBase::AnalysisBase(QObject* parent)
	: QObject(parent)
	, VariableInfoProvider(this)
{
	// If the parent object is the form, just use it. This is used in R-Syntax mode when the AnalysisForm::parseOptions creates a dummy AnalysisBase
	_analysisForm = qobject_cast<AnalysisForm*>(parent);

	connectDataSpecChanges();
}

AnalysisBase::AnalysisBase(QObject* parent, AnalysisBase* duplicateMe)
	: QObject(parent)
	, VariableInfoProvider(this)
{
	//mds-plan D3.4: a duplicate is seeded from the source's GENERATED Storage tree (clean typed
	//state), parked exactly like a file-load and applied once through the controls when the
	//duplicate's form attaches (setAnalysisUp -> bindTo). The old mirror copy is gone; a source
	//without an attached form has no component state to duplicate (empty Storage), matching the
	//old "copy the (empty) mirror" behaviour. The duplicate is "created with options" whenever
	//the source held any.
	_pendingOptions			= duplicateMe->createOptionsJson(BoundControl::Storage);
	_hasPendingOptions		= _pendingOptions.size() > 0;
	_createdWithOptions		= _hasPendingOptions;

	connectDataSpecChanges();
}

void AnalysisBase::connectDataSpecChanges()
{
	//dataSpec only shows what distinguishes this analysis' data from that of the others, so it changes
	//along with the datasets and filters that exist, and with the title of the dataset it runs on.
	//A workspace can be replaced (loading a file) while the analyses stay, so this is called again
	//whenever the analysis gets a filter and the connections are made unique to keep that harmless.
	Workspace * workspace = Workspace::singleton();

	if(!workspace)
		return;

	connect(workspace, &Workspace::dataSetCreated,		this, &AnalysisBase::dataSpecChanged, Qt::UniqueConnection);
	connect(workspace, &Workspace::dataSetRemoved,		this, &AnalysisBase::dataSpecChanged, Qt::UniqueConnection);
	connect(workspace, &Workspace::dataSetTitleChanged,	this, &AnalysisBase::dataSpecChanged, Qt::UniqueConnection);
	connect(workspace, &Workspace::filtersCountChanged,	this, &AnalysisBase::dataSpecChanged, Qt::UniqueConnection);
}

QQuickItem* AnalysisBase::formItem() const
{
	return _analysisForm;
}

void AnalysisBase::destroyForm()
{
	Log::log() << "Analysis(" << this << ")::destroyForm() called" << std::endl;

	if(_analysisForm)
	{
		Log::log(false) << " it has a AnalysisForm " << _analysisForm << " so let's destroy it" << std::endl;

		_analysisForm->setParent(		nullptr);
		_analysisForm->setParentItem(	nullptr);

		//The form is a live QML item tree. The AnalysisFormExpander delegate that spawned
		//it is torn down deferred (Repeater deleteLater semantics after the model row
		//removal) and still holds JS wrappers/references to this form (myForm, Connections,
		//closures) until then. A synchronous delete here frees the whole tree under the
		//QV4 incremental GC's feet -> intermittent EXC_BAD_ACCESS in QV4::markDrain.
		//Defer the deletion to the next event-loop pass, at the same quiet point where
		//the delegate's JS references get invalidated.
		//Until then the form must stay inert, as it does before the deferred delete in
		//Analyses::clear/removeAnalysis (Analysis::remove): it no longer reacts to data or
		//engine signals while its analysis or module may already be on their way out.
		_analysisForm->cleanUpForm();
		_analysisForm->deleteLater();
		_analysisForm = nullptr;

		emit formItemChanged();
	}
	else
		Log::log(false) << " it has no AnalysisForm." << std::endl;
	
	_analysisForm = nullptr;
}

void AnalysisBase::createForm(QQuickItem* parentItem)
{
	Log::log() << "Analysis(" << this << ")::createForm() called with parentItem " << parentItem << std::endl;

	setQmlError("");
	
	if(parentItem)
		_parentItem = parentItem;

	if(_analysisForm)
	{
		Log::log() << "It already has a form, so we destroy it." << std::endl;
		
		if (!parentItem) 
			parentItem = _analysisForm->parentItem();
		
		destroyForm();
	}
	else if(!parentItem)
		parentItem = _parentItem;

	try
	{
		Log::log()  << std::endl << "Loading QML form from: " << qmlFormPath(false, false) << std::endl;

		QObject * newForm = instantiateQml(QUrl::fromLocalFile(tq(qmlFormPath(false, false))), module(), qmlContext(parentItem));

		Log::log() << "Created a form, got pointer " << newForm << std::endl;

		_analysisForm = qobject_cast<AnalysisForm *>(newForm);

		if(!_analysisForm)
			throw std::logic_error("QML file '" + qmlFormPath(false, false) + "' didn't spawn into AnalysisForm, but into: " + (newForm ? fq(newForm->objectName()) : "null"));

		_analysisForm->setAnalysis(this);
		_analysisForm->setParent(this);
		_analysisForm->setParentItem(parentItem);

		emit formItemChanged();
	}
	catch(qmlLoadError & e)
	{
		setQmlError(e.what());
		_analysisForm = nullptr;
	}
	catch(std::exception & e)
	{
		setQmlError(e.what());
		_analysisForm = nullptr;
	}
}

std::string AnalysisBase::qmlFormPath(bool, bool) const
{
	return module() + "/qml/"  + name();
}

const QString &AnalysisBase::qmlError() const
{
	return _qmlError;
}

void AnalysisBase::setQmlError(const QString &newQmlError)
{
	if (_qmlError == newQmlError)
		return;
	_qmlError = newQmlError;
	emit qmlErrorChanged();
}

void AnalysisBase::setIsAnnotated(bool isAnnotated)
{
	if (!_analysisForm || _isAnnotated == isAnnotated)
		return;

	_isAnnotated = isAnnotated;

	_analysisForm->setIsAnnotated(isAnnotated);
}

// This method tries to find the parent keys in _boundValues Json object
// If found, it sets the path to this reference to parentNames and returns a reference of the sub Json object
Json::Value& AnalysisBase::_getParentBoundValue(const QVector<JASPControl::ParentKey>& parentKeys, QVector<std::string>& parentNames, bool& found, bool createAnyway)
{
	return resolveOptionParent(_boundValues, parentKeys, parentNames, found, createAnyway);
}

//mds-plan D3.1: extracted out of _getParentBoundValue so that the _boundValues mirror and the
//Phase-3 generators (AnalysisForm::createOptionsJson) share ONE implementation of the nested
//option placement - same row-chain semantics, same parentNames bookkeeping. Everything below is
//the mirror's code, unchanged, except that the tree it walks is a parameter.
//
// A parentKey has 3 properties: <name>, <key> and <value>: it assumes that the json boundValue is an abject, one of its member is <name>,
// whom value is an array of json objects. These objects have a member <key>, and one of them has for value <value>.
// if there are several parentKeys, it repeats this operation
//
// {
//		anOptionName : "one"
//		...
//		<name>: [
//			{
//				<key>: "anothervalue"
//				anotherKey: "xxx"
//			},
//			{
//				<key>: "<value>"
//				anotherKey: "yyy" // parentBoundValue gets a reference to this Json object
//			}
//		]
// }
Json::Value & AnalysisBase::resolveOptionParent(Json::Value & root, const QVector<JASPControl::ParentKey> & parentKeys, QVector<std::string> & parentNames, bool & found, bool createAnyway)
{
	found = (parentKeys.size() == 0);
	Json::Value* parentBoundValue = &root;

	for (const auto& parent : parentKeys)
	{
		found = false;
		if (createAnyway && !parentBoundValue->isMember(parent.name))	(*parentBoundValue)[parent.name] = Json::Value(Json::arrayValue);

		if (parentBoundValue->isMember(parent.name))
		{
			Json::Value* parentBoundValues = &(*parentBoundValue)[parent.name];
			if (parentBoundValues->isObject() && parentBoundValues->isMember("value") && parentBoundValues->isMember("types"))
				parentBoundValues = &(*parentBoundValues)["value"]; // If the control contain variable names, the option values are inside the 'value' member

			if (!parentBoundValues->isNull() && parentBoundValues->isArray())
			{
				for (Json::Value & boundValue : (*parentBoundValues))
				{
					//isMember throws on non-objects (e.g. fuzzed garbage, or an array
					//left half-bound after an interrupted option bind), which would
					//escape through QML signal handlers and terminate the process.
					if (!boundValue.isObject())
						continue;

					if (boundValue.isMember(parent.key))
					{
						Json::Value* keyValue = &(boundValue[parent.key]);
						if (keyValue->isObject() && keyValue->isMember("value") && keyValue->isMember("types"))
							keyValue = &((*keyValue)["value"]); // The key can be also a variable name

						// The value can be a string or an array of strings (for interaction terms)
						if (keyValue->isString() && parent.value.size() == 1)
						{
							if (keyValue->asString() == parent.value[0])	found = true;
						}
						else if (keyValue->isArray() && keyValue->size() == parent.value.size())
						{
							found = true;
							size_t i = 0;
							for (const Json::Value& compVal : *keyValue)
							{
								if (!compVal.isString() || compVal.asString() != parent.value[i]) found = false;
								i++;
							}
						}
						if (found)
						{
							parentBoundValue = &boundValue;
							parentNames.append(parent.name);
							break;
						}
					}
				}
			}

			if (!found && createAnyway && (parentBoundValues->isNull() || parentBoundValues->isArray()))
			{
				// A control can be created before the parent control is completed.
				Json::Value row(Json::objectValue);
				if (parent.value.size() == 1)
					row[parent.key] = parent.value[0];
				else
				{
					Json::Value newValue(Json::arrayValue);
					for (size_t i = 0; i < parent.value.size(); i++)
						newValue.append(parent.value[i]);
					row[parent.key] = newValue;
				}
				parentBoundValue = &(parentBoundValues->append(row));
				found = true;
			}
		}
	}

	return *parentBoundValue;
}

//mds-plan D3.1: the mirror's send-tree placement (was inline in setBoundValue), shared with the
//generators so `.meta` and `.filterIds` nest by the same rule on both sides.
void AnalysisBase::placeOptionMeta(Json::Value & metaRoot, const QVector<std::string> & parentNames, const std::string & name, const Json::Value & meta)
{
	Json::Value* metaBoundValue = &metaRoot;
	for (const std::string& parent : parentNames)
		metaBoundValue = &((*metaBoundValue)[parent]);
	(*metaBoundValue)[name] = meta;
}

//mds-plan D3.3: sidecar placement differs from meta placement in ONE way: a meta node is always
//an object, so row-child meta merges into the parent option's node freely. A `.filterIds` node
//is a bare int/array, and a list control with stamped row children would have its positional
//array sitting exactly where the children's named nodes go - placing through placeOptionMeta
//would hit jsoncpp's "requires objectValue" assert the moment such an aware form generates
//(latent since D3.2, live from the D3.3 flip on: every run generates). On collision the
//own-node is demoted into the container's reserved "$" member - the grammar
//ColumnEncoder::sidecarOwnFilterId reads; non-colliding trees keep the D3.2 shape byte-identically.
void AnalysisBase::placeOptionFilterIds(Json::Value & filterIdsRoot, const QVector<std::string> & parentNames, const std::string & name, const Json::Value & filterIds)
{
	Json::Value * node = &filterIdsRoot;

	for(const std::string & parent : parentNames)
	{
		Json::Value & child = (*node)[parent];	//creates a null slot on the way down, placeable

		if(child.isArray() || child.isInt())	//an own-node already placed here: demote to "$"
		{
			Json::Value container(Json::objectValue);
			container[ColumnEncoder::FILTERIDS_SELF] = child;
			child = container;
		}

		node = &child;
	}

	Json::Value & slot = (*node)[name];

	if(slot.isObject() && !filterIds.isObject())	//named children already in the slot: the own-node rides "$"
		slot[ColumnEncoder::FILTERIDS_SELF] = filterIds;
	else
		slot = filterIds;
}

std::string AnalysisBase::_displayParentKeys(const QVector<JASPControl::ParentKey> & parentKeys) const
{
	std::string keys;
	bool firstKey = true;
	for (const auto& parentKey : parentKeys)
	{
		std::string parentValue;
		bool firstValue = true;
		for (const std::string& v : parentKey.value)
		{
			if (!firstValue) parentValue += "*";
			parentValue += v;
			firstValue = false;
		}

		if (!firstKey) keys += " - ";
		keys += ("key: " + parentKey.key + " name: " + parentKey.name + " value: " + parentValue);
		firstKey = false;
	}

	return keys;
}

void AnalysisBase::setBoundValue(const std::string &name, const Json::Value &value, const Json::Value &meta, const QVector<JASPControl::ParentKey>& parentKeys)
{
	bool found = false;
	QVector<std::string> parents;
	Json::Value& parentBoundValue = _getParentBoundValue(parentKeys, parents, found, true);

	if (found && parentBoundValue.isObject())
	{
		parentBoundValue[name] = value;

		if ((meta.isObject() || meta.isArray()) && meta.size() > 0)
			placeOptionMeta(_boundValues[".meta"], parents, name, meta);
	}
	else
		Log::log() << "Could not find parent keys " << _displayParentKeys(parentKeys) << " in options: " << _boundValues.toStyledString() << std::endl;

	if (_analysisForm->initialized())
	{
		emit boundValuesChanged();

		//For multi-dataset aware analyses the referenced datasets (and thus dataSpec) come from the bound
		//values' .meta, so a changed option can change what the analysis runs on.
		if (multiDataSetAware())
			emit dataSpecChanged();
	}
}

void AnalysisBase::setBoundValues(const Json::Value &boundValues)
{
	_boundValues = boundValues;
}

//mds-plan C12/D3.4: the explicit pending-options parking lot. setAnalysisUp consumes it ONCE
//(takePendingOptions) and routes it through bindTo; run/save/dataSpec never read it. wasCreatedByOptions
//distinguishes "the analysis arrived with these options" (file/RPC/duplicate) from a later
//programmatic rebind.
void AnalysisBase::setPendingOptions(const Json::Value & options, bool wasCreatedByOptions)
{
	_pendingOptions = options;
	_hasPendingOptions = true;

	if(wasCreatedByOptions)
		_createdWithOptions = true;
}

Json::Value AnalysisBase::takePendingOptions()
{
	Json::Value options = _pendingOptions;

	_pendingOptions = Json::objectValue;
	_hasPendingOptions = false;

	return options;
}

//mds-plan C11/D3.4: the transient `.filterIds` sidecar is desktop-internal provenance (consumed by
//the slice-queue build and request encoding) and never leaves to R, RPC clients, QML options() or
//generated scripts. Stripping is the one top-level member removal D3.3 established.
void AnalysisBase::stripTransientSidecar(Json::Value & options)
{
	options.removeMember(ColumnEncoder::FILTERIDS_MEMBER);
}

QString AnalysisBase::boundValuesAsJson() const
{
	Json::StreamWriterBuilder builder;
	builder["indentation"] = "";
	return QString::fromStdString(Json::writeString(builder, _boundValues));
}

//mds-plan C11/D3.1: the whole options tree GENERATED from the components (the form walks its
//registered controls), not read from _boundValues. While the mirror still feeds the run/save paths
//(until D3.3/D3.5) the equivalence harness keeps the two in lockstep; from the flip on this is the
//only tree. An analysis without a form has no component state to generate from.
Json::Value AnalysisBase::createOptionsJson(BoundControl::JsonFormat format)
{
	return _analysisForm ? _analysisForm->createOptionsJson(format) : Json::Value(Json::objectValue);
}

QString AnalysisBase::createOptionsJsonAsJson(int format)
{
	Json::Value options = createOptionsJson(format == 1 ? BoundControl::Storage : BoundControl::Engine);

	Json::StreamWriterBuilder builder;
	builder["indentation"] = "";
	return QString::fromStdString(Json::writeString(builder, options));
}

const Json::Value &AnalysisBase::boundValue(const std::string &name, const QVector<JASPControl::ParentKey> &parentKeys)
{
	bool found = false;
	QVector<std::string> parentNames;
	Json::Value& parentBoundValue = _getParentBoundValue(parentKeys, parentNames, found);


	if (found && !parentBoundValue.isNull() && parentBoundValue.isObject())	return parentBoundValue[name];
	else																	return Json::Value::null;
}


Filter *AnalysisBase::filter() const
{
	return _filter;
}

bool AnalysisBase::usesDataSet(int dataSetId) const
{
	//A multi-dataset aware analysis operates on exactly the datasets its options reference (plus
	//its own filter's dataset if it has one): the "unbound applies to any dataset" shortcut below
	//would claim all of them, which is meaningless when each selection names its own dataset.
	if(multiDataSetAware())
	{
		if(_filterDataSet && _filterDataSet->id() == dataSetId)
			return true;

		return referencedDataSets().count(dataSetId) > 0;
	}

	//An analysis without an explicit dataset binding applies to any dataset (e.g. reports, unbound analyses).
	if(!_filterDataSet || _filterDataSet->id() == dataSetId)
		return true;

	return false;
}

std::map<int, int> AnalysisBase::referencedDataSets() const
{
	//mds-plan D3.6/C11: computed ON DEMAND from component state - the typed filterIds the bound
	//controls generate (the same per-control createFilterIds() output that rides the transient
	//`.filterIds` sidecar at send time, positional per-term entries included), the filters the
	//registered selection hosts hold, and the analysis' own filter. No mirror, no meta: the
	//.meta-stamp walk (ColumnEncoder::collectDataSetIdsFromMeta over the bound-values tree) is
	//dead input since the D3.3 flip and retires with the mirror (D3.4).
	//
	//The D3.5/C12 gate stays: references exist only once the form has attached and its controls
	//bound - pending JSON is never read for behaviour, and setAnalysisUp re-notifies consumers
	//through notifyDataSpecChanged() when real component state appears.
	//
	//Filter ids imply their dataset (C1); an id that names no live filter contributes nothing
	//(the C16 doctrine: stale provenance goes raw, it must not keep a dead dataset claimed -
	//the old map carried dead stamped ids through, but every consumer already resolved them to
	//no dataset and skipped them). Two filters of one dataset: first seen wins, matching the
	//old walk's keep-the-first-pair rule; every consumer (dataSpec, usesDataSet, RPC
	//dataSetIds) additionally dedups against the analysis' own dataset anyway.
	if(!_analysisForm || !_analysisForm->initialized())
		return {};

	Workspace * workspace = Workspace::singleton();

	if(!workspace)
		return {};

	std::map<int, int> dataSetFilterIds;

	auto addFilter = [&workspace, &dataSetFilterIds](int filterId)
	{
		if(filterId < 0)
			return;

		Filter * filter = workspace->filterById(filterId);	//resolves named filters of reopened files too (D1.6)

		if(!filter || !filter->data())
			return;

		const int dataSetId = filter->data()->id();

		if(dataSetFilterIds.find(dataSetId) == dataSetFilterIds.end())
			dataSetFilterIds[dataSetId] = filterId;
	};

	//(a) the values' own typed provenance: per-control createFilterIds generations, including
	// foreign filterIds a bindTo-copied term carries from another host's selection (mds-plan C9).
	std::set<int> valueFilterIds;
	_analysisForm->collectFilterIds(valueFilterIds);
	for(int filterId : valueFilterIds)
		addFilter(filterId);

	//(b) every registered selection host's current filter: a host that serves no values yet still
	// owns a namespace (the empty-list case the option-level mirror stamp used to cover, and any
	// host whose dropdown never wrote its option).
	for(const Filter * filter : selectedSelectionFilters())
		if(filter)
			addFilter(filter->id());

	//(c) the analysis' own filter (the FilterMenuButton one) - part of what the analysis runs on;
	// hostless column-bearing controls already generate it through (a), and every consumer dedups
	// it, so naming it explicitly changes no report and closes the no-controlled-at-all case.
	if(_filter)
		addFilter(_filter->id());

	return dataSetFilterIds;
}

void AnalysisBase::notifyDataSpecChanged()
{
	emit dataSpecChanged();
}

QString AnalysisBase::filterName() const
{
	return _filter ? _filter->nameQ() : "";
}

int AnalysisBase::filterId() const
{
	return _filter ? _filter->id() : -1;
}

QString AnalysisBase::dataSpec() const
{
	//A multi-dataset aware analysis has no single dataset: Desktop::Analysis::dataSet() refuses
	//to name one (and logs about it), so the old `!data` early return made this always empty for
	//aware analyses - the results header showed nothing and every call spammed the log. The spec
	//comes from the datasets the options actually reference instead: own dataset first when it
	//has one, then the others, " + " joined - deleting any of them has consequences for this
	//analysis as well (mds-plan C3/M7).
	if(multiDataSetAware())
	{
		Workspace * workspace = Workspace::singleton();

		if(!workspace)
			return "";

		const std::map<int, int> referenced = referencedDataSets();

		QStringList titles;

		if(_filterDataSet)
			titles.append(_filterDataSet->title());

		for(const auto & dataSetFilter : referenced)
			if(!_filterDataSet || dataSetFilter.first != _filterDataSet->id())
				if(DataSet * other = workspace->dataSetById(dataSetFilter.first))
					titles.append(other->title());

		return titles.join(" + ");
	}

	DataSet * data = dataSet();

	if(!data)
		return "";

	bool showDataSet	= data->workspace() && data->workspace()->dataSets().size()	> 1,
		 showFilter		= _filter			&& data->filters().size()				> 1;

	return	showDataSet && showFilter	?	QString("%1 - %2").arg(data->title(), _filter->title())
		:	showDataSet					?	data->title()
		:	showFilter					?	_filter->title()
		:									"";
}

void AnalysisBase::setMultiDataSetAware(bool multiDataSetAware)
{
	if (_multiDataSetAware == multiDataSetAware)
		return;

	_multiDataSetAware = multiDataSetAware;
	emit multiDataSetAwareChanged();
}

void AnalysisBase::setFilterId(int filterId)
{
	if(!Workspace::singleton())
		return;
	
	Filter * f = Workspace::singleton()->filterById(filterId);
		
	if(f == _filter)
		return;

	_filter			= f;
	_filterDataSet	= _filter ? _filter->data() : nullptr;

	connectDataSpecChanges();

	emit filterChanged(_filter);
	emit dataSpecChanged();

	refresh();
}

// ---- VariableInfoProvider over the selection-host registry (mds-plan C10/D2.4) -----------------
// The whole point of this endpoint is that it is DERIVED, never stored: every call re-enumerates
// the form's live selection hosts (which unregister eagerly, mds-plan D2.3) and reads their filter
// through the virtual - a Filter's encode handle lazy-attaches and dies with the filter (D1.2),
// so caching a handle/encoder/filter pointer here would be the first stale namespace in the
// house. See the class doc in analysisbase.h for the one/many/none semantics and what merged
// consumers must do.

QList<Filter *> AnalysisBase::selectedSelectionFilters() const
{
	QList<Filter *> filters;

	if(!_analysisForm)
		return filters;

	for(JASPControl * host : _analysisForm->selectionHostControls())
	{
		Filter * filter = host ? host->selectedSelectionFilter() : nullptr;	//per-use virtual (D2.3)
		if(filter && !filters.contains(filter))
			filters << filter;
	}

	return filters;
}

bool AnalysisBase::selectionNamespaceIsMerged() const
{
	return selectedSelectionFilters().size() > 1;
}

QVariantList AnalysisBase::selectionNamespaceFilterIds() const
{
	QVariantList ids;
	for(const Filter * filter : selectedSelectionFilters())
		ids.append(filter->id());
	return ids;
}

QStringList AnalysisBase::selectionNamespaceVariableNames() const
{
	return provideInfo(varInfoType::VariableNames).toStringList();
}

void AnalysisBase::handleSelectionNamespaceMaybeChanged()
{
	emit selectionNamespaceChanged();

	//The referenced-dataset view of an aware analysis follows its hosts' selections (their stamps
	//ride along with the restamp walk, and once Phase 3 computes dataSpec from component state the
	//namespace IS the data spec): re-emit so title/RPC/queue consumers re-read the live view.
	if(multiDataSetAware())
		emit dataSpecChanged();
}

ColumnEncoder *AnalysisBase::columnEncoder()
{
	const QList<Filter *> filters = selectedSelectionFilters();

	if(filters.isEmpty())
		return _filter ? _filter->columnEncoder() : nullptr;	//no host selection: analysis filter (C13 host-bound fallback)
	if(filters.size() > 1)
		return nullptr;										//merged: no single namespace, say so with nullptr

	return filters.first()->columnEncoder();				//the live Filter resolves its own view handle (never cached)
}

QAbstractItemModel *AnalysisBase::providerModel()
{
	const QList<Filter *> filters = selectedSelectionFilters();

	if(filters.isEmpty())
		return _filter ? _filter->providerModel() : nullptr;
	if(filters.size() > 1)
		return nullptr;										//merged: there is no single model

	return filters.first()->providerModel();
}

QVariant AnalysisBase::provideInfo(varInfoType info, const QString& name, int row) const
{
	const QList<Filter *> filters = selectedSelectionFilters();

	if(filters.isEmpty())
		return _filter ? _filter->provideInfo(info, name, row) : QVariant();
	if(filters.size() == 1)
		return filters.first()->provideInfo(info, name, row);

	//Merged view (mds-plan C10): the union of the selected filters. Only VariableNames is truly
	//aggregated - with collisions KEPT (a column that lives behind two selected namespaces is two
	//entries; the terms' filterId tells them apart downstream, C9). Everything else has no honest
	//union: per-name requests scan the hosts in registration order for the first hit, dataset-wide
	//requests answer from the first host. Consumers that need a single namespace must use the
	//analysis' own filter or a specific host - see the class doc.
	if(info == varInfoType::VariableNames)
	{
		QStringList names;
		for(Filter * filter : filters)
			names << filter->provideInfo(varInfoType::VariableNames).toStringList();
		return names;
	}

	if(info == varInfoType::DataAvailable)
	{
		for(Filter * filter : filters)
			if(filter->provideInfo(varInfoType::DataAvailable).toBool())
				return true;
		return false;
	}

	for(Filter * filter : filters)
	{
		const QVariant value = filter->provideInfo(info, name, row);
		if(value.isValid())
			return value;						//first host that has this column (or answers at all)
	}

	return QVariant();
}

bool AnalysisBase::absorbInfo(varInfoType info, const QString& name, int row, QVariant value)
{
	const QList<Filter *> filters = selectedSelectionFilters();

	if(filters.isEmpty())
		return _filter ? _filter->absorbInfo(info, name, row, value) : false;
	if(filters.size() == 1)
		return filters.first()->absorbInfo(info, name, row, value);

	//Merged: write through the first host that accepts it - same first-hit rule as provideInfo.
	//Editing through a merged namespace is inherently ambiguous; a caller that knows which slice
	//it means goes through that host (or the term's filterId) instead.
	for(Filter * filter : filters)
		if(filter->absorbInfo(info, name, row, value))
			return true;

	return false;
}
