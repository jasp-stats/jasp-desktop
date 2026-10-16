//
// Copyright (C) 2013-2024 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "databridge.h"
#include "tempfiles.h"
#include "databaseinterface.h"
#include "columnencoder.h"
#include "rbridge.h"
#include "timers.h"
#include "filter.h"
#include "log.h"

DataBridge::DataBridge(unsigned long sessionID, bool useMemory)
	: _extraEncodings(new ColumnEncoder(ExtraOptionsPrefix))
{
	JASPTIMER_START(TempFiles Attach);
	TempFiles::attach(sessionID);
	JASPTIMER_STOP(TempFiles Attach);

	if(sessionID != 0) //Otherwise we are just running to fix R packages
	{
		//singletonOrNull() and not singleton(): the latter *creates* an interface when there is none, so it
		//can never tell us whether a host already made one and we would always end up deleting somebody
		//else's. See resolveDb() below for why that matters.
		_db = DatabaseInterface::singletonOrNull();				//Borrowed: whoever created it destroys it.

		if(!_db)
		{
			_db		= new DatabaseInterface(false, useMemory);	//Nobody else made one (the engine process), so it is ours.
			_ownsDb	= true;
		}
	}
}

DataBridge::~DataBridge()
{
	if(_ownsWorkspace)
		delete _workspace;

	_workspace		= nullptr;
	_ownsWorkspace	= false;
	
	//Re-resolve before deciding: another owner may have replaced or destroyed it, and then it is not ours.
	DatabaseInterface * db = resolveDb();

	if(db && _ownsDb)
	{
		db->close();
		delete db;
	}

	_db		= nullptr;
	_ownsDb	= false;
}

void DataBridge::provideStateFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("state", _analysisId, root, relativePath);
}

void DataBridge::provideJaspResultsFileName(std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific("jaspResults.json", _analysisId, root, relativePath);
}

void DataBridge::provideSpecificFileName(const std::string & specificName, std::string & root, std::string & relativePath)
{
	return TempFiles::createSpecific(specificName, _analysisId, root, relativePath);
}

void DataBridge::provideTempFileName(const std::string & extension, std::string & root, std::string & relativePath)
{
	TempFiles::create(extension, _analysisId, root, relativePath);
}

bool DataBridge::isColumnNameOk(const std::string & columnName)
{
	if(columnName == "" || !provideAndUpdateDataSet())
		return false;

	return provideAndUpdateDataSet()->column(columnName);
}

int DataBridge::getColumnType(const std::string &columnName)
{
	return int(!isColumnNameOk(columnName) ? columnType::unknown : provideAndUpdateDataSet()->column(columnName)->type());
}

int DataBridge::getColumnAnalysisId(const std::string &columnName)
{
	return	!isColumnNameOk(columnName)
		? -1
		: provideAndUpdateDataSet()->column(columnName)->analysisId();
}

int DataBridge::getColumnOriginalIndex(const std::string &columnName)
{
	return provideAndUpdateDataSet()->getColumnIndex(columnName);
}

Workspace * DataBridge::resolveWorkspace()
{
	//Workspace is a process-wide singleton. In the engine we are its only owner, but when we run inside
	//a host that already made one (SyntaxInterface via DataSetProvider, the desktop via DataSetPackage)
	//we must use that one: constructing a second Workspace replaces the singleton with an empty one and
	//cuts the R bridge off from the dataset that was actually loaded. There is then no shownDataSet, so
	//ColumnEncoder::setCurrentEncoder() never runs and every rbridge_* call falls back to the empty
	//default encoder. Resolve it on every use rather than caching, because an owner may swap it out
	//from under us (DataSetProvider::resetDataSet() deletes and recreates it on each loadDataSet).
	if(Workspace::singleton() != _workspace)
	{
		_workspace		= Workspace::singleton();	//Borrowed: whoever created it destroys it.
		_ownsWorkspace	= false;
	}

	if(!_workspace)
	{
		_workspace		= new Workspace();			//Nobody else made one (the engine process), so it is ours.
		_ownsWorkspace	= true;
	}

	return _workspace;
}

DatabaseInterface * DataBridge::resolveDb()
{
	//DatabaseInterface is a process-wide singleton and ownership of it is not ours to assume. In the engine
	//we are the one who makes it, but when we run inside a host that already made one (SyntaxInterface via
	//DataSetProvider, the desktop via DataSetPackage) it belongs to that host and must outlive us: deleting
	//it here leaves that owner deleting freed memory on its own way out. And because ~DatabaseInterface()
	//clears the singleton, the next DatabaseInterface::singleton() quietly opens a fresh interface instead
	//of complaining, so an in-memory db silently loses everything that was loaded into it. Resolve it on
	//every use rather than trusting the cached pointer, because an owner may swap it out from under us
	//(DataSetProvider::getProvider() destroys and recreates the provider, interface and all, whenever the
	//in-memory flag flips, and DatabaseInterface::closeInterfaces() deletes the singleton outright).
	if(DatabaseInterface::singletonOrNull() != _db)
	{
		_db		= DatabaseInterface::singletonOrNull();	//Somebody replaced or destroyed what we were pointing at, so it was never ours to free.
		_ownsDb	= false;
	}

	return _db;
}

DataSet * DataBridge::provideAndUpdateDataSet(int dataSetId, std::function<void(float)> progressCallback)
{
	JASPTIMER_RESUME(DataBridge::provideAndUpdateDataSet());
	
	resolveWorkspace();

	_workspace->checkForUpdates(progressCallback);
	
	if(dataSetId != -1)
		_workspace->setShownDataSet(dataSetId);
			
	if(_workspace->shownDataSet())
	{
		DataSet * ds = _workspace->shownDataSet();
		//Remint the dataset's canonical (mds-plan C2) - every filter view's block in the global
		//decode table is rewritten as a consequence (D1.4 funnel) - and point the legacy non-aware
		//request indirection at the shown filter's VIEW (C7). D1.6 keeps this as the DOCUMENTED
		//LEGACY FALLBACK ONLY: the aware paths resolve their encoder explicitly (the option-stamp
		//resolver in prepareMultiDataSetRun, the slice handle in rbridge_readDataSetRequested), so
		//this currentEncoder is consulted by nothing but the non-aware request context (plain
		//rbridge encodeAll/encodeRScript entries) - behaviour byte-identical to pre-Phase-1 modulo
		//the C1 token shape. Also see the EngineBridgeCallbacks/DataSet guard in
		//ColumnEncoder::setCurrentEncoder / ~DataSet.
		ds->encoder().setCurrentNames(ds->getColumnTypesMap());

		if(ColumnEncoder * view = ds->shownEncoder())
			ColumnEncoder::setCurrentEncoder(view);
	}
	
	JASPTIMER_STOP(DataBridge::provideAndUpdateDataSet());

	return _workspace->shownDataSet();
}

DataBridge::MultiDataSetRunPlan DataBridge::prepareMultiDataSetRun(Json::Value & options, int analysisDataSetId, const std::string & analysisFilter, const std::string & logName)
{
	MultiDataSetRunPlan plan;
	Workspace * workspace = resolveWorkspace();

	//An analysis without a filter reports dataSetId -1 (AnalysisBase only reaches its dataset via
	//the filter), but the shown dataset that provideAndUpdateDataSet() resolves is the primary then.
	//Without this the whole slice queue would key on -1, dataSetById(-1) fails and every dataset
	//gets skipped, leaving the analysis with an empty `datasets` list.
	DataSet * shown = provideAndUpdateDataSet(analysisDataSetId);
	plan.primaryDataSetId = analysisDataSetId >= 0 ? analysisDataSetId : (shown ? shown->id() : -1);
	const int primaryDataSetId = plan.primaryDataSetId;

	//"No filter" is the dataset's default filter - a real db row with a real id - so the analysis
	//always has a concrete primary slice and the queue needs no placeholders.
	DataSet * dataset			= provideAndUpdateDataSet(primaryDataSetId);
	Filter  * analysisFilterObj = dataset && !analysisFilter.empty() ? dataset->filter(analysisFilter) : nullptr;
	Filter  * primaryFilter		= analysisFilterObj ? analysisFilterObj : (dataset ? dataset->defaultFilter() : nullptr);

	//Every value of an aware analysis was selected through a filter, and the generated Engine
	//json names that filter per option/term in the transient `.filterIds` sidecar (mds-plan
	//D3.2/D3.3 - the provenance stamps left .meta with C8). Filter ids are globally unique and
	//imply their dataset (Filter::data()), so the queue is just the distinct sidecar ids plus the
	//analysis' own slice: no dataset/filter pairing, no -1 placeholders, no dedup dance (a
	//std::set is the dedup). Nulls (unqualified terms) contribute nothing - they encode through
	//the C7 fallback, the shown-filter namespace of the dataset the analysis itself anchors at.
	std::set<int> filterIds;
	ColumnEncoder::collectFilterIdsFromSidecar(options.get(ColumnEncoder::FILTERIDS_MEMBER, Json::Value::null), filterIds);

	if(primaryFilter)
		filterIds.insert(primaryFilter->id());

	//Load every involved dataset once so each dataset's encoder holds fresh names before we
	//encode against them, then anchor the shown dataset back at the analyses own one.
	for(const int filterId : filterIds)
	{
		Filter * sliceFilter = workspace->filterById(filterId);	//materialises named filters of reopened files

		if(sliceFilter && sliceFilter->data() && sliceFilter->data()->id() != primaryDataSetId)
			provideAndUpdateDataSet(sliceFilter->data()->id());
	}

	dataset = provideAndUpdateDataSet(primaryDataSetId);

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true, //aware always collects the wanted cols, that is how it knows which slices to queue
			[workspace](int & dataSetId, int filterId) -> ColumnEncoder *
			{
				//mds-plan D3.3/D3.7: encode against the SIDECAR filter's VIEW - the exact namespace
				//the slice for that filter is handed to R in (rbridge_readDataSetRequested anchors
				//the slice read on the same view). filter ids are globally unique and imply their
				//dataset (Filter::data), so the id alone resolves it - and re-attributes the
				//collected cols+types to that dataset (the walk has no meta stamps left to read a
				//dataSetId from).
				if(filterId >= 0)
				{
					Filter * stampFilter = workspace->filterById(filterId);	//materialises named filters of reopened files

					if(stampFilter && stampFilter->data())
					{
						dataSetId = stampFilter->data()->id();
						return stampFilter->columnEncoder();
					}
				}

				//C7 legacy fallback (plan section 4 table): an unqualified node (absent sidecar,
				//null position, released files, hostless runs) or a sidecar id whose filter does
				//not resolve encodes against the dataset's shown-filter VIEW - the same namespace
				//provideAndUpdateDataSet points the legacy currentEncoder at - so unstamped runs
				//keep exactly today's behaviour.
				DataSet * ds = workspace->dataSetById(dataSetId);
				return ds ? ds->shownEncoder() : nullptr;
			},
			primaryDataSetId);

	std::vector<MultiDataSetSlice> queue;
	Json::Value sliceKeys(Json::arrayValue), sliceTitles(Json::objectValue), sliceDataSetIds(Json::objectValue);

	for(const int filterId : filterIds)
	{
		Filter	* sliceFilter	= workspace->filterById(filterId);
		DataSet * ds			= sliceFilter ? sliceFilter->data() : nullptr;

		if(!ds)
		{
			Log::log() << "DataBridge::prepareMultiDataSetRun: '" << logName << "' references filter " << filterId
					   << " which does not resolve to a dataset anymore; this slice is skipped." << std::endl;
			continue;
		}

		//The slice key IS the filter id - exactly what a per-form selection option carries - so
		//an analysis indexes its queued datasets directly, the same way encoded option values
		//index a dataframe's columns.
		const std::string key = std::to_string(filterId);

		auto cols = colsPerDataSet.find(ds->id());
		queue.push_back({ ds->id(), filterId, cols == colsPerDataSet.end() ? ColumnEncoder::colsPlusTypes() : cols->second });

		sliceKeys.append(key);
		sliceTitles[key]			= fq(ds->title());
		sliceDataSetIds[key]		= std::to_string(ds->id());
	}

	setMultiDataSetQueue(std::move(queue));

	plan.multiDataSetJson				= Json::objectValue;
	plan.multiDataSetJson["ids"]		= sliceKeys;			///< slice keys = the filter id per slice
	plan.multiDataSetJson["names"]		= sliceTitles;			///< slice key -> dataset title
	plan.multiDataSetJson["dataSetIds"] = sliceDataSetIds;		///< slice key -> dataset id

	//The primary slice (what the analysis itself runs on): its own filter, else the primary
	//dataset's default - a real id in the queue above whenever a primary dataset exists. When
	//there is no primary filter at all the member stays ABSENT: jaspBase keys on !is.null, so an
	//"" here would hand analyses a non-NULL primarySliceKey that is not a key of anything.
	if(primaryFilter)
		plan.multiDataSetJson["primary"] = std::to_string(primaryFilter->id());

	auto primaryCols = colsPerDataSet.find(primaryDataSetId);
	plan.primaryCols = primaryCols == colsPerDataSet.end() ? ColumnEncoder::colsPlusTypes() : primaryCols->second;

	return plan;
}

void DataBridge::stripTransientProvenanceForR(Json::Value & options)
{
	//mds-plan C11/D3.3: the transient `.filterIds` sidecar is consumed by prepareMultiDataSetRun
	//(slice queue + per-node encoding) and by updateOptionsAccordingToMeta (loadFilteredData
	//anchor), then removed here at the last moment before R sees the options - R never sees
	//provenance, and the Engine->R contract stays byte-identical to pre-Phase-3 otherwise.
	//Sidecar-less (every non-aware) tree passes through untouched.
	if(options.isObject())
		options.removeMember(ColumnEncoder::FILTERIDS_MEMBER);
}

std::string DataBridge::createColumn(const std::string &columnName, bool computed)
{
	if(columnName.empty() || isColumnNameOk(columnName))
		return "";

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->createColumn(columnName);

	col->setAnalysisId(_analysisId);
	col->setCodeType(computed ? computedColumnType::analysis : computedColumnType::analysisNotComputed);

	reloadColumnNames();

	return rbridge_encodeColumnName(columnName.c_str());
}

bool DataBridge::deleteColumn(const std::string &columnName)
{
	if(!isColumnNameOk(columnName))
		return false;

	DataSet * data = provideAndUpdateDataSet();
	Column  * col  = data->column(columnName);

	if(col->analysisId() != _analysisId)
		return false;

	data->removeColumn(columnName);

	reloadColumnNames();

	return true;
}

bool DataBridge::setColumnDataAndType(const std::string &columnName, const std::vector<std::string> &data, columnType colType, bool computed)
{
	if(!isColumnNameOk(columnName))
		return false;

	return provideAndUpdateDataSet()->column(columnName)->overwriteDataAndType(data, colType, computed);
}

bool DataBridge::setDataSet(const std::string & datasetName, const std::vector<std::string> & columnNames, const std::vector<columnType> & columnTypes, const std::vector<std::vector<std::string>> & columnData)
{
	DataSet * ds = resolveWorkspace()->dataSetByName(datasetName);

	if(!ds)
		return false;

	size_t	colCount	= columnNames.size(),
			rowCount	= 0;

	for(const auto & col : columnData)
		rowCount = std::max(rowCount, col.size());

	//Replace the current contents of the (computed) output dataset wholesale.
	while(ds->columnCount() > 0)
		ds->removeColumn(0);

	ds->setRowCount(rowCount);

	//insertColumns starts colIdx at 0 (createColumn would leave an off-by-one gap on an empty dataset).
	ds->insertColumns(size_t(0), colCount);

	for(size_t i=0; i<colCount; i++)
	{
		Column * col = ds->column(i);

		col->setName(columnNames[i]);
		col->setDefaultValues(columnTypes[i], false);
		col->setValues(columnData[i], columnData[i], 0);
		col->setType(columnTypes[i]);
	}

	ds->incRevision();

	return true;
}

void DataBridge::reloadColumnNames()
{
	DataSet * ds = provideAndUpdateDataSet();
	if(ds)
		ds->encoder().setCurrentNames(ds->getColumnTypesMap());
}

void DataBridge::updateOptionsAccordingToMeta(Json::Value & encodedOptions)
{
	JASPTIMER_SCOPE(DataBridge::updateOptionsAccordingToMeta);

	//mds-plan D3.3: the loadFilteredData anchor is the option's scalar `.filterIds` sidecar node
	//(grammar: ColumnEncoder::sidecarOwnFilterId; the generator emits it for the filtered
	//tableview, gated by encodeValue, so no aware run loses the anchor) - the .meta filterId
	//stamp is gone with C8 and the dataSetId verification and its mismatch warning retire with
	//the stamps. The walk threads the sidecar exactly like the request-encoding walk: positional
	//nodes are consumed element-wise, container nodes stay shared for named children.
	std::function<void(Json::Value&,Json::Value&,const Json::Value&)> recursiveUpdate;
	recursiveUpdate = [&recursiveUpdate, this](Json::Value & options, Json::Value & meta, const Json::Value & sidecar)
	{
		if(meta.isNull())
			return;

		Json::Value loadFilteredData = !meta.isObject() || !meta.isMember("loadFilteredData") ? Json::nullValue : meta["loadFilteredData"];

		switch(options.type())
		{
		case Json::arrayValue:
			for(int i=0; i<options.size() && i < meta.size(); i++)
			{
				const Json::Value * positional	= ColumnEncoder::sidecarPositional(sidecar);
				const Json::Value	elementNode	= positional && i < positional->size() ? (*positional)[i] : Json::Value::null;

				recursiveUpdate(options[i], meta.type() == Json::arrayValue ? meta[i] : meta, sidecar.isObject() ? sidecar : elementNode);
			}

			return;

		case Json::objectValue:
			for(const std::string & memberName : options.getMemberNames())
				if(memberName != ".meta" && memberName != ColumnEncoder::FILTERIDS_MEMBER && meta.isMember(memberName))
					recursiveUpdate(options[memberName], meta[memberName], ColumnEncoder::sidecarChild(sidecar, memberName));

			if(loadFilteredData.isObject())
			{
				const std::string	colName = loadFilteredData["column"].asString(),
									filterN	= loadFilteredData["filter"].asString();

				//A multi-dataset aware analysis names the filter its values were selected through in
				//the sidecar node at this option's path (D3.3). Filter ids are globally unique and
				//imply their dataset (Filter::data()), so THAT filter is the anchor: resolve it
				//first and look the column up in its own dataset - the id must never re-point the
				//lookup at another dataset. Indexing one dataset's column with another dataset's
				//filter vector is an unchecked out-of-bounds read on filter->filtered(). Options
				//without a scalar sidecar node (all non-aware ones) keep the old name-based path on
				//the dataset this request anchors at.
				const int			sidecarId	= ColumnEncoder::sidecarOwnFilterId(sidecar);
				Filter			*	stamped	= sidecarId >= 0 ? resolveWorkspace()->filterById(sidecarId) : nullptr;
				DataSet			*	data	= nullptr;
				Filter			*	filter	= nullptr;

				if(stamped && stamped->data())
				{
					data   = stamped->data();								//the filter implies its dataset; that is the anchor
					provideAndUpdateDataSet(data->id());				//and point the request's encoder indirection at it too
					filter = stamped;
				}
				else
				{
					data   = provideAndUpdateDataSet(-1);				//the dataset this request anchors at (.meta carries no dataSetId since C8)
					filter = data ? data->filter(filterN) : nullptr;

					if(!filter && sidecarId >= 0)	//stale filter name AND an unresolvable sidecar id: nothing left to honour
						Log::log() << "DataBridge::updateOptionsAccordingToMeta: neither filter '" << filterN << "' nor filterId " << sidecarId
									   << " resolves" << (data ? " in dataset " + std::to_string(data->id()) : "") << "; the option keeps its current values." << std::endl;
				}

				if(!data)
					return;

				Column			*	col		= data->column(colName);

				if(!col)
					return;

				if(col && filter)
				{
					Json::Value rowIndices	= Json::arrayValue,
								values		= Json::arrayValue;
					doublevec	dbls		= col->dataAsRDoubles({}); //We dont pass a filter because we need to know the rowindices.

					for(size_t r=0; r<dbls.size(); r++)
						if(filter->filtered()[r])
						{
							rowIndices	.append(int(r+1));
							values		.append(dbls[r]);
						}

					options["rowIndices"]	= rowIndices;
					options["values"]		= values;
				}
			}
			return;

		default:
			return;
		}
	};

	recursiveUpdate(encodedOptions, encodedOptions[".meta"], encodedOptions.get(ColumnEncoder::FILTERIDS_MEMBER, Json::Value::null));


	//Log::log() << "After updating options according to their meta it is now:\n" << encodedOptions << std::endl;
}



