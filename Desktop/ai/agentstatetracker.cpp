//
// AgentStateTracker — implementation
//

#include "agentstatetracker.h"

#include "analysis/analysis.h"
#include "analysis/analyses.h"
#include "data/datasetpackage.h"

AgentStateTracker * AgentStateTracker::_singleton = nullptr;

AgentStateTracker::AgentStateTracker(QObject * parent)
	: QObject(parent)
{
}

// ------------------------------------------------------------------
// Singleton init + signal hooks
// ------------------------------------------------------------------

void AgentStateTracker::init()
{
	if (_singleton)
		return;

	_singleton = new AgentStateTracker();
	_singleton->connectHooks();
}

void AgentStateTracker::notifyAnalysisObserved(size_t analysisId)
{
	if (_singleton)
		_singleton->afterAnalysisObserved(analysisId);
}

void AgentStateTracker::notifyDataObserved()
{
	if (_singleton)
		_singleton->afterDataObserved();
}

void AgentStateTracker::connectHooks()
{
	if (_hooksConnected)
		return;
	_hooksConnected = true;

	auto * ans = Analyses::analyses();
	if (ans)
	{
		// Analysis added — mark dirty + connect per-analysis options signal
		connect(ans, &Analyses::analysisAdded, this, [this](Analysis * a)
		{
			size_t id = a->id();
			markAnalysisAdded(id);

			// Per-analysis hook for option changes (no Analyses-level relay)
			connect(a, &Analysis::userModifiedSomething, this,
				[this, id]() { markAnalysisOptionsChanged(id); });
		});

		// Analysis removed
		connect(ans, &Analyses::analysisRemoved, this, [this](Analysis * a)
		{
			markAnalysisRemoved(a->id());
		});

		// Status relay
		connect(ans, &Analyses::analysisStatusChanged, this, [this](Analysis * a)
		{
			// Don't re-mark if already flagged as added/removed
			auto it = _analysisStates.find(a->id());
			if (it != _analysisStates.end())
			{
				if (it->second.dirtyFlags.count(AnalysisChange::Added) ||
				    it->second.dirtyFlags.count(AnalysisChange::Removed))
					return;
			}
			markAnalysisStatusChanged(a->id());
		});

		// Results relay
		connect(ans, &Analyses::analysisResultsChanged, this, [this](Analysis * a)
		{
			auto it = _analysisStates.find(a->id());
			if (it != _analysisStates.end())
			{
				if (it->second.dirtyFlags.count(AnalysisChange::Added) ||
				    it->second.dirtyFlags.count(AnalysisChange::Removed))
					return;
			}
			markAnalysisResultsChanged(a->id());
		});

		// For analyses that already exist at init time (e.g. loaded from file),
		// connect their userModifiedSomething signal.
		ans->applyToAll([this](Analysis * a)
		{
			connect(a, &Analysis::userModifiedSomething, this,
				[this, a]() { markAnalysisOptionsChanged(a->id()); });
		});
	}

	// Data changes
	auto * ws = DataSetPackage::pkg() ? DataSetPackage::pkg()->workspace() : nullptr;
	if (ws)
	{
		connect(ws, &Workspace::datasetChanged, this,
			[this](int dataSetId,
			       QStringList changedColumns,
			       QStringList missingColumns,
			       QMap<QString, QString> changeNameColumns,
			       bool rowCountChanged,
			       bool hasNewColumns)
		{
			(void)dataSetId;
			(void)rowCountChanged;

			QStringList added;
			QStringList changed;

			if (hasNewColumns)
				added = changedColumns;
			else
				changed = changedColumns;

			markDataChanged(added, missingColumns, changed, changeNameColumns);
		});
	}
}

// ------------------------------------------------------------------
// Dirty-marking
// ------------------------------------------------------------------

void AgentStateTracker::markAnalysisOptionsChanged(size_t analysisId)
{
	_analysisStates[analysisId].dirtyFlags.insert(AnalysisChange::Options);
}

void AgentStateTracker::markAnalysisStatusChanged(size_t analysisId)
{
	_analysisStates[analysisId].dirtyFlags.insert(AnalysisChange::Status);
}

void AgentStateTracker::markAnalysisResultsChanged(size_t analysisId)
{
	_analysisStates[analysisId].dirtyFlags.insert(AnalysisChange::Results);
}

void AgentStateTracker::markAnalysisAdded(size_t analysisId)
{
	auto & state = _analysisStates[analysisId];
	state.dirtyFlags.clear();
	state.dirtyFlags.insert(AnalysisChange::Added);
}

void AgentStateTracker::markAnalysisRemoved(size_t analysisId)
{
	auto & state = _analysisStates[analysisId];
	state.dirtyFlags.clear();
	state.dirtyFlags.insert(AnalysisChange::Removed);
}

void AgentStateTracker::markDataChanged(const QStringList & added,
                                        const QStringList & removed,
                                        const QStringList & changed,
                                        const QMap<QString, QString> & renamed)
{
	_dataState.dirty = true;

	// Apply renames to names we're already tracking, so the snapshot
	// always shows current names (e.g. "Column 32" → "age").
	for (auto it = renamed.begin(); it != renamed.end(); ++it)
	{
		const QString & oldName = it.key();
		const QString & newName = it.value();
		_dataState.added.replaceInStrings(oldName, newName);
		_dataState.changed.replaceInStrings(oldName, newName);
		_dataState.removed.removeAll(newName);
	}

	// A column removed after being added cancels out
	for (const auto & s : removed)
		_dataState.added.removeAll(s);

	for (const auto & s : added)    _dataState.added.append(s);
	for (const auto & s : removed)  _dataState.removed.append(s);
	for (const auto & s : changed)  _dataState.changed.append(s);

	// Track renames for columns that already existed (not newly added).
	for (auto it = renamed.begin(); it != renamed.end(); ++it)
	{
		if (!_dataState.added.contains(it.value()))
			_dataState.renamed[it.key()] = it.value();
	}

	// Deduplicate
	_dataState.added.removeDuplicates();
	_dataState.removed.removeDuplicates();
	_dataState.changed.removeDuplicates();
}

// ------------------------------------------------------------------
// Baseline clearing
// ------------------------------------------------------------------

void AgentStateTracker::afterAnalysisObserved(size_t analysisId)
{
	_analysisStates.erase(analysisId);
}

void AgentStateTracker::afterDataObserved()
{
	_dataState.dirty = false;
	_dataState.added.clear();
	_dataState.removed.clear();
	_dataState.changed.clear();
	_dataState.renamed.clear();
}

void AgentStateTracker::clearAll()
{
	_analysisStates.clear();
	afterDataObserved();
}

void AgentStateTracker::markClean()
{
	// The full snapshot has been delivered — clear all dirty flags so the
	// baseline advances.  Everything the agent hasn't seen yet is now seen.
	_analysisStates.clear();
	afterDataObserved();
}

// ------------------------------------------------------------------
// Dirty query
// ------------------------------------------------------------------

bool AgentStateTracker::isDirty() const
{
	if (_dataState.dirty) return true;

	for (const auto & [id, state] : _analysisStates)
		if (!state.dirtyFlags.empty())
			return true;

	return false;
}

bool AgentStateTracker::isUserDiverged() const
{
	if (_dataState.dirty) return true;

	for (const auto & [id, state] : _analysisStates)
	{
		if (state.dirtyFlags.count(AnalysisChange::Options) ||
		    state.dirtyFlags.count(AnalysisChange::Added)   ||
		    state.dirtyFlags.count(AnalysisChange::Removed))
			return true;
	}

	return false;
}

// ------------------------------------------------------------------
// Snapshot building
// ------------------------------------------------------------------

Json::Value AgentStateTracker::buildDataSnapshot(int dataSetId) const
{
	Json::Value data(Json::objectValue);

	auto * ws = DataSetPackage::pkg() ? DataSetPackage::pkg()->workspace() : nullptr;
	DataSet * ds = ws ? (dataSetId >= 0 ? ws->dataSetById(dataSetId) : ws->shownDataSet()) : nullptr;
	if (!ds)
	{
		data["loaded"] = false;
		return data;
	}

	data["loaded"]   = true;
	data["dataSetId"]= ds->id();
	data["rowCount"] = static_cast<int>(ds->rowCount());

	Json::Value columns(Json::arrayValue);
	auto colTypes = ds->getColumnTypesMap();
	for (const auto & [name, type] : colTypes)
	{
		Json::Value col;
		col["name"] = name;
		col["type"] = columnTypeToString(type);
		columns.append(col);
	}
	data["columns"] = columns;

	return data;
}

Json::Value AgentStateTracker::buildSingleAnalysis(int analysisId,
                                                    bool includeOptions,
                                                    bool includeResults,
                                                    bool includeDescriptions,
                                                    bool useDelta) const
{
	Json::Value entry(Json::objectValue);

	Analysis * a = Analyses::analyses()->get(static_cast<size_t>(analysisId));
	if (!a)
	{
		entry["id"]     = analysisId;
		entry["status"] = "removed";
		return entry;
	}

	entry["id"]     = analysisId;
	entry["name"]   = a->name();
	entry["module"] = a->module();
	entry["status"] = Analysis::statusToString(a->status());
	entry["dataSetId"] = a->dataSet() ? a->dataSet()->id() : -1;

	if (includeOptions)
		// writeOptionsDelta writes both "options" and the optionMeta delta/full.
		Analyses::writeOptionsDelta(entry, a, includeDescriptions, useDelta);

	if (includeResults && a->isFinished())
	{
		Json::Value results = a->results();
		Analyses::stripResults(results);
		entry["results"] = results;
	}
	else
		entry["results"] = Json::nullValue;

	return entry;
}

Json::Value AgentStateTracker::buildWorkspaceSnapshot(bool includeResults) const
{
	Json::Value snapshot(Json::objectValue);

	// --- Data ---
	// Backward-compatible alias for the shown/active dataset.
	snapshot["data"] = buildDataSnapshot();

	// --- All datasets (workspace-aware) ---
	Workspace * ws = DataSetPackage::pkg() ? DataSetPackage::pkg()->workspace() : nullptr;
	if (ws)
	{
		Json::Value datasets(Json::arrayValue);
		DataSet * shown = ws->shownDataSet();
		for (DataSet * ds : ws->dataSets())
		{
			Json::Value d = buildDataSnapshot(ds->id());
			d["dataSetId"]   = ds->id();
			d["title"]       = ds->title().toStdString();
			d["name"]        = ds->name().toStdString();
			d["isComputed"]  = ds->isComputed();
			d["active"]      = (ds == shown);
			datasets.append(d);
		}
		snapshot["datasets"] = datasets;
		snapshot["activeDataSetId"] = shown ? shown->id() : Json::nullValue;
	}

	// --- Analyses (all) ---
	Json::Value analyses(Json::arrayValue);
	if (auto * ans = Analyses::analyses())
	{
		ans->applyToAll([&](Analysis * a)
		{
			analyses.append(buildSingleAnalysis(
				static_cast<int>(a->id()),
				true,   // includeOptions
				includeResults,
				false)); // includeDescriptions — not needed for snapshots
		});
	}
	snapshot["analyses"] = analyses;

	// --- Active analysis ---
	if (auto * ans = Analyses::analyses())
	{
		int curIdx = ans->currentAnalysisIndex();
		if (curIdx >= 0 && curIdx < ans->count())
		{
			if (Analysis * active = (*ans)[static_cast<size_t>(curIdx)])
				snapshot["activeAnalysisId"] = static_cast<int>(active->id());
		}
	}

	return snapshot;
}

Json::Value AgentStateTracker::buildAnalysesSnapshot(const std::vector<int> & analysisIds,
                                                      bool includeOptions,
                                                      bool includeResults,
                                                      bool includeDescriptions,
                                                      bool useDelta) const
{
	Json::Value snapshot(Json::objectValue);

	Json::Value analyses(Json::arrayValue);
	Json::Value missing(Json::arrayValue);

	for (int id : analysisIds)
	{
		Analysis * a = Analyses::analyses()->get(static_cast<size_t>(id));
		if (!a)
		{
			missing.append(id);
			continue;
		}

		analyses.append(buildSingleAnalysis(id, includeOptions, includeResults, includeDescriptions, useDelta));
	}

	snapshot["analyses"] = analyses;
	if (!missing.empty())
		snapshot["missing"] = missing;

	return snapshot;
}
