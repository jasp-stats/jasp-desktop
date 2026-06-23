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
	auto * pkg = DataSetPackage::pkg();
	if (pkg)
	{
		connect(pkg, &DataSetPackage::datasetChanged, this,
			[this](QStringList changedColumns,
			       QStringList missingColumns,
			       QMap<QString, QString> changeNameColumns,
			       bool rowCountChanged,
			       bool hasNewColumns)
		{
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
	// Results changes are a separate category from Status — the agent needs
	// to know whether to re-fetch results (e.g. analysis completed, image edited).
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

	// Apply renames to names we're already tracking, so the status block
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
	// If the old name was in `added`, it's already updated above and we
	// don't report a separate rename — the column just appears in `added`
	// with its real name.
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

void AgentStateTracker::clearEphemeralFlags()
{
	// Clear Added/Removed from all analysis entries; remove entries that
	// become empty (only had ephemeral flags).
	for (auto it = _analysisStates.begin(); it != _analysisStates.end(); )
	{
		it->second.dirtyFlags.erase(AnalysisChange::Added);
		it->second.dirtyFlags.erase(AnalysisChange::Removed);
		if (it->second.dirtyFlags.empty())
			it = _analysisStates.erase(it);
		else
			++it;
	}

	// Clear all data changes
	afterDataObserved();
}

// ------------------------------------------------------------------
// Query
// ------------------------------------------------------------------

bool AgentStateTracker::hasPendingChanges() const
{
	if (_dataState.dirty) return true;

	for (const auto & [id, state] : _analysisStates)
		if (!state.dirtyFlags.empty())
			return true;

	return false;
}

Json::Value AgentStateTracker::buildStatusBlock()
{
	Json::Value result(Json::objectValue);

	// --- Data ---
	if (_dataState.dirty)
	{
		Json::Value data(Json::objectValue);

		if (!_dataState.added.isEmpty())
		{
			Json::Value arr(Json::arrayValue);
			for (const auto & s : _dataState.added) arr.append(s.toStdString());
			data["added"] = arr;
		}
		if (!_dataState.removed.isEmpty())
		{
			Json::Value arr(Json::arrayValue);
			for (const auto & s : _dataState.removed) arr.append(s.toStdString());
			data["removed"] = arr;
		}
		if (!_dataState.changed.isEmpty())
		{
			Json::Value arr(Json::arrayValue);
			for (const auto & s : _dataState.changed) arr.append(s.toStdString());
			data["changed"] = arr;
		}
		if (!_dataState.renamed.isEmpty())
		{
			Json::Value obj(Json::objectValue);
			for (auto it = _dataState.renamed.begin(); it != _dataState.renamed.end(); ++it)
				obj[it.key().toStdString()] = it.value().toStdString();
			data["renamed"] = obj;
		}

		result["data"] = data;
	}

	// --- Analyses ---
	Json::Value analyses(Json::arrayValue);
	for (const auto & [id, state] : _analysisStates)
	{
		if (state.dirtyFlags.empty())
			continue;

		bool isRemoved = state.dirtyFlags.count(AnalysisChange::Removed);

		// Skip stale dirty flags for analyses that no longer exist
		if (!isRemoved && !Analyses::analyses()->get(id))
			continue;

		Json::Value entry(Json::objectValue);
		entry["id"] = static_cast<int>(id);

		Json::Value flags(Json::arrayValue);

		for (auto flag : state.dirtyFlags)
		{
			std::string name;
			switch (flag)
			{
				case AnalysisChange::Options: name = "options"; break;
				case AnalysisChange::Status:   name = "status";  break;
				case AnalysisChange::Results:  name = "results"; break;
				case AnalysisChange::Added:    name = "added";   break;
				case AnalysisChange::Removed:  name = "removed"; break;
			}
			flags.append(name);
		}

		// Include current status for analyses that still exist
		if (!isRemoved)
		{
			if (auto * a = Analyses::analyses()->get(id))
				entry["status"] = Analysis::statusToString(a->status());
		}

		entry["changed"] = flags;
		analyses.append(entry);
	}

	if (!analyses.empty())
		result["analyses"] = analyses;

	// --- Active analysis ---
	if (auto * ans = Analyses::analyses())
	{
		int curIdx = ans->currentAnalysisIndex();
		if (curIdx >= 0 && curIdx < ans->count())
		{
			if (Analysis * active = (*ans)[static_cast<size_t>(curIdx)])
				result["activeAnalysisId"] = static_cast<int>(active->id());
		}
	}

	clearEphemeralFlags();

	return result;
}

QString AgentStateTracker::buildStatusText()
{
	QStringList lines;

	// --- Data ---
	if (_dataState.dirty)
	{
		QStringList parts;

		if (!_dataState.added.isEmpty())
			parts << QStringLiteral("added=[") + _dataState.added.join(QStringLiteral(", ")) + QStringLiteral("]");

		if (!_dataState.removed.isEmpty())
			parts << QStringLiteral("removed=[") + _dataState.removed.join(QStringLiteral(", ")) + QStringLiteral("]");

		if (!_dataState.changed.isEmpty())
			parts << QStringLiteral("changed=[") + _dataState.changed.join(QStringLiteral(", ")) + QStringLiteral("]");

		if (!_dataState.renamed.isEmpty())
		{
			QStringList renames;
			for (auto it = _dataState.renamed.begin(); it != _dataState.renamed.end(); ++it)
				renames << it.key() + QStringLiteral("→") + it.value();
			parts << QStringLiteral("renamed=[") + renames.join(QStringLiteral(", ")) + QStringLiteral("]");
		}

		if (!parts.isEmpty())
			lines << QStringLiteral("data: ") + parts.join(QStringLiteral(", "));
	}

	// --- Analyses ---
	for (const auto & [id, state] : _analysisStates)
	{
		if (state.dirtyFlags.empty())
			continue;

		bool isRemoved = state.dirtyFlags.count(AnalysisChange::Removed);

		// Skip stale dirty flags for analyses that no longer exist
		if (!isRemoved && !Analyses::analyses()->get(id))
			continue;

		// Build "Changed:" label from change categories
		QStringList changed;
		if (state.dirtyFlags.count(AnalysisChange::Added))    changed << QStringLiteral("added");
		if (state.dirtyFlags.count(AnalysisChange::Removed))  changed << QStringLiteral("removed");
		if (state.dirtyFlags.count(AnalysisChange::Options))  changed << QStringLiteral("options");
		if (state.dirtyFlags.count(AnalysisChange::Results))  changed << QStringLiteral("results");
		if (state.dirtyFlags.count(AnalysisChange::Status))   changed << QStringLiteral("status");

		QStringList parts;
		if (!changed.isEmpty())
			parts << QStringLiteral("Changed: ") + changed.join(QStringLiteral(", "));

		// Current status for existing analyses
		if (!isRemoved)
		{
			if (auto * a = Analyses::analyses()->get(id))
				parts << QStringLiteral("Status: ") + QString::fromStdString(Analysis::statusToString(a->status()));
		}

		lines << QStringLiteral("analysis ") + QString::number(static_cast<int>(id))
		        + QStringLiteral(": ") + parts.join(QStringLiteral(", "));
	}

	// --- Active analysis ---
	if (auto * ans = Analyses::analyses())
	{
		int curIdx = ans->currentAnalysisIndex();
		if (curIdx >= 0 && curIdx < ans->count())
		{
			if (Analysis * active = (*ans)[static_cast<size_t>(curIdx)])
				lines << QStringLiteral("active: ") + QString::number(static_cast<int>(active->id()));
		}
	}

	if (lines.isEmpty())
		return QString();

	clearEphemeralFlags();

	return QStringLiteral("<jasp_status>\n") + lines.join(QStringLiteral("\n"))
	     + QStringLiteral("\n</jasp_status>");
}

// ------------------------------------------------------------------
// Display helper
// ------------------------------------------------------------------

QString AgentStateTracker::stripStatusBlock(const QString & text)
{
	static const QRegularExpression rx(
		QStringLiteral("\\s*<jasp_status>.*?</jasp_status>\\s*"),
		QRegularExpression::DotMatchesEverythingOption);

	QString result = text;
	result.remove(rx);
	return result.trimmed();
}
