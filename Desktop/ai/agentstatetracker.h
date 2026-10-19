//
// AgentStateTracker — Tracks workspace changes for the AI agent.
//
// The AI agent (embedded via AiBridge, or external via MCP) needs to know when
// the JASP workspace changes outside its control: when the user edits an
// analysis option in the UI, when a column is added/removed/renamed, when an
// analysis completes or is deleted.
//
// This singleton hooks into Analyses and DataSetPackage signals and
// accumulates granular "dirty" flags per analysis (Options / Status / Results
// / Added / Removed) and for data columns (added / removed / changed / renamed).
//
// The dispatcher queries isDirty() on every RPC response.  When dirty, it
// calls buildWorkspaceSnapshot() to produce a full current-state snapshot
// (all analyses' options + results + status, plus the data column schema),
// attaches it as `_stateUpdate` on read-method results or as `data` on a
// `-32001` divergence error for mutation methods, then calls markClean() to
// advance the baseline.
//
// Baseline semantics:
//   Dirty flags are set when user/background changes happen and cleared when
//   the agent observes state — either by receiving a snapshot (dispatcher
//   calls markClean) or by calling get_analyses_state / analysis_run /
//   analysis_results / analysis_create (handlers call notifyAnalysisObserved
//   / notifyDataObserved).
//
// Singleton lifetime: created in MainWindow (always) and AiBridge.
//

#ifndef AGENTSTATETRACKER_H
#define AGENTSTATETRACKER_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QMap>

#include <set>
#include <map>
#include <vector>

#include <json/json.h>

class Analysis;

class AgentStateTracker : public QObject
{
	Q_OBJECT

public:
	/// Categories of change for a single analysis.
	enum class AnalysisChange
	{
		Options,   ///< User modified options (userModifiedSomething fired)
		Status,    ///< Analysis status changed (running, complete, error, …)
		Results,   ///< Results JSON changed (re-run, image edit, …)
		Added,     ///< New analysis appeared
		Removed    ///< Analysis was deleted
	};

	static AgentStateTracker * tracker() { return _singleton; }

	/// Lazily create the singleton and connect to Analyses + DataSetPackage
	/// signals.  Safe to call multiple times; only the first call connects.
	static void init();

	// ------------------------------------------------------------------
	// Null-safe convenience wrappers — these check _singleton internally.
	// Use these from RPC handlers; use the instance methods only when you
	// already hold a valid tracker pointer.
	// ------------------------------------------------------------------

	/// Mark that the agent has observed an analysis's state (clears dirty flags).
	static void notifyAnalysisObserved(size_t analysisId);

	/// Mark that the agent has observed the dataset (clears data dirty flags).
	static void notifyDataObserved();

	// ------------------------------------------------------------------
	// Dirty-marking — called by signal hooks
	// ------------------------------------------------------------------

	void markAnalysisOptionsChanged(size_t analysisId);
	void markAnalysisStatusChanged(size_t analysisId);
	void markAnalysisResultsChanged(size_t analysisId);
	void markAnalysisAdded(size_t analysisId);
	void markAnalysisRemoved(size_t analysisId);

	void markDataChanged(const QStringList & added,
	                     const QStringList & removed,
	                     const QStringList & changed,
	                     const QMap<QString, QString> & renamed);

	// ------------------------------------------------------------------
	// Baseline clearing — called when the agent observes state
	// ------------------------------------------------------------------

	void afterAnalysisObserved(size_t analysisId);
	void afterDataObserved();

	/// Clear all dirty flags (analysis + data).
	void clearAll();

	// ------------------------------------------------------------------
	// Dirty query + snapshot building
	// ------------------------------------------------------------------

	/// True if any analysis or data dirty flags are set.
	bool isDirty() const;

	/// True if any USER-INDUCED dirty flags are set (Options, Added, Removed,
	/// or Data changes).  Excludes background evolution (Status, Results) which
	/// should be reported via _stateUpdate but should NOT block mutations.
	bool isUserDiverged() const;

	/// Clear all dirty flags without building a snapshot.
	/// Called by the dispatcher after it attaches a `_stateUpdate` to a
	/// response, or after a divergence error is delivered.
	void markClean();

	/// Full workspace snapshot: every analysis (options + results + status)
	/// plus the complete data column schema.
	/// @param includeResults  if false, `results` is set to null for each
	///                         analysis (useful when only options matter).
	Json::Value buildWorkspaceSnapshot(bool includeResults = true) const;

	/// Filtered snapshot for get_analyses_state: only the requested analyses.
	/// @param useDelta  if true (default), optionMeta is sent as a diff against
	///                  the last baseline; if false, the full optionMeta is sent.
	Json::Value buildAnalysesSnapshot(const std::vector<int> & analysisIds,
	                                  bool includeOptions,
	                                  bool includeResults,
	                                  bool includeDescriptions,
	                                  bool useDelta = true) const;

	/// Current dataset column schema (name + type per column).
	Json::Value buildDataSnapshot(int dataSetId = -1) const;

private:
	explicit AgentStateTracker(QObject * parent = nullptr);

	void connectHooks();

	/// Build a single analysis entry.  Shared by buildWorkspaceSnapshot and
	/// buildAnalysesSnapshot.
	/// @param useDelta  if true, optionMeta is diffed; if false, full meta is sent.
	Json::Value buildSingleAnalysis(int analysisId,
	                                bool includeOptions,
	                                bool includeResults,
	                                bool includeDescriptions,
	                                bool useDelta = true) const;

	struct AnalysisState
	{
		std::set<AnalysisChange> dirtyFlags;
	};
	std::map<size_t, AnalysisState> _analysisStates;

	struct DataState
	{
		bool                  dirty     = false;
		QStringList           added;
		QStringList           removed;
		QStringList           changed;   ///< columns whose type/values changed
		QMap<QString,QString> renamed;   ///< old name → new name
	} _dataState;

	static AgentStateTracker * _singleton;
	bool _hooksConnected = false;
};

#endif // AGENTSTATETRACKER_H
