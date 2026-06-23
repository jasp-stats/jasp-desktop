//
// AgentStateTracker — Tracks workspace changes for the AI agent.
//
// The AI agent (embedded via AiBridge, or external via MCP) needs to know when
// the JASP workspace changes outside its control: when the user edits an
// analysis option in the UI, when a column is added/removed/renamed, when an
// analysis completes or is deleted.
//
// This singleton hooks into Analyses and DataSetPackage signals, accumulates
// "dirty" flags, and exposes two query styles:
//
//   - buildStatusText():  compact semi-structured text for AiBridge to inject
//                         as a <jasp_status> postfix on the latest user message.
//   - buildStatusBlock(): structured JSON for the poll_state_change RPC method
//                         (external MCP agents).
//
//
// Baseline semantics:
//   Ephemeral notifications (Added, Removed, data column changes) are cleared
//   automatically after appearing in ONE status block delivery.  They are
//   one-time events — once the agent has been told, it knows.
//
//   Persistent flags (Options, Status, Results) survive until the agent
//   actually fetches current state via get_analyses_state, analysis_run,
//   analysis_results, or analysis_create — at which point
//   afterAnalysisObserved(id) clears them.
//
// Singleton lifetime: created in AiBridge or MainWindow, hooks signals lazily.
//

#ifndef AGENTSTATETRACKER_H
#define AGENTSTATETRACKER_H

#include <QObject>
#include <QString>
#include <QStringList>
#include <QMap>
#include <QSet>
#include <QRegularExpression>

#include <set>
#include <map>

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
		Added,     ///< New analysis appeared (ephemeral)
		Removed    ///< Analysis was deleted (ephemeral)
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

	/// Clear all dirty flags without the agent having to observe each one.
	/// Used on clearChat / conversation reset.
	void clearAll();

	// ------------------------------------------------------------------
	// Query — builds the status block and clears ephemeral flags.
	// ------------------------------------------------------------------

	/// True if any analysis or data dirty flags are set.
	bool hasPendingChanges() const;

	/// Structured JSON diff for the poll_state_change RPC method.
	/// Returns an empty object when nothing changed.
	/// Clears ephemeral flags (Added, Removed, data changes) after building.
	Json::Value buildStatusBlock();

	/// Compact semi-structured text for AiBridge's <jasp_status> postfix.
	/// Returns an empty string when nothing changed.
	/// Clears ephemeral flags (Added, Removed, data changes) after building.
	QString buildStatusText();

	// ------------------------------------------------------------------
	// Display helper
	// ------------------------------------------------------------------

	/// Strip a <jasp_status>…</jasp_status> block (and surrounding whitespace)
	/// from a user message.  Used when pushing message content to deep-chat.
	static QString stripStatusBlock(const QString & text);

private:
	explicit AgentStateTracker(QObject * parent = nullptr);

	void connectHooks();

	/// Clear ephemeral flags: Added, Removed (analyses) and all data changes.
	/// Called by buildStatusText / buildStatusBlock after a successful delivery.
	void clearEphemeralFlags();

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
