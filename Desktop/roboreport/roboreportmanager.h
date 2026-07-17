//
// RoboReportManager — launches deterministic R "RoboReport" scripts.
//
// RoboReport scripts are R files that drive the JASP RPC toolset (the same
// JSON-RPC methods the AI agent uses) to generate annotated statistical
// reports.
//
// Script resolution order (first match wins):
//   1. <module package>/scripts/roboreport/<AnalysisName>.R   (preferred)
//   2. Resources/roboreport/<module>/<AnalysisName>.R          (fallback)
//
// Each script defines `roboreport_main(analysisId)` and is executed by
// `jaspRoboReport::run_script()` inside the shared RCmdEngine.
//
// This singleton:
//   - Resolves (module, analysis) -> script path by convention.
//   - Ensures the JaspRpcServer is listening (scripts talk to it over HTTP).
//   - Ensures the RCmdEngine exists (shared with RCommander via EngineSync).
//   - Generates typed R code that calls run_script() and fires it.
//   - Streams script output via signals.
//
// Modeled on AgentStateTracker (Desktop/ai/agentstatetracker.h) and the
// RCommander engine-driving pattern (Desktop/qquick/rcommander.cpp).
//
// Lifetime: created in MainWindow. Always exists; idempotent init.
//

#ifndef ROBOREPORTMANAGER_H
#define ROBOREPORTMANAGER_H

#include <QObject>
#include <QString>

class EngineRepresentation;

class RoboReportManager : public QObject
{
	Q_OBJECT

public:
	/// Null-safe accessor. Returns nullptr before init() / after shutdown.
	static RoboReportManager* manager() { return _singleton; }

	/// Lazily create the singleton. Safe to call multiple times.
	/// @param parent  Optional QObject parent (e.g. MainWindow).
	static void init(QObject* parent = nullptr);

	/// Null-safe entry point. Resolves the script for the given analysis,
	/// ensures the engine + RPC server are ready, and fires the script.
	/// No-ops (with a log line) if no script exists or the singleton is null.
	static void runForAnalysis(int analysisId);

	/// True if a RoboReport script exists for (module, analysis).
	static bool hasScript(const std::string& module, const std::string& analysis);

signals:
	/// Emitted when a script begins executing for the given analysis.
	void scriptStarted(int analysisId);

	/// Emitted for each line/block of R output (from rCodeReturnedLog).
	void scriptOutput(int analysisId, const QString& line);

	/// Emitted when the script finishes. success is false if the engine
	/// reported an error or a ROBOREPORT_ERROR sentinel was seen.
	void scriptFinished(int analysisId, bool success, const QString& errorMsg);

private:
	explicit RoboReportManager(QObject* parent);

	/// Instance-level run (assumes _singleton != nullptr).
	void _runForAnalysis(int analysisId);

	/// Resolve the script path for (module, analysis).
	/// Tries module package scripts/ folder first, then Resources/ fallback.
	/// Returns empty string if not found.
	QString _resolveScriptPath(const std::string& module, const std::string& analysis) const;

	/// Parse metadata from the script header (Name, Target, Version, Description).
	struct ScriptMetadata { QString name, target, version, description; };
	ScriptMetadata _parseScriptMetadata(const QString& path) const;

	/// Log a warning if the script's version constraint isn't met by the module.
	/// Non-blocking — the script still runs regardless.
	void _checkVersion(const ScriptMetadata& meta, const std::string& module) const;

	/// Lazily create the RCmdEngine (via EngineSync) and wire its signals once.
	void _ensureEngine();

	/// Ensure the JaspRpcServer is listening; start it if necessary.
	/// Returns the port in use, or 0 on failure.
	quint16 _ensureRpcServer();

	/// Build the R wrapper code that calls jaspRoboReport::run_script().
	QString _buildRWrapper(const QString& scriptPath, int analysisId,
	                       const QString& rpcHost, quint16 rpcPort) const;

	EngineRepresentation*	_engine		= nullptr;
	bool					_wired		= false;	///< signals connected to _engine
	int						_activeId	= -1;		///< analysisId currently running, or -1

	static RoboReportManager* _singleton;
};

#endif // ROBOREPORTMANAGER_H
