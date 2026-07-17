//
// RoboReportManager implementation — see roboreportmanager.h for API docs.
//

#include "roboreportmanager.h"

#include "analysis/analyses.h"
#include "analysis/analysis.h"
#include "engine/enginesync.h"
#include "engine/enginerepresentation.h"
#include "mainwindow.h"
#include "modules/dynamicmodules.h"
#include "rpc/jasprpcserver.h"

#include "log.h"
#include "dirs.h"
#include "utilities/messageforwarder.h"

#include <QString>
#include <QFile>
#include <QMessageBox>

RoboReportManager* RoboReportManager::_singleton = nullptr;

RoboReportManager::RoboReportManager(QObject* parent)
	: QObject(parent)
{
}

void RoboReportManager::init(QObject* parent)
{
	if (_singleton)
		return;

	_singleton = new RoboReportManager(parent);
	Log::log() << "[RoboReport] Manager initialized." << std::endl;
}

bool RoboReportManager::hasScript(const std::string& module, const std::string& analysis)
{
	if (!_singleton)
		return false;

	return !_singleton->_resolveScriptPath(module, analysis).isEmpty();
}

void RoboReportManager::runForAnalysis(int analysisId)
{
	if (!_singleton)
	{
		Log::log() << "[RoboReport] runForAnalysis called before init — ignoring analysis "
				  << analysisId << std::endl;
		return;
	}

	_singleton->_runForAnalysis(analysisId);
}

void RoboReportManager::_runForAnalysis(int analysisId)
{
	// 1. Resolve the analysis.
	Analysis* a = Analyses::analyses()->get(static_cast<size_t>(analysisId));
	if (!a)
	{
		Log::log() << "[RoboReport] Analysis " << analysisId << " not found." << std::endl;
		emit scriptFinished(analysisId, false, "Analysis not found");
		return;
	}

	std::string module = a->module();
	std::string name   = a->name();

	// 2. Resolve the script path.
	QString scriptPath = _resolveScriptPath(module, name);
	if (scriptPath.isEmpty())
	{
		Log::log() << "[RoboReport] No script for " << module << "::" << name
				  << " (analysis " << analysisId << ")." << std::endl;
		MessageForwarder::showWarning(tr("RoboReport"),
			tr("No RoboReport script is available for %1::%2.").arg(
				QString::fromStdString(module), QString::fromStdString(name)),
			QMessageBox::Icon::Information);
		emit scriptFinished(analysisId, false,
			QString::fromStdString("No RoboReport script available for " + module + "::" + name));
		return;
	}

	// 2b. Parse metadata + log version info (non-blocking).
	ScriptMetadata meta = _parseScriptMetadata(scriptPath);
	_checkVersion(meta, module);

	// 3. Ensure the RPC server is listening (the script talks to it over HTTP).
	quint16 port = _ensureRpcServer();
	if (port == 0)
	{
		Log::log() << "[RoboReport] Failed to start the JASP-RPC server." << std::endl;
		MessageForwarder::showWarning(tr("RoboReport"),
			tr("Could not start the JASP-RPC server required for report generation."));
		emit scriptFinished(analysisId, false, "JASP-RPC server failed to start");
		return;
	}

	// 4. Ensure the RCmdEngine exists and its signals are wired.
	_ensureEngine();
	if (!_engine)
	{
		Log::log() << "[RoboReport] No RCmdEngine available." << std::endl;
		MessageForwarder::showWarning(tr("RoboReport"),
			tr("Could not start the R engine required for report generation."));
		emit scriptFinished(analysisId, false, "RCmdEngine not available");
		return;
	}

	// 5. Guard against contention with RCommander — they share the single engine.
	if (!_engine->idle() || _activeId >= 0)
	{
		Log::log() << "[RoboReport] RCmdEngine busy — cannot run RoboReport for analysis "
				  << analysisId << " right now." << std::endl;
		MessageForwarder::showWarning(tr("RoboReport"),
			tr("The R engine is currently busy. Please wait for any running R commands to finish and try again."));
		emit scriptFinished(analysisId, false, "R engine is busy");
		return;
	}

	// 6. Fire the script.
	QString wrapper = _buildRWrapper(scriptPath, analysisId, "127.0.0.1", port);

	Log::log() << "[RoboReport] Launching script for " << module << "::" << name
			  << " (analysis " << analysisId << "):\n"
			  << wrapper.toStdString() << std::endl;

	_activeId = analysisId;
	emit scriptStarted(analysisId);

	_engine->runScriptOnProcess(wrapper);
}

QString RoboReportManager::_resolveScriptPath(const std::string& module, const std::string& analysis) const
{
	QString analysisFile = QString::fromStdString(analysis + ".R");

	// 1. Preferred: module package's scripts/roboreport/ folder.
	//    This lives in inst/scripts/roboreport/ in the module source and
	//    becomes <module_library>/<module>/scripts/roboreport/ after install.
	if (DynamicModules::dynMods())
	{
		Modules::DynamicModule* dynMod = DynamicModules::dynMods()->dynamicModule(module);
		if (dynMod)
		{
			QString path = QString::fromStdString(dynMod->moduleInstFolder())
			             + "scripts/roboreport/" + analysisFile;
			if (QFile::exists(path))
				return path;
		}
	}

	// 2. Fallback: Resources/roboreport/<module>/<AnalysisName>.R (dev/test).
	QString fallback = QString::fromStdString(
		Dirs::resourcesDir() + "roboreport/" + module + "/" + analysis + ".R");
	if (QFile::exists(fallback))
		return fallback;

	return {};
}

RoboReportManager::ScriptMetadata RoboReportManager::_parseScriptMetadata(const QString& path) const
{
	ScriptMetadata meta;

	QFile f(path);
	if (!f.open(QIODevice::ReadOnly | QIODevice::Text))
		return meta;

	// Metadata lives in the comment block at the top of the file.
	// Only scan the first 30 lines.
	for (int i = 0; i < 30 && !f.atEnd(); ++i)
	{
		QString line = QString::fromUtf8(f.readLine());

		// Only parse comment lines.
		if (!line.startsWith('#'))
			break;

		// Strip the leading '#' characters and whitespace.
		QString content = line.mid(line.indexOf('#') + 1);
		while (content.startsWith('#') || content.startsWith(' ') || content.startsWith('-'))
			content = content.mid(1);
		content = content.trimmed();

		// "RoboReport Script: <name>" — the script title line.
		if (content.startsWith("RoboReport Script:", Qt::CaseInsensitive))
			meta.name = content.mid(18).trimmed();
		else if (content.startsWith("Target:", Qt::CaseInsensitive))
			meta.target = content.mid(7).trimmed();
		else if (content.startsWith("Version:", Qt::CaseInsensitive))
			meta.version = content.mid(8).trimmed();
		else if (content.startsWith("Description:", Qt::CaseInsensitive))
			meta.description = content.mid(12).trimmed();
	}

	return meta;
}

void RoboReportManager::_checkVersion(const ScriptMetadata& meta, const std::string& module) const
{
	if (meta.version.isEmpty())
		return;

	if (!DynamicModules::dynMods())
		return;

	Modules::DynamicModule* dynMod = DynamicModules::dynMods()->dynamicModule(module);
	if (!dynMod)
		return;

	QString moduleVersion = QString::fromStdString(dynMod->version().asString());

	// Non-blocking: just log. The script runs regardless.
	// A full version-constraint parser can be added later.
	Log::log() << "[RoboReport] Script \"" << meta.name.toStdString()
	          << "\" requires " << meta.version.toStdString()
	          << ", module " << module << " is "
	          << moduleVersion.toStdString() << std::endl;
}

void RoboReportManager::_ensureEngine()
{
	if (_engine)
		return;

	_engine = EngineSync::singleton()->createRCmdEngine();
	if (!_engine)
	{
		Log::log() << "[RoboReport] createRCmdEngine() returned null." << std::endl;
		return;
	}

	if (!_wired)
	{
		// The RCmdEngine emits rCodeReturnedLog (NOT rCodeReturned) when R
		// code finishes — see EngineRepresentation::processRCodeReply():
		//   if(runsRCmd()) emit rCodeReturnedLog(...)  else emit rCodeReturned(...)
		//
		// Error handling: the engine captures R errors via jaspRCPP_getLastErrorMsg()
		// and reports them through hasError=true with the error message in `log`.
		// Script authors signal failure by calling stop("message").
		connect(_engine, &EngineRepresentation::rCodeReturnedLog, this,
			[this](const QString& log, bool hasError)
			{
				if (_activeId < 0)
					return;

				emit scriptOutput(_activeId, log);

				int finishedId = _activeId;
				_activeId = -1;

				if (hasError)
				{
					Log::log() << "[RoboReport] Script failed for analysis " << finishedId
					          << ": " << log.toStdString() << std::endl;
					MessageForwarder::showWarning(tr("RoboReport failed"),
						tr("The RoboReport script failed with the following error:\n\n%1").arg(log.trimmed()));
					emit scriptFinished(finishedId, false, log.trimmed());
				}
				else
				{
					Log::log() << "[RoboReport] Script completed for analysis " << finishedId << std::endl;
					emit scriptFinished(finishedId, true, QString());
				}
			});

		// rCodeReturned fires for non-RCmd engines (analysis engines).
		// Keep a connection as a safety net — unlikely to fire but harmless.
		connect(_engine, &EngineRepresentation::rCodeReturned, this,
			[this](const QString& result, int requestId, bool hasError)
			{
				if (_activeId < 0)
					return;
				emit scriptOutput(_activeId, result);
			});

		_wired = true;
	}
}

quint16 RoboReportManager::_ensureRpcServer()
{
	MainWindow* mw = MainWindow::singleton();
	if (!mw || !mw->rpcServer())
	{
		Log::log() << "[RoboReport] No MainWindow/RPC server available." << std::endl;
		return 0;
	}

	JaspRpcServer* srv = mw->rpcServer();
	quint16 port = srv->serverPort();
	if (port != 0)
		return port; // already listening

	if (!srv->start())
	{
		Log::log() << "[RoboReport] JaspRpcServer::start() failed." << std::endl;
		return 0;
	}

	port = srv->serverPort();
	Log::log() << "[RoboReport] Started JASP-RPC server on port " << port << std::endl;
	return port;
}

QString RoboReportManager::_buildRWrapper(const QString& scriptPath, int analysisId,
                                          const QString& rpcHost, quint16 rpcPort) const
{
	// Escape single quotes in the path for safe embedding in R single-quoted string.
	QString escapedPath = scriptPath;
	escapedPath.replace("'", "\\'");

	// Build typed R that calls run_script() directly (Decision #14: no env vars).
	// R errors propagate naturally: the engine captures them via
	// jaspRCPP_getLastErrorMsg() and reports hasError=true in rCodeReturnedLog.
	return QString(
		"jaspRoboReport::run_script(\n"
		"  path       = '%1',\n"
		"  analysisId = %2L,\n"
		"  rpcHost    = '%3',\n"
		"  rpcPort    = %4L\n"
		")\n"
	).arg(escapedPath)
	 .arg(analysisId)
	 .arg(rpcHost)
	 .arg(rpcPort);
}
