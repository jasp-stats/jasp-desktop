//
// Copyright (C) 2013-2025 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#include "syntaxbridge_interface.h"
#include "syntaxbridge.h"
#include <QQmlApplicationEngine>
#include <QQmlContext>
#include <QFileInfo>
#include <QQmlComponent>
#include <QQuickItem>
#include <QCoreApplication>
#include <QEvent>
#include <QDir>
#include <QThread>
#include <QQmlIncubator>
#include "controls/jaspcontrol.h"
#include "datasetprovider.h"
#include "databridge.h"
#include "rbridge.h"
#include "processinfo.h"
#include "tempfiles.h"
#include "analysisbase.h"
#include "analysisform.h"
#include "log.h"
#include "utilities/qmlutils.h"
#include "dirs.h"
#include "utilities/appdirs.h"
#include "modules/dynamicmodule.h"
#include "archivereader.h"
#include "databaseinterface.h"

#include <string>
#include <vector>

#include <QtPlugin>
#ifdef USE_QT_STATIC_LIBS
Q_IMPORT_PLUGIN(QMinimalIntegrationPlugin)
#endif
Q_IMPORT_PLUGIN(JASP_ControlsPlugin)

#define _STRINGIZE(x) #x
#define STRINGIZE(x) _STRINGIZE(x)

static bool									gl_initialized					= false;
static bool									gl_initializedDbInMemory		= false;
static bool									gl_rBridgeInitialized			= false;
static bool									gl_jaspBaseInitialized			= false;
static QGuiApplication			*			gl_application					= nullptr;
static QQmlEngine				*			gl_qmlEngine					= nullptr;
static DataBridge				*			gl_dataBridge					= nullptr;
static ColumnEncoder			*			gl_extraEncodings				= nullptr;
static QMap<QString, std::pair<QDateTime, AnalysisForm* > >	gl_qmlFormMap;
static int									gl_applicationArgc				= 0;
static std::vector<std::string>				gl_applicationArgvStorage;
static std::vector<char*>					gl_applicationArgv;

static bool									gl_verbose						=
#ifdef JASP_DEBUG
	true;
#else
	false;
#endif

static std::string							gl_param_resultFont				=
#ifdef WIN32
	"Arial,sans-serif,freesans,\"Segoe UI\"";
#elif __APPLE__
	"\"Lucida Grande\",Helvetica,Arial,sans-serif,\"Helvetica Neue\",freesans";
#else
	"freesans,sans-serif";
#endif

static bool readJaspJsonEntry(Json::Value & root, const char * filePath, const char * entry, std::string * error = nullptr)
{
	try
	{
		if (!filePath || std::string(filePath).empty())
		{
			if (error)
				*error = "Cannot read from an empty JASP archive path.";
			return false;
		}

		ArchiveReader reader(filePath, entry);
		int errorCode = 0;
		std::string json = reader.readAllData(sizeof(char), errorCode);
		if (errorCode != 0)
		{
			if (error)
				*error = std::string("Could not read entry ") + entry + " from JASP archive " + filePath + ".";
			else
				Log::log() << "Could not read JASP archive entry." << std::endl;
			return false;
		}

		Json::Reader parser;
		if (!parser.parse(json, root))
		{
			if (error)
				*error = std::string("Could not parse entry ") + entry + " from JASP archive " + filePath + ".";
			else
				Log::log() << "Could not parse JASP archive entry." << std::endl;
			return false;
		}
		return true;
	}
	catch (const std::exception & exception)
	{
		if (error)
			*error = std::string("Could not read entry ") + entry + " from JASP archive " + (filePath ? filePath : "") + ": " + exception.what();
		else
			Log::log() << "Could not read JASP archive entry." << std::endl;
		return false;
	}
}

static const char* statusResult(Json::Value status)
{
	static std::string result;
	result = status.toStyledString();
	return result.c_str();
}

static Json::Value statusBase(const char * operation)
{
	Json::Value status(Json::objectValue);
	status["operation"] = operation;
	status["ok"] = false;
	return status;
}

static const char* statusError(Json::Value status, const std::string & error)
{
	status["ok"] = false;
	status["error"] = error;
	Log::log() << error << std::endl;
	return statusResult(status);
}

static Json::Value analysisOptionsStatus(const char * filePath, int analysisNr)
{
	Json::Value status = statusBase("syntaxBridgeAnalysisOptionsFromJaspFile");
	status["analysisNr"] = analysisNr;

	Json::Value analysesJson;
	std::string error;
	if (!readJaspJsonEntry(analysesJson, filePath, "analyses.json", &error))
	{
		status["failure"] = "read";
		status["error"] = error;
		return status;
	}

	const Json::Value & analyses = analysesJson.isArray() ? analysesJson : analysesJson["analyses"];
	if (!analyses.isArray())
	{
		status["failure"] = "schema";
		status["error"] = std::string("JASP archive analyses.json does not contain an analyses array.");
		return status;
	}

	status["analysisCount"] = static_cast<Json::UInt64>(analyses.size());
	if (analysisNr < 0 || analysisNr >= int(analyses.size()))
	{
		status["failure"] = "index";
		status["error"] = std::string("Could not find analysis ") + std::to_string(analysisNr) + " in JASP archive.";
		return status;
	}

	const Json::Value & options = analyses[analysisNr]["options"];
	if (options.isNull())
	{
		status["failure"] = "schema";
		status["error"] = std::string("Analysis ") + std::to_string(analysisNr) + " does not contain options.";
		return status;
	}

	status["ok"] = true;
	status["options"] = options;
	return status;
}

static void clearRequestedDataState()
{
	rbridge_setWantedCols(ColumnEncoder::colsPlusTypes());

	ColumnEncoder::colTypeMap noColumns;
	ColumnEncoder::columnEncoder()->setCurrentNames(noColumns);

	if (gl_extraEncodings)
		gl_extraEncodings->setCurrentNames(noColumns);
}

static void clearDataBridgeState()
{
	clearRequestedDataState();
	rbridge_clearDataBridge();

	if (gl_dataBridge)
	{
		delete gl_dataBridge;
		gl_dataBridge = nullptr;
	}
}

static void createDataBridge(bool dbInMemory)
{
	gl_dataBridge = new DataBridge(ProcessInfo::currentPID(), dbInMemory);
	rbridge_setDataBridge(gl_dataBridge);
	gl_initializedDbInMemory = dbInMemory;
}

static void clearQmlFormCache()
{
	for (auto value : gl_qmlFormMap.values())
		deleteQuickItem(value.second);

	if (gl_application)
		QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);

	gl_qmlFormMap.clear();

	if (gl_qmlEngine)
	{
		gl_qmlEngine->clearSingletons();
		gl_qmlEngine->clearComponentCache();
	}
}

static void refreshQmlDataSetInfoContext()
{
	if (gl_qmlEngine)
		gl_qmlEngine->rootContext()->setContextProperty("dataSetInfo", VariableInfo::info());
}

static DataSetProvider* resetDataProvider(bool dbInMemory, bool resetDataSet)
{
	bool providerWillBeRecreated = gl_initialized && gl_initializedDbInMemory != dbInMemory;
	if (providerWillBeRecreated)
		clearQmlFormCache();

	DataSetProvider * provider = DataSetProvider::getProvider(dbInMemory, resetDataSet, gl_application);
	gl_initializedDbInMemory = dbInMemory;
	refreshQmlDataSetInfoContext();
	return provider;
}

static bool recreateCleanDataBridgeState(bool dbInMemory)
{
	try
	{
		clearDataBridgeState();
		DataSetProvider::getProvider(!dbInMemory, true, gl_application);
		resetDataProvider(dbInMemory, true);
		createDataBridge(dbInMemory);
		return gl_dataBridge != nullptr;
	}
	catch (const std::exception & exception)
	{
		Log::log() << "Could not restore SyntaxInterface native dataset state after failed JASP archive load: " << exception.what() << std::endl;
	}
	catch (...)
	{
		Log::log() << "Could not restore SyntaxInterface native dataset state after failed JASP archive load." << std::endl;
	}

	return false;
}

extern "C" {
void STDCALL syntaxBridgeClearQmlState()
{
	clearQmlFormCache();
}

void STDCALL syntaxBridgeClearDataSetState()
{
	clearDataBridgeState();

	if (gl_initialized)
	{
		resetDataProvider(gl_initializedDbInMemory, true);
		createDataBridge(gl_initializedDbInMemory);
	}
}

void STDCALL syntaxBridgeClearNativeState()
{
	syntaxBridgeClearQmlState();
	syntaxBridgeClearDataSetState();
}

void STDCALL syntaxBridgeCleanup()
{
	syntaxBridgeClearQmlState();
}

void STDCALL syntaxBridgeShutdown()
{
	syntaxBridgeClearQmlState();
	clearDataBridgeState();

	if (gl_extraEncodings)
	{
		delete gl_extraEncodings;
		gl_extraEncodings = nullptr;
	}

	if (gl_initialized)
	{
		DataSetProvider * provider = DataSetProvider::getProvider(gl_initializedDbInMemory, false, gl_application);
		delete provider;
	}

	if (gl_qmlEngine)
	{
		delete gl_qmlEngine;
		gl_qmlEngine = nullptr;
	}

	if (gl_application)
	{
		gl_application->processEvents();
		delete gl_application;
		gl_application = nullptr;
	}

	gl_applicationArgc = 0;
	gl_applicationArgv.clear();
	gl_applicationArgvStorage.clear();
	gl_initialized = false;
	gl_initializedDbInMemory = false;
	gl_rBridgeInitialized = false;
	gl_jaspBaseInitialized = false;
}

void STDCALL syntaxBridgeLoadDataSet(const SyntaxBridgeDataSet* syntaxBridgeDataSet, bool dbInMemory, int threshold, bool orderLabelsByValue)
{
	if (!init(dbInMemory))
	{
		Log::log() << "Error during initialization" << std::endl;
		return;
	}

	DataSetProvider* provider = nullptr;
	if (gl_initializedDbInMemory != dbInMemory)
	{
		clearDataBridgeState();
		provider = resetDataProvider(dbInMemory, true);
		createDataBridge(dbInMemory);
	}
	else
		provider = DataSetProvider::getProvider(dbInMemory);

	std::map<std::string, stringvec > dataSet;

	for (int colNr = 0; colNr < syntaxBridgeDataSet->columnCount; colNr++)
	{
		const SyntaxBridgeColumn& column  = syntaxBridgeDataSet->columns[colNr];
		stringvec values;
		for (int rowNr = 0; rowNr < syntaxBridgeDataSet->rowCount; rowNr++)
			values.push_back(column.values[rowNr]);
		dataSet[column.name] = values;
	}

	provider->loadDataSet(dataSet, threshold, orderLabelsByValue);
}

void STDCALL syntaxBridgeLoadDataSetFromJaspFile(const char * filePath, bool dbInMemory)
{
	syntaxBridgeLoadDataSetFromJaspFileStatus(filePath, dbInMemory);
}

const char* STDCALL syntaxBridgeLoadDataSetFromJaspFileStatus(const char * filePath, bool dbInMemory)
{
	Json::Value status = statusBase("syntaxBridgeLoadDataSetFromJaspFile");
	status["dbInMemoryRequested"] = dbInMemory;
	status["dbInMemoryUsed"] = false;

	if (!filePath || std::string(filePath).empty())
		return statusError(status, "Cannot load dataset from an empty JASP archive path.");

	if (dbInMemory)
		status["warning"] = "dbInMemory=TRUE is ignored for .jasp archives; SyntaxInterface loads archive databases through file-backed internal.sqlite.";

	if (!init(false))
	{
		return statusError(status, "Error during initialization.");
	}

	bool nativeStateMutated = false;

	try
	{
		Json::Value manifest;
		std::string manifestError;
		if (!readJaspJsonEntry(manifest, filePath, "manifest.json", &manifestError))
			return statusError(status, manifestError);

		std::string jaspVersionStr = manifest.get("jaspVersion", "").asString();
		std::string archiveVersionStr = manifest.get("jaspArchiveVersion", "").asString();
		if (archiveVersionStr.empty())
			return statusError(status, "JASP archive manifest is missing jaspArchiveVersion.");
		if (jaspVersionStr.empty())
			return statusError(status, "JASP archive manifest is missing jaspVersion.");

		status["jaspArchiveVersion"] = archiveVersionStr;
		status["jaspVersion"] = jaspVersionStr;

		Version jaspVersion(jaspVersionStr);

		// Keep SyntaxInterface below Desktop's DataSetPackage/UI ownership while
		// mirroring the archive import steps that matter for backend replay:
		// extract internal.sqlite, upgrade it for the saved JASP version, then
		// expose it through the bridge-owned DataBridge.
		clearDataBridgeState();
		nativeStateMutated = true;
		DataSetProvider * provider = resetDataProvider(false, false);
		provider->closeDatabase();
		ArchiveReader(filePath, DatabaseInterface::singleton()->dbFile(true)).writeEntryToTempFiles([](float) {});
		provider->loadDatabase(jaspVersion);
		status["databaseUpgraded"] = true;
		createDataBridge(false);

		DataSet * dataSet = gl_dataBridge ? gl_dataBridge->provideAndUpdateDataSet() : nullptr;
		if (!dataSet)
		{
			status["nativeStateRestored"] = recreateCleanDataBridgeState(false);
			return statusError(status, std::string("Could not load dataset from JASP archive ") + filePath + ": no dataset was provided by the bridge.");
		}

		status["ok"] = true;
		status["columnCount"] = static_cast<Json::UInt64>(dataSet->columnCount());
		status["rowCount"] = static_cast<Json::UInt64>(dataSet->rowCount());
		return statusResult(status);
	}
	catch (const std::exception & exception)
	{
		if (nativeStateMutated)
			status["nativeStateRestored"] = recreateCleanDataBridgeState(false);
		return statusError(status, std::string("Could not load dataset from JASP archive ") + filePath + ": " + exception.what());
	}
}

const char* STDCALL syntaxBridgeLoadQmlAndParseOptions(const char* moduleName, const char* analysisName, const char* qmlFile, const char* options, const char* version, bool preloadData)
{
	if (!init())
	{
		Log::log() << "Error during initialization" << std::endl;
		return "";
	}

	std::string qmlFileStr		= qmlFile,
				versionStr		= version,
				analysisNameStr	= analysisName,
				moduleNameStr	= moduleName;


	AnalysisForm* form = getQmlForm(tq(qmlFileStr));

	if (!form)
	{
		Log::log() << "Cannot create QML Form " << qmlFileStr << std::endl;
		return "";
	}

	Json::Value parsedOptions;
	std::string errorMsg;

	if (!form->parseOptions(options, parsedOptions, errorMsg))
	{
		Log::log() << "Error when parsing options: " << errorMsg << std::endl;
		return "";
	}

	gl_extraEncodings->setCurrentNamesFromOptionsMeta(parsedOptions);
	gl_dataBridge->updateOptionsAccordingToMeta(parsedOptions);
	ColumnEncoder::colsPlusTypes analysisColsTypes = ColumnEncoder::encodeColumnNamesinOptions(parsedOptions, preloadData);

	rbridge_setWantedCols(analysisColsTypes);

	static std::string result;
	result = parsedOptions.toStyledString();

	return result.c_str();
}

const char* STDCALL syntaxBridgeAnalysisOptionsFromJaspFile(const char * filePath, int analysisNr)
{
	static std::string result;
	result = "";

	Json::Value status = analysisOptionsStatus(filePath, analysisNr);
	if (!status["ok"].asBool())
	{
		if (status.isMember("error"))
			Log::log() << status["error"].asString() << std::endl;
		return result.c_str();
	}

	result = status["options"].toStyledString();
	return result.c_str();
}

const char* STDCALL syntaxBridgeAnalysisOptionsFromJaspFileStatus(const char * filePath, int analysisNr)
{
	Json::Value status = analysisOptionsStatus(filePath, analysisNr);
	if (!status["ok"].asBool() && status.isMember("error"))
		Log::log() << status["error"].asString() << std::endl;
	return statusResult(status);
}


const char* STDCALL syntaxBridgeGenerateModuleWrappers(const char* modulePath)
{
	if (!init())
		return "Error during initialization";

	static std::string result;

	QString modulePathQ = tq(modulePath);

	ModuleInfo description = parseDescription(modulePathQ);

	for (const AnalysisInfo & analysis : description.analyses)
	{
		Log::log() << "Analysis " << analysis.analysisName << " with qml file " << analysis.qmlFileName << std::endl;
		if (!generateWrapper(modulePathQ, analysis.analysisName, analysis.qmlFileName, analysis.analysisTitle, analysis.preloadData))
		{
			result = "Error when generating wrapper of " + fq(analysis.analysisName);
			return result.c_str();
		}
	}

	return "Wrappers generated";
}


const char* STDCALL syntaxBridgeGenerateAnalysisWrapper(const char* modulePath, const char* analysisName)
{
	if (!init())
		return "Error during initialization";

	static std::string result;

	gl_qmlEngine->clearComponentCache();

	Modules::DynamicModule * module = new Modules::DynamicModule(gl_application, gl_qmlEngine->rootContext(), modulePath, false);
	module->initialize(gl_qmlEngine->rootContext());

	std::string analysisNameStr = analysisName,
				modulePathStr	= modulePath;

	for (Modules::AnalysisEntry * analysisEntry : module->menu())
	{
		if (analysisEntry->isAnalysis() && analysisEntry->function() == analysisNameStr)
		{
			if (!generateWrapper(tq(modulePathStr), tq(analysisNameStr), tq(analysisEntry->qml()), tq(analysisEntry->title()), analysisEntry->preloadData()))
			{
				result = "Error when generating wrapper of " + analysisNameStr;
				return result.c_str();
			}

			result = "Wrapper generated for analysis " + analysisNameStr;
			return result.c_str();

		}
	}

	result = "Cannot find analysis " + analysisNameStr + " in module path " + modulePathStr;

	return result.c_str();
}

const char* STDCALL syntaxBridgeParseDescription(const char* modulePath)
{
	if (!init())
	{
		Log::log() << "Error during initialization" << std::endl;
		return "";
	}

	ModuleInfo description = parseDescription(tq(modulePath));

	Json::Value jsonDescription(Json::objectValue);

	jsonDescription["name"]				= fq(description.name);
	jsonDescription["title"]			= fq(description.title);
	jsonDescription["author"]			= fq(description.author);
	jsonDescription["website"]			= fq(description.website);
	jsonDescription["license"]			= fq(description.license);
	jsonDescription["maintainer"]		= fq(description.maintainer);
	jsonDescription["description"]		= fq(description.description);
	jsonDescription["requiresData"]		= description.requiresData;
	jsonDescription["hasWrappers"]		= description.hasWrappers;
	jsonDescription["isCommon"]			= description.isCommon;
	jsonDescription["version"]			= description.version.asString();

	Json::Value	analyses(Json::arrayValue);

	for (const AnalysisInfo & analysis : description.analyses)
	{
		Json::Value jsonAnalysis(Json::objectValue);
		jsonAnalysis["name"]		= fq(analysis.analysisName);
		jsonAnalysis["qml"]			= fq(analysis.qmlFileName);
		jsonAnalysis["title"]		= fq(analysis.analysisTitle);
		jsonAnalysis["preloadData"]	= analysis.preloadData;
		jsonAnalysis["hasWrapper"]	= analysis.hasWrapper;

		analyses.append(jsonAnalysis);
	}

	jsonDescription["analyses"]		= analyses;

	static std::string result;
	result = jsonDescription.toStyledString();

	return result.c_str();
}

const char* STDCALL syntaxBridgeGetVariableNames()
{
	static std::string result;

	if (!init())
	{
		Log::log() << "Error during initialization" << std::endl;
		result = "";
		return result.c_str();
	}

	size_t numCols = 0;
	const char ** columnNames = rbridge_allColumnNames(numCols, false);

	Json::Value jsonNames(Json::arrayValue);
	for (size_t i = 0; i < numCols; ++i)
		jsonNames.append(columnNames[i]);

	result = jsonNames.toStyledString();
	return result.c_str();
}

} // extern "C"


void blockSignalsRecursive(QObject* item)
{
	for (QObject* obj : item->children())
		blockSignalsRecursive(obj);
	item->blockSignals(true);
}

void deleteQuickItem(QQuickItem* item)
{
	blockSignalsRecursive(item);
	item->setParent(nullptr);
	item->setParentItem(nullptr);
	item->deleteLater();
}

void sendMessage(const char * msg)
{
	if (gl_verbose)
		Log::log() << "Send Message: " << msg << std::endl;
}

bool init(bool dbInMemory)
{
	if (gl_initialized) return true;
	gl_initialized = true;
	gl_initializedDbInMemory = dbInMemory;

	if (gl_verbose)
	{
		QString qt_install_dir = qgetenv("QT_DIR");
#ifdef QT_DIR
		if (qt_install_dir.isEmpty())
			qt_install_dir = STRINGIZE(QT_DIR);
#endif
		Log::log() << "QT_DIR found in environment: " + fq(qt_install_dir) << std::endl;

		QString rHome = qgetenv("R_HOME");
		Log::log() << "R_HOME: " << fq(rHome) << std::endl;
	}

	//const char*	platformArg = "-platform";
	//const char*	platformOpt = "minimal"; //"cocoa";

	gl_applicationArgvStorage = {"JASP"}; //{qmlR, platformArg, platformOpt};
	gl_applicationArgv.clear();
	for (std::string & argument : gl_applicationArgvStorage)
		gl_applicationArgv.push_back(argument.data());
	gl_applicationArgv.push_back(nullptr);
	gl_applicationArgc = static_cast<int>(gl_applicationArgvStorage.size());

	qputenv("QT_QPA_PLATFORM", "minimal");

	gl_application = new QGuiApplication(gl_applicationArgc, gl_applicationArgv.data());
	gl_qmlEngine = new QQmlEngine();

	Dirs::setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles::init(ProcessInfo::currentPID());
	DataSetProvider::getProvider(dbInMemory, false, gl_application); // Create the DataSetProvider in case the loadDataSet was not already called

	QmlUtils::setupQMLEngine(gl_qmlEngine);
	QmlUtils::registerQmlModuleTypes();

	createDataBridge(dbInMemory);
	gl_extraEncodings = new ColumnEncoder("JaspExtraOptions_");

	rbridge_init(gl_dataBridge, sendMessage, [](){ return false; }, gl_extraEncodings, gl_param_resultFont.c_str(), false);
	gl_rBridgeInitialized = true;

	return true;
}

void ensureRBridgeInitialized()
{
	if (gl_rBridgeInitialized)
		return;

	rbridge_init(gl_dataBridge, sendMessage, [](){ return false; }, gl_extraEncodings, gl_param_resultFont.c_str(), false);
	gl_rBridgeInitialized = true;
}

void ensureJaspBaseInitialized()
{
	if (gl_jaspBaseInitialized)
		return;

	// Option parsing and dataset replay do not need jaspBase. Load it only for
	// the less common path where QML explicitly asks to evaluate R code.
	ensureRBridgeInitialized();
	jaspRCPP_init_jaspBase();
	gl_jaspBaseInitialized = true;
}

void sendRScriptHandler(AnalysisForm* form, QString script, QString controlName, bool whiteListedVersion)
{
	if (gl_verbose)
		Log::log() << "R Script " << fq(script) << " sent by " << controlName << std::endl;

	ensureJaspBaseInitialized();

	bool hasError = false;
	std::string result = rbridge_evalRCodeWhiteListed(fq(script).c_str(), whiteListedVersion);

	if (result == "")
	{
		hasError = true;
		result = jaspRCPP_getLastErrorMsg();
	}

	if (gl_verbose)
		Log::log() << "R Script result " << (hasError ? "has error" : "") << ": " << result << std::endl;

	form->runScriptRequestDone(tq(result), controlName, hasError);
}

AnalysisForm* getQmlForm(const QString& qmlFileStr)
{
	AnalysisForm* qmlForm = nullptr;

	QFileInfo	qmlFileInfo(qmlFileStr);
	if (!qmlFileInfo.exists())
	{
		Log::log() << "File not found: " << fq(qmlFileStr) << std::endl;
		return nullptr;
	}

	if (gl_qmlFormMap.contains(qmlFileStr) && gl_qmlFormMap[qmlFileStr].first == qmlFileInfo.lastModified())
		qmlForm = gl_qmlFormMap[qmlFileStr].second;
	else
	{
		QUrl urlFile = QUrl::fromLocalFile(qmlFileInfo.absoluteFilePath());

		QQmlIncubator localIncubator(QQmlIncubator::Synchronous);
		QQmlComponent qmlComp( gl_qmlEngine, urlFile, QQmlComponent::PreferSynchronous);
		QQmlContext* context = gl_qmlEngine->rootContext();

		qmlComp.create(localIncubator, context);

		switch (localIncubator.status())
		{
		case QQmlIncubator::Null:
		case QQmlIncubator::Loading:
		{
			Log::log() << "Could not load QML component!" << std::endl;
			// Try it with QQmlComponent::create: this gives a better error
			qmlForm = qobject_cast<AnalysisForm*>(qmlComp.create(context));
			if (qmlForm)
				Log::log() << "Form could be loaded via QQmlComponent::create instead of QQmlIncubator::created. Quite weird!" << std::endl;
			else
			{
				for(const auto & error : qmlComp.errors())
					Log::log() << "Error when creating component at " << fq(QString::number(error.line())) << "," << fq(QString::number(error.column())) << " in file " << error.url().toString() << ": " << fq(error.description()) << std::endl;

				return nullptr;
			}
		}
		case QQmlIncubator::Error:
			Log::log() << "Error when creating component!" << std::endl;
			for(const auto & error : localIncubator.errors())
				Log::log() << "Error when creating component at " << fq(QString::number(error.line())) << "," << fq(QString::number(error.column())) << " in file " << error.url().toString() << ": " << fq(error.description()) << std::endl;
			return nullptr;
		case QQmlIncubator::Ready:
		{
			Log::log() << "QML form created" << std::endl;
			qmlForm = qobject_cast<AnalysisForm*>(localIncubator.object());
			if (!qmlForm)
			{
				Log::log() << "Object created is not an AnalysisForm object!!" << std::endl;
				return nullptr;
			}

			break;
		}
		}


		AnalysisBase* analysis = qmlForm->analysisObj();
		if (!analysis)
		{
			analysis = new AnalysisBase(qmlForm); // Make dummy analysis
			qmlForm->setAnalysis(analysis);
		}

		QObject::connect(analysis,	&AnalysisBase::sendRScriptSignal,	[qmlForm](QString script, QString controlName, bool whiteListedVersion, QString module) { sendRScriptHandler(qmlForm, script, controlName, whiteListedVersion); });

		if (gl_qmlFormMap.contains(qmlFileStr))
			deleteQuickItem(gl_qmlFormMap[qmlFileStr].second); // delete old version of the form
		gl_qmlFormMap[qmlFileStr] = std::make_pair(qmlFileInfo.lastModified(), qmlForm);

		gl_application->processEvents();

	}

	return qmlForm;
}


bool generateWrapper(const QString& modulePath, const QString& analysisName, const QString& qmlFileName, const QString& analysisTitle, bool preloadData)
{
	QString qmlFilePath = modulePath + "/inst/qml/" + qmlFileName;

	AnalysisForm* form = getQmlForm(qmlFilePath);
	if (!form)
	{
		Log::log() << "Cannot create the QML form " << qmlFilePath << std::endl;
		return false;
	}

	QString returnedValue = form->generateWrapper(QDir(modulePath).dirName(), analysisName, qmlFileName, analysisTitle, preloadData);

	QFile file(modulePath + "/R/" + analysisName + "Wrapper.R");
	file.resize(0); // Empty the file
	if (file.open(QIODevice::ReadWrite)) {
		QTextStream stream(&file);
		stream << returnedValue;
	}

	return true;
}

ModuleInfo parseDescription(const QString & modulePath)
{
	QDir moduleDir(modulePath);

	if (!moduleDir.exists())
	{
		Log::log() << "Module path not found: " + modulePath << std::endl;
		return ModuleInfo();
	}

	Modules::DynamicModule * module = new Modules::DynamicModule(gl_application, gl_qmlEngine->rootContext(), modulePath, false);
	module->initialize(gl_qmlEngine->rootContext());

	ModuleInfo moduleInfo(module->nameQ(), module->titleQ(), module->author(), module->website().toString(), module->license(), module->maintainer(), module->description(),
						  module->requiresData(), module->isCommon(), module->hasWrappers(), module->version());

	for (Modules::AnalysisEntry * analysisEntry : module->menu())
	{
		if (analysisEntry->isAnalysis())
			moduleInfo.analyses.push_back(AnalysisInfo(tq(analysisEntry->function()), tq(analysisEntry->qml()), tq(analysisEntry->title()), analysisEntry->preloadData(), analysisEntry->hasWrapper()));
	}

	return moduleInfo;
}






