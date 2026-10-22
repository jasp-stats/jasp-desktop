#include "dirs.h"
#include "testengineimages.h"
#include "testinfo.h"
#include "tempfiles.h"
#include "ipcchannel.h"
#include "processinfo.h"
#include "enginedefinitions.h"
#include "qutils.h"
#include "engine/enginesync.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "gui/preferencesmodel.h"
#include "utilities/languagemodel.h"
#include <QApplication>
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "engine/enginerepresentation.h"
#include <QElapsedTimer>
#include <QStandardPaths>

namespace
{
	const int	analysisId		= 7;
	const char*	analysisName	= "TestEngineImagesAnalysis";

	//Draws a plot whose x-axis title is the encoded name of contNormal, like an analysis on contNormal does.
	//It stops if the name did not get encoded, otherwise the test could not tell a decoded title from one that was never encoded.
	const char*	analysisRCall	=
		"function(jaspResults, dataset, options) {\n"
		"	encoded <- jaspBase::encodeColNames('contNormal', strict = TRUE)\n"
		"	if (identical(encoded, 'contNormal')) stop('contNormal did not get encoded')\n"
		"	plot <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x = x, y = y)) + ggplot2::geom_point() + ggplot2::xlab(encoded)\n"
		"	jaspResults[['plot']] <- jaspBase::createJaspPlot(plot = plot, title = 'Plot', width = 320, height = 240)\n"
		"}";

	QString moduleLibToUse()
	{
		QString lib = qgetenv("JASP_TEST_MODULE_LIB");

		if(lib.isEmpty()) //The library of the installed jaspDescriptives module, as AppDirs::userModulesLibDir() gives it in JASP itself
			lib = QStandardPaths::writableLocation(QStandardPaths::GenericDataLocation) + "/JASP/JASP/Modules/module_libs/jaspDescriptives";

		return lib;
	}
}

void TestEngineImages::init()
{
	TempFiles	::	clearSessionDir();
	Dirs		::	setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles	::	init(ProcessInfo::currentPID());
	Settings	::	informSettingsThatThisIsATest();

	//Without preferences EngineSync allows a single engine; in a test the preferences hold the defaults, which allow more.
	//The settings sent to engines include the language, so that needs its model too.
	_language	=	new LanguageModel(qobject_cast<QApplication *>(qApp), nullptr, this);
	_prefs		=	new PreferencesModel(this);
	_pkg		=	new DataSetPackage(this);
	_importer	=	new CSVImporter();
	_engines	=	new EngineSync(this);

	_engines		->	start();
	_analysisEngine	=	_engines->createNewEngine(true, 0);
	_freshEngine	=	_engines->createNewEngine(true, 1);
}

void TestEngineImages::cleanup()
{
	for(EngineRepresentation * engine : { _analysisEngine, _freshEngine })
		if(engine)
			engine->shutEngineDown();

	_analysisEngine = _freshEngine = nullptr;

	delete _engines;
	_engines = nullptr;

	DatabaseInterface::singleton()->close();
	DatabaseInterface::singleton()->closeInterfaces();

	delete _pkg;
	_pkg = nullptr;

	delete _importer;
	_importer = nullptr;

	delete _prefs;
	_prefs = nullptr;

	delete _language;
	_language = nullptr;
}

IPCChannel * TestEngineImages::channelOf(EngineRepresentation * engine)
{
	return emit engine->channelSignal(engine->channelNumber());
}

Json::Value TestEngineImages::waitForReply(EngineRepresentation * engine, std::function<bool(const Json::Value &)> isTheOne, int timeoutMs)
{
	QElapsedTimer	timer;
	std::string		data;

	timer.start();

	while(timer.elapsed() < timeoutMs)
	{
		Json::Value reply;

		if(channelOf(engine)->receive(data, 100) && !data.empty() && Json::Reader().parse(data, reply) && isTheOne(reply))
			return reply;

		QCoreApplication::processEvents();
	}

	return Json::nullValue;
}

///Puts jaspBase on the engine's library path and initializes it, the way a module load request from JASP does
bool TestEngineImages::loadJaspBase(EngineRepresentation * engine, const QString & moduleLib)
{
	Json::Value request		= Json::objectValue;

	request["typeRequest"]		= engineStateToString(engineState::moduleLoadRequest);
	request["moduleRequest"]	= moduleStatusToString(moduleStatus::loading);
	request["moduleName"]		= "jaspDescriptives";
	request["moduleLibPaths"]	= "c('" + fq(moduleLib) + "', .libPaths())";
	request["moduleCode"]		= "library('jaspBase'); library('jaspGraphs');\nreturn('succes!')";

	channelOf(engine)->send(request.toStyledString());

	Json::Value reply = waitForReply(engine, [](const Json::Value & reply) { return reply.get("typeRequest", "").asString() == engineStateToString(engineState::moduleLoadRequest); }, 120000);

	if(!reply.get("succes", false).asBool())
		qWarning() << "Loading jaspBase failed:" << tq(reply.toStyledString());

	return reply.get("succes", false).asBool();
}

void TestEngineImages::testResizeOnFreshEngineDecodesColumnNames()
{
	QString moduleLib = moduleLibToUse();

	if(!QDir(moduleLib + "/jaspBase").exists() || !QDir(moduleLib + "/jaspGraphs").exists())
		QSKIP(qPrintable("No jaspBase and jaspGraphs in '" + moduleLib + "', set JASP_TEST_MODULE_LIB to a library that has them"));

	QVERIFY2(channelOf(_analysisEngine) && channelOf(_freshEngine), "Not enough engine channels");

	//Both engines must be done starting before any data exists, otherwise a fresh engine can still pick up the column names while it starts.
	//That is the situation after loading a .jasp file: the engines got resumed before the data arrived.
	_engines->startStoppedEngine(_analysisEngine);
	_engines->startStoppedEngine(_freshEngine);
	QTRY_VERIFY_WITH_TIMEOUT(_analysisEngine->idle() && _freshEngine->idle(), 60000);

	//From here on the test talks to the engines itself
	_engines->killProcessTimer();

	QVERIFY2(loadJaspBase(_analysisEngine,	moduleLib), "Could not load jaspBase in the analysis engine");
	QVERIFY2(loadJaspBase(_freshEngine,		moduleLib), "Could not load jaspBase in the fresh engine");

	_importer->loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int){});
	QVERIFY2(_pkg->dataSet() && _pkg->dataSet()->column("contNormal"), "debug.csv did not load");

	auto replyFor = [](const std::set<std::string> & statuses)
	{
		return [statuses](const Json::Value & reply)
		{
			return	reply.get("typeRequest", "").asString() == engineStateToString(engineState::analysis)	&&
					reply.get("id", -1).asInt() == analysisId												&&
					statuses.count(reply.get("status", "").asString());
		};
	};

	std::set<std::string> failures = { "fatalError", "validationError", "exception", "aborted" };

	//Run the analysis on the first engine, which stores the plot in the analysis state
	Json::Value run			= Json::objectValue;
	run["typeRequest"]			= engineStateToString(engineState::analysis);
	run["id"]					= analysisId;
	run["perform"]				= performTypeToString(performType::run);
	run["revision"]				= 1;
	run["name"]					= analysisName;
	run["title"]				= "Test";
	run["dynamicModuleCall"]	= analysisRCall;
	run["preloadData"]			= false;
	run["options"]				= Json::objectValue;

	channelOf(_analysisEngine)->send(run.toStyledString());

	std::set<std::string> runDone = failures;
	runDone.insert("complete");

	Json::Value runReply = waitForReply(_analysisEngine, replyFor(runDone), 120000);
	QVERIFY2(runReply.get("status", "").asString() == "complete", qPrintable("The analysis did not complete: " + tq(runReply.toStyledString())));

	//Resize the plot on the engine that never ran an analysis
	Json::Value resize		= run;
	resize["perform"]			= performTypeToString(performType::editImg);
	resize["dynamicModuleCall"]	= "";
	resize["image"]["name"]		= "plot";
	resize["image"]["type"]		= "resize";
	resize["image"]["width"]	= 400;
	resize["image"]["height"]	= 300;

	channelOf(_freshEngine)->send(resize.toStyledString());

	std::set<std::string> editDone = failures;
	editDone.insert("imageEdited");

	Json::Value editReply = waitForReply(_freshEngine, replyFor(editDone), 120000);
	QVERIFY2(editReply.get("status", "").asString() == "imageEdited",	qPrintable("The resize did not succeed: "		+ tq(editReply.toStyledString())));
	QVERIFY2(!editReply["results"].get("error", false).asBool(),		qPrintable("The resize returned an error: "	+ tq(editReply.toStyledString())));

	//The engine decodes column names in every reply, as far as it knows them. The plot was drawn with the encoded name,
	//so it only comes back as contNormal when the fresh engine knows the dataset's column names, which drawing the plot needs as well.
	std::string xTitle = editReply["results"]["editOptions"]["xAxis"]["settings"].get("title", "").asString();
	QVERIFY2(xTitle == "contNormal", qPrintable("The x-axis title came back as '" + tq(xTitle) + "' instead of 'contNormal'"));
}

QTEST_MAIN(TestEngineImages)
