#include "dirs.h"
#include "testengine.h"
#include <QSignalSpy>
#include "testinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "utilities/qutils.h"
#include "engine/enginesync.h"
#include "utilities/appdirs.h"
#include "utilities/settings.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "engine/enginerepresentation.h"
#include "appinfo.h"
#include "qquick/rcommander.h"

void TestEngine::initTestCase()
{

}

void TestEngine::init()
{
	TempFiles	::	clearSessionDir();
	Dirs		::	setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles	::	init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory
	Settings	::	informSettingsThatThisIsATest();
	
	_pkg		=	new DataSetPackage(this);
	_importer	=	new CSVImporter();
	_engines	=	new EngineSync(this);

	_engines	->	start();
	_engineRep	=	_engines->createNewEngine(true, 0);
	_importer	->	loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), [](int i){});
	_data		=	_pkg->dataSet();
}

void TestEngine::cleanup()
{
	if(_engineRep)
		_engineRep->shutEngineDown();
	
	delete _engines;
	_engines = nullptr;
	
	delete _data;
	_data = nullptr;
	
	DatabaseInterface::singleton()->close();
	DatabaseInterface::singleton()->closeInterfaces();
	
	delete _pkg;
	_pkg = nullptr;
	
	delete _importer;
	_importer = nullptr;
}

void TestEngine::testComputedColumns()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");
	
	_engines->startStoppedEngine(_engineRep);
	
	// const QString & columnName, const QString & warning, bool dataChanged
	QSignalSpy spy(_engineRep, SIGNAL(computeColumnSucceeded(const QString &, const QString &, bool))); 
	
	QVERIFY2(spy.isValid(),	"Spy is broken!");
	
	if(!_data->column("V1")->hasLabels())
		_data->column("V1")->noLabelsToLabels();	
		
	Column * col = _data->column("contBinom");
	col->setCodeType(computedColumnType::rCode);
	col->setRCode("V1");
	
	
	_engines->computeColumn("contBinom", tq(col->rCode()), columnType::ordinal);
	
	spy.wait();
	
	QVERIFY2(spy.count() == 1,	"Did not get a response");
	
	QVariantList response = spy.takeFirst();
	spy.clear();
	
	QVERIFY2(response[0].toString() == "contBinom",	"Did not get the right column back in response");
	QVERIFY2(response[1].toString() == "",			"Got a warning!");
	QVERIFY2(response[2].toBool(),					"Did not get dataChanged back in response");

	col->checkForUpdates();
	

	Json::Value		jsonContBinom	= col->jsonForCompare(),
					jsonV1			= _data->column("V1")->jsonForCompare();

	std::cout << jsonContBinom.toStyledString() << "\n" << jsonV1.toStyledString() << std::endl;
	
	QVERIFY2(jsonContBinom["labels"] == jsonV1["labels"], "Labels are not the same");
	QVERIFY2(jsonContBinom["data"]   == jsonV1["data"],   "Data is not the same");



	//Now lets see if it can also not be the same:
	col->setRCode("V1+1");

	_engines->computeColumn("contBinom", tq(col->rCode()), columnType::scale);

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	response = spy.takeFirst();
	spy.clear();

	QVERIFY2(response[0].toString() == "contBinom",	"Did not get the right column back in response");
	QVERIFY2(response[1].toString() == "",			"Got a warning!");
	QVERIFY2(response[2].toBool(),					"Did not get dataChanged back in response");

	col->checkForUpdates();

	jsonContBinom	= _data->column("contBinom")->jsonForCompare();
	jsonV1			= _data->column("V1")->jsonForCompare();
	
	std::cerr << jsonContBinom["labels"].toStyledString() << "\n" << jsonV1["labels"].toStyledString() << std::endl;

	QVERIFY2(jsonContBinom["data"]   != jsonV1["data"],   "Data is the same, but they shouldnt be");

	Column * col2 = _data->column("contcor1");
	col2->setCodeType(computedColumnType::rCode);
	col2->setRCode("contBinom-1"); //Should make it the same as V1 again
	
	if(!col2->hasLabels())
		col2->noLabelsToLabels();

	_engines->computeColumn("contcor1", tq(col2->rCode()), columnType::scale);

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	response = spy.takeFirst();
	spy.clear();

	QVERIFY2(response[0].toString() == "contcor1",	"Did not get the right column back in response");
	QVERIFY2(response[1].toString() == "",			"Got a warning!");
	QVERIFY2(response[2].toBool(),					"Did not get dataChanged back in response");

	col2->checkForUpdates();

	Json::Value		jsonContCor1	= col2->jsonForCompare();

	//std::cout << jsonContCor1.toStyledString() /*<< "\n" << jsonV1.toStyledString()*/ << std::endl;

	QVERIFY2(jsonContCor1["data"]   == jsonV1["data"],   "Data is not the same");
	QVERIFY2(jsonContCor1["labels"] == jsonV1["labels"], "Labels are not the same");

}

void TestEngine::testBatchAnalysis()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");

	EngineRepresentation * rcmd = _engines->createRCmdEngine();
	_engines->startStoppedEngine(rcmd);

	//A fresh engine boots in "initializing" and *silently drops* every request that is not a
	//resume message until it has been resumed (Engine::receiveMessages), so wait for the
	//resume/settings handshake to finish before sending anything.
	QTRY_VERIFY_WITH_TIMEOUT(rcmd->idle(), 60000);

	QSignalSpy spy(rcmd, SIGNAL(rCodeReturnedLog(const QString &, bool)));
	QVERIFY2(spy.isValid(), "Spy is broken!");

	//Regression: before the batch R code was embedded in jasp-desktop, every plain Run errored
	//out whenever the loaded jaspBase did not know .normalizeBatchResult.
	rcmd->runScriptOnProcess("1 + 1");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");

	const QString plainLog = spy.takeFirst()[0].toString();
	spy.clear();

	QVERIFY2(!plainLog.contains("Error", Qt::CaseInsensitive),											"Plain code errored in the commander");
	QVERIFY2(plainLog.contains("@@@JASP_BATCH_RESULT@@@[]@@@JASP_BATCH_RESULT_END@@@"),					"No empty batch result around a non-batch result");

	//A batch result: the desktop reads module/analysis/title as JSON strings, so length-1 values
	//must be scalars (auto_unbox), computed doubles must keep their precision (digits = NA,
	//jsonlite's default of 4 decimals would round 0.123456789012346 down to 0.1235) and NULL must
	//stay null instead of becoming an empty object (null = "null").
	const QString mockWrapper =
		"mockWrapper <- function(x, precise, aNull)\n"
		"{\n"
		"	list(module = 'testModule', analysis = 'FakeAnalysis',\n"
		"		 options = list(x = x, precise = precise, aNull = aNull))\n"
		"}\n";

	rcmd->runScriptOnProcess(mockWrapper + "batchAnalysis(mockWrapper, x = c(1, 2), precise = 0.123456789012345678, aNull = NULL)");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");

	const QString batchLog = spy.takeFirst()[0].toString();
	spy.clear();

	QVERIFY2(batchLog.contains("@@@JASP_BATCH_RESULT@@@"),						"Batch result was not transported");
	QVERIFY2(batchLog.contains("\"module\":\"testModule\""),					"module did not serialize as a scalar");
	QVERIFY2(!batchLog.contains("\"module\":[\"testModule\""),					"module serialized as an array");
	QVERIFY2(batchLog.contains("\"precise\":0.123456789012346"),				"Precision was lost, digits = NA expected");
	QVERIFY2(batchLog.contains("\"aNull\":null"),								"NULL did not serialize as JSON null");
	QVERIFY2(batchLog.contains("\"title\":\"FakeAnalysis: x=1\""),				"Automatic titles are missing");

	//A dry run must not create anything
	rcmd->runScriptOnProcess(mockWrapper + "batchAnalysis(mockWrapper, x = 1, precise = 1, aNull = NULL, .dryRun = TRUE)");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("@@@JASP_BATCH_RESULT@@@[]@@@JASP_BATCH_RESULT_END@@@"), "A dry run produced analyses");
	spy.clear();

	//Wrapper JSON strings (the plain lapply style) pass through unchanged
	rcmd->runScriptOnProcess("list('{\"module\":\"testModule\",\"analysis\":\"FakeAnalysis\",\"options\":{}}')");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("\"module\":\"testModule\",\"analysis\":\"FakeAnalysis\",\"options\":{}"), "Wrapper JSON strings were not passed through");
	spy.clear();

	//Ordinary R values stay batch-free
	rcmd->runScriptOnProcess("data.frame(x = 1:2)");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("@@@JASP_BATCH_RESULT@@@[]@@@JASP_BATCH_RESULT_END@@@"), "A data.frame was treated as a batch result");
	spy.clear();

	//Smoke: the selective column load and the frame override work in the commander engine
	rcmd->runScriptOnProcess("nrow(data)");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	const QString dataLog = spy.takeFirst()[0].toString();
	spy.clear();
	QVERIFY2(!dataLog.contains("Error", Qt::CaseInsensitive), "nrow(data) errored in the commander");

	rcmd->runScriptOnProcess("length(data$V1)");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(!spy.takeFirst()[0].toString().contains("Error", Qt::CaseInsensitive), "length(data$V1) errored in the commander");
	spy.clear();

	//"Columns by name": with the flag there is no fallback to the full dataset, so code that
	//names no columns gets an empty frame (rows kept, columns not).
	rcmd->runScriptOnProcess("paste(ncol(data), nrow(data) > 0)", true);
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("0 TRUE"), "Columns by name loaded the full dataset although no columns were named");
	spy.clear();

	//A column that *is* named does get loaded through the requested read (and only that one)
	rcmd->runScriptOnProcess("invisible(data$contNormal); paste(ncol(data), nrow(data) > 0)", true);
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("1 TRUE"), "Named-column read did not load exactly that one column");
	spy.clear();

	//columnFormula is embedded alongside the batch code: runtime column names must become
	//formula symbols treated literally, so batch code can build formulas from names(data)
	//without knowing R's backtick rules (base R's reformulate() cannot do this: it parses
	//the term labels, so "part length" is a syntax error there).
	rcmd->runScriptOnProcess("deparse(columnFormula('part length'))");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("`part length`"), "columnFormula did not turn a column name with a space into a symbol");
	spy.clear();

	rcmd->runScriptOnProcess("deparse(columnFormula('a+b', intercept = FALSE))");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("0 + `a+b`"), "columnFormula did not treat 'a+b' literally as one column or lost the intercept flag");
	spy.clear();

	rcmd->runScriptOnProcess("deparse(columnFormula('temperature', 'pressure', dependent = 'strength'))");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	QVERIFY2(spy.takeFirst()[0].toString().contains("strength ~ temperature + pressure"), "columnFormula mishandled dependent or multiple columns");
	spy.clear();

	//Regression: on Windows the strict encoder threw an MSVC exception into the GCC-built R-Interface for
	//any name that is not a column, which crashed the engine (heap corruption) instead of giving an R error.
	rcmd->runScriptOnProcess("paste(tryCatch(.encodeColNamesStrict('noSuchColumn'), error = function(e) 'encodeErrored'),"
							 "      tryCatch(.decodeColNamesStrict('noSuchColumn'), error = function(e) 'decodeErrored'),"
							 "      is.null(data[['noSuchColumn']]))");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine, did it crash?");
	QVERIFY2(spy.takeFirst()[0].toString().contains("encodeErrored decodeErrored TRUE"), "Unknown names did not give R errors and NULL");
	spy.clear();

	//names(data) gives the encoded names: columnFormula must take those as existing columns
	rcmd->runScriptOnProcess("f <- columnFormula(names(data)[1]); 'formulaBuilt'");
	QVERIFY2(spy.wait(30000), "No reply from the commander engine");
	const QString encodedNamesLog = spy.takeFirst()[0].toString();
	spy.clear();
	QVERIFY2(encodedNamesLog.contains("formulaBuilt"),	"columnFormula failed on an encoded column name");
	QVERIFY2(!encodedNamesLog.contains("No column named"),	"columnFormula reported an encoded column name as missing");
}

void TestEngine::testBatchCreationGate()
{
	QVERIFY2(_engines, "No EngineSync!");

	const bool wasPro = AppInfo::proMode();
	AppInfo::setProMode(false);

	//RCommander is a QQuickItem but constructible without a QML scene: it creates its own
	//commander engine through EngineSync::singleton(), which is the test's _engines. That engine
	//also boots in "initializing" and runCode refuses to send until it is idle, so wake it up here
	//first and wait for the handshake (RCommander's createRCmdEngine call reuses this engine).
	EngineRepresentation * rcmd = _engines->createRCmdEngine();
	_engines->startStoppedEngine(rcmd);
	QTRY_VERIFY_WITH_TIMEOUT(rcmd->idle(), 60000);

	RCommander commander;

	const QString batchCode =
		"mockWrapper <- function(x) list(module = 'testModule', analysis = 'FakeAnalysis', options = list(x = x))\n"
		"batchAnalysis(mockWrapper, x = c(1, 2))\n";

	QVERIFY2(commander.runCode(batchCode), "runCode refused to run");

	for (int i = 0; i < 300 && !commander.output().contains("requires JASP Enterprise"); ++i)
		QTest::qWait(100);

	QVERIFY2(commander.output().contains("requires JASP Enterprise"),	"Community build stayed silent about batch creation being enterprise-only");
	QVERIFY2(!commander.output().contains("Added "),					"Analyses were created in a community build");

	AppInfo::setProMode(wasPro);
}

void TestEngine::testFilters()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");

	_engines->startStoppedEngine(_engineRep);

	QSignalSpy spy(_engineRep, SIGNAL(filterDone(int)));

	QVERIFY2(spy.isValid(),	"Spy is broken!");

	_data->filter()->setRFilter("V1%%2==0");
	_engines->sendFilter("", tq(_data->filter()->rFilter()));

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	QVariantList response = spy.takeFirst();
	spy.clear();

	std::cout << "Response was: " << response[0].toString() << std::endl;

	_data->checkForUpdates();

	QVERIFY2(_data->filter()->filteredRowCount() == _data->rowCount() / 2,	"Did not get right filtered rowCount!");
	QVERIFY2(!_data->filter()->filtered()[0],								"Expected first filtered to be FALSE");
	QVERIFY2(_data->filter()->filtered()[1],								"Expected second filtered to be TRUE");

}


QTEST_MAIN(TestEngine)
