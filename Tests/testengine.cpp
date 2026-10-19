#include "dirs.h"
#include "qutils.h"
#include "testengine.h"
#include <QSignalSpy>
#include <QElapsedTimer>
#include "testinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "engine/enginesync.h"
#include "utilities/appdirs.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "engine/enginerepresentation.h"
#include "utilities/settings.h"
#include "filter.h"
#include "variableinfo.h"
#include "scriptconstructormodel.h"
#include "scriptnode.h"
#include "scriptconstructorregistry.h"

#include <json/json.h>
#include <random>
#include <functional>

void TestEngine::initTestCase()
{

}

void TestEngine::init()
{
	Settings::informSettingsThatThisIsATest(); //For deterministic Settings behaviour, as in TestAll.
	TempFiles	::	clearSessionDir();
	Dirs		::	setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles	::	init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

	_pkg		=	new DataSetPackage(this);
	_importer	=	new CSVImporter();
	_engines	=	new EngineSync(this);

	_engines	->	start();
	_engineRep	=	_engines->createNewEngine(true, 0);
	_importer	->	loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int i){});
	_data		=	_pkg->dataSet();
}

void TestEngine::cleanup()
{
	if(_engineRep)
		_engineRep->shutEngineDown();
	
	//Gets cleaned up by EngineSync
	_engineRep = nullptr;

	delete _engines;
	_engines = nullptr;

	DatabaseInterface::singleton()->close();
	DatabaseInterface::singleton()->closeInterfaces();

	delete _pkg;
	_pkg = nullptr;

	_data = nullptr;

	delete _importer;
	_importer = nullptr;
}

void TestEngine::testComputedColumns()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");
	
	_engines->startStoppedEngine(_engineRep);
	
	// int dataSetId, const QString & columnName, const QString & warning, bool dataChanged
	QSignalSpy spy(_engineRep, SIGNAL(computeColumnSucceeded(int, const QString &, const QString &, bool))); 
	
	QVERIFY2(spy.isValid(),	"Spy is broken!");
	
	if(!_data->column("V1")->hasLabels())
		_data->column("V1")->noLabelsToLabels();	
		
	Column * col = _data->column("contBinom");
	col->setCodeType(computedColumnType::rCode);
	col->setRCode("V1");
	
	_engines->computeColumn(_data->id(), "contBinom", tq(col->rCode()), columnType::ordinal);
	
	spy.wait();
	
	QVERIFY2(spy.count() == 1,	"Did not get a response");
	
	QVariantList response = spy.takeFirst();
	spy.clear();
	
	QVERIFY2(response[0].toInt() == _data->id(),	"Did not get the right dataSet back in response");
	QVERIFY2(response[1].toString() == "contBinom",	"Did not get the right column back in response");
	QVERIFY2(response[2].toString() == "",			"Got a warning!");
	QVERIFY2(response[3].toBool(),					"Did not get dataChanged back in response");

	col->checkForUpdates();
	

	Json::Value		jsonContBinom	= col->jsonForCompare(),
					jsonV1			= _data->column("V1")->jsonForCompare();

	std::cout << jsonContBinom.toStyledString() << "\n" << jsonV1.toStyledString() << std::endl;
	
	QVERIFY2(jsonContBinom["labels"] == jsonV1["labels"], "Labels are not the same");
	QVERIFY2(jsonContBinom["data"]   == jsonV1["data"],   "Data is not the same");



	//Now lets see if it can also not be the same:
	col->setRCode("V1+1");

	_engines->computeColumn(_data->id(), "contBinom", tq(col->rCode()), columnType::scale);

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	response = spy.takeFirst();
	spy.clear();

	QVERIFY2(response[0].toInt() == _data->id(),	"Did not get the right dataSet back in response");
	QVERIFY2(response[1].toString() == "contBinom",	"Did not get the right column back in response");
	QVERIFY2(response[2].toString() == "",			"Got a warning!");
	QVERIFY2(response[3].toBool(),					"Did not get dataChanged back in response");

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

	_engines->computeColumn(_data->id(), "contcor1", tq(col2->rCode()), columnType::scale);

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	response = spy.takeFirst();
	spy.clear();

	QVERIFY2(response[0].toInt() == _data->id(),	"Did not get the right dataSet back in response");
	QVERIFY2(response[1].toString() == "contcor1",	"Did not get the right column back in response");
	QVERIFY2(response[2].toString() == "",			"Got a warning!");
	QVERIFY2(response[3].toBool(),					"Did not get dataChanged back in response");

	col2->checkForUpdates();

	Json::Value		jsonContCor1	= col2->jsonForCompare();

	//std::cout << jsonContCor1.toStyledString() /*<< "\n" << jsonV1.toStyledString()*/ << std::endl;

	QVERIFY2(jsonContCor1["data"]   == jsonV1["data"],   "Data is not the same");
	QVERIFY2(jsonContCor1["labels"] == jsonV1["labels"], "Labels are not the same");

}

void TestEngine::testComputedColumnCascade()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");

	_engines->startStoppedEngine(_engineRep);

	//The cascade dispatches computed columns from the data layer back to the engine via this
	//connection (normally set up by MainWindow). Replicate it here so the test can exercise it.
	connect(DataSetPackage::pkg(), &DataSetPackage::runComputedColumn, _engines, &EngineSync::computeColumn, Qt::QueuedConnection);

	QSignalSpy spy(_engineRep, SIGNAL(computeColumnSucceeded(int, const QString &, const QString &, bool)));

	QVERIFY2(spy.isValid(),	"Spy is broken!");

	Column * colA = _data->column("contBinom");
	Column * colB = _data->column("contcor1");

	colA->setCodeType(computedColumnType::rCode);
	colB->setCodeType(computedColumnType::rCode);

	//colB depends on colA, so it may only run after colA has been validated.
	colA->setRCode("V1");
	colB->setRCode("contBinom-1");

	QVERIFY2(colA->invalidated(),	"contBinom should be invalidated after setting its code");
	QVERIFY2(colB->invalidated(),	"contcor1 should be invalidated after setting its code");

	//colA is still invalidated, so colB (which depends on it) must not be runnable yet.
	QVERIFY2(colB->iShouldBeSentAgain() == false,	"contcor1 should not run while its dependency contBinom is still invalidated");

	//Run colA; the connected coordinator should validate it and cascade so that colB gets dispatched too.
	_engines->computeColumn(_data->id(), "contBinom", tq(colA->rCode()), columnType::scale);

	bool gotColA = false, gotColB = false;

	//Give the engine plenty of time: the very first engine/R initialization in a fresh session can be slow.
	while((!gotColA || !gotColB) && spy.wait(120000))
	{
		while(spy.count() > 0)
		{
			QVariantList response = spy.takeFirst();
			if(response[1].toString() == "contBinom")	gotColA = true;
			else if(response[1].toString() == "contcor1") gotColB = true;
		}
	}

	QVERIFY2(gotColA,	"contBinom never reported as computed");
	QVERIFY2(gotColB,	"contcor1 never ran: the cascade from contBinom to its dependents is broken");

	colA->checkForUpdates();
	colB->checkForUpdates();

	//The coordinator validates/depends dispatch happens through QueuedConnections, so let the
	//event loop settle before checking the final invalidation state.
	QElapsedTimer	waitTimer;
	waitTimer.start();
	while((colA->invalidated() || colB->invalidated()) && waitTimer.elapsed() < 5000)
		QCoreApplication::processEvents();

	QVERIFY2(!colA->invalidated(),	"contBinom should be validated after a successful compute");
	QVERIFY2(!colB->invalidated(),	"contcor1 should be validated after its dependency ran");

	disconnect(DataSetPackage::pkg(), &DataSetPackage::runComputedColumn, _engines, &EngineSync::computeColumn);
}

void TestEngine::testComputedDataSet()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");

	_engines->startStoppedEngine(_engineRep);

	//The dispatch goes through this connection (normally set up by MainWindow), replicate it here.
	connect(DataSetPackage::pkg(), &DataSetPackage::runComputedDataSet, _engines, &EngineSync::computeDataSet, Qt::QueuedConnection);

	QSignalSpy spy(_engineRep, SIGNAL(computeDataSetSucceeded(int, const QString &, bool)));

	QVERIFY2(spy.isValid(),	"Spy is broken!");

	DataSet * computed = Workspace::singleton()->createComputedDataSet("computedOut", _data->defaultFilter()->id());

	QVERIFY2(computed,						"Could not create computed dataset!");
	QVERIFY2(computed->isComputed(),		"Computed dataset should be marked as computed!");
	QVERIFY2(computed->defaultInputDataSet() == _data,	"Computed dataset should reference the input dataset!");
	QVERIFY2(computed->invalidated(),		"Computed dataset should start out invalidated!");

	computed->setRCode("data.frame(x = contBinom, y = V1 + 1)");

	QVERIFY2(computed->invalidated(),		"Setting rCode should keep the dataset invalidated");

	//setRCode() dispatches it (through checkForDependentDatasetsToBeSent) once the engine is idle.
	bool gotIt = false;
	while(!gotIt && spy.wait(120000))
	{
		while(spy.count() > 0)
		{
			QVariantList response = spy.takeFirst();
			if(response[0].toInt() == computed->id())
				gotIt = true;
		}
	}

	QVERIFY2(gotIt,	"Computed dataset never reported as computed");

	computed->checkForUpdates();

	//The coordinator validates the dataset through QueuedConnections, so let the event loop settle.
	QElapsedTimer	waitTimer;
	waitTimer.start();
	while(computed->invalidated() && waitTimer.elapsed() < 5000)
		QCoreApplication::processEvents();

	QVERIFY2(!computed->invalidated(),	"Computed dataset should be validated after a successful compute");
	QVERIFY2(computed->column("x"),		"Computed dataset should have the produced column x");
	QVERIFY2(computed->column("y"),		"Computed dataset should have the produced column y");
	QVERIFY2(computed->rowCount() == _data->rowCount(),	"Computed dataset should have the same number of rows as its input");

	//A second computed dataset that uses the first one as its input must wait for it to be valid,
	//then cascade.
	DataSet * computed2 = Workspace::singleton()->createComputedDataSet("computedOut2", computed->defaultFilter()->id());

	QVERIFY2(computed2,						"Could not create dependent computed dataset!");
	computed2->setRCode("data.frame(z = x * 2)");

	QVERIFY2(computed2->iShouldBeSentAgain(),	"Dependent computed dataset should be runnable once its input is valid");

	//computed is already valid, so dispatch computed2 and wait for it.
	bool gotIt2 = false;
	while(!gotIt2 && spy.wait(120000))
	{
		while(spy.count() > 0)
		{
			QVariantList response = spy.takeFirst();
			if(response[0].toInt() == computed2->id())
				gotIt2 = true;
		}
	}

	QVERIFY2(gotIt2,	"Dependent computed dataset never ran: the dataset cascade is broken");

	computed2->checkForUpdates();

	QElapsedTimer	waitTimer2;
	waitTimer2.start();
	while(computed2->invalidated() && waitTimer2.elapsed() < 5000)
		QCoreApplication::processEvents();

	QVERIFY2(!computed2->invalidated(),	"Dependent computed dataset should be validated after its input ran");
	QVERIFY2(computed2->column("z"),		"Dependent computed dataset should have the produced column z");

	//Regression guard against the infinite-recompute loop. The deterministic oracle is the *state*: a
	//recompute livelock necessarily leaves a dataset invalidated or wanting to be sent again, because
	//iShouldBeSentAgain() gates every (re)dispatch. This is machine-speed independent, unlike waiting
	//a fixed wall-clock time for a signal that never comes.
	QVERIFY2(!computed->invalidated(),		"Computed dataset should not be invalidated after validation");
	QVERIFY2(!computed->iShouldBeSentAgain(),	"Computed dataset should not want another compute");
	QVERIFY2(!computed2->invalidated(),		"Dependent computed dataset should not be invalidated after validation");
	QVERIFY2(!computed2->iShouldBeSentAgain(),	"Dependent computed dataset should not want another compute");

	//Secondary signal-level guard: after draining and a short settle, no recompute may have arrived.
	while(spy.count() > 0)
		spy.takeFirst();

	QElapsedTimer	settleTimer;
	settleTimer.start();
	while(settleTimer.elapsed() < 1500)
		QCoreApplication::processEvents();

	QCOMPARE(spy.count(), 0);

	disconnect(DataSetPackage::pkg(), &DataSetPackage::runComputedDataSet, _engines, &EngineSync::computeDataSet);
}

void TestEngine::testFilters()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");

	_engines->startStoppedEngine(_engineRep);

	QSignalSpy spy(_engineRep, SIGNAL(filterDone(int)));

	QVERIFY2(spy.isValid(),	"Spy is broken!");

	_data->defaultFilter()->setRFilter("V1%%2==0");
	_engines->sendFilter(_data->id(), "", tq(_data->defaultFilter()->rFilter()));

	spy.wait();

	QVERIFY2(spy.count() == 1,	"Did not get a response");

	QVariantList response = spy.takeFirst();
	spy.clear();

	std::cout << "Response was: " << response[0].toString() << std::endl;

	_data->checkForUpdates();

	QVERIFY2(_data->defaultFilter()->filteredRowCount() == _data->rowCount() / 2,	"Did not get right filtered rowCount!");
	QVERIFY2(!_data->defaultFilter()->filtered()[0],								"Expected first filtered to be FALSE");
	QVERIFY2(_data->defaultFilter()->filtered()[1],									"Expected second filtered to be TRUE");

}

void TestEngine::testVariableInfoPerFilter()
{
	DataSet * ds = _data;

	// Programmatically ensure data exists (CSV import may be environment-dependent)
	if(ds->columnCount() == 0)
	{
		ds->beginBatchedToDB();
		ds->setColumnCount(2);
		ds->setRowCount(6);
		auto lookup = [](size_t r) -> std::string { return std::to_string(int(r + 1)); };
		ds->column(0)->initFromLookups("V1", 6, lookup, lookup, "V1", columnType::unknown, {}, 10, true);
		auto lookup2 = [](size_t r) -> std::string { return std::string(1, char('a' + r)); };
		ds->column(1)->initFromLookups("V2", 6, lookup2, lookup2, "V2", columnType::unknown, {}, 10, true);
		ds->endBatchedToDB([](float){});
	}

	QCOMPARE_GE(ds->columnCount(),	2);
	QCOMPARE_GE(ds->rowCount(),		6);

	int rowCount		 = ds->rowCount();

	//NB: vector<bool>(count, value) — use the full row-count, every row passes.
	ds->defaultFilter()->setFilterVector(boolvec(rowCount, true));
	QCOMPARE(ds->defaultFilter()->filteredRowCount(), rowCount);

	Filter * filterEven = ds->createFilter("filterEven", true);
	Filter * filterOdd  = ds->createFilter("filterOdd",  true);

	QVERIFY2(filterEven != ds->defaultFilter(),	"Named filter should be different from default");
	QVERIFY2(filterOdd  != ds->defaultFilter(),	"Named filter should be different from default");
	QVERIFY2(filterEven != filterOdd,			"Two named filters should be different");

	boolvec evenFilter(rowCount, false);
	for(int i=0; i<rowCount; i+=2)	evenFilter[i] = true;
	QVERIFY2(filterEven->setFilterVector(evenFilter),	"Could not set filter vector for filterEven");

	boolvec oddFilter(rowCount, false);
	for(int i=1; i<rowCount; i+=2)	oddFilter[i] = true;
	QVERIFY2(filterOdd->setFilterVector(oddFilter),		"Could not set filter vector for filterOdd");

	//evenFilter sets rows 0,2,... (ceil(rowCount/2)), oddFilter rows 1,3,... (floor(rowCount/2)).
	//The fixture has an odd row count here, so the two counts differ; don't assume both equal rowCount/2.
	QCOMPARE(filterEven->filteredRowCount(), int((rowCount + 1) / 2));
	QCOMPARE(filterOdd->filteredRowCount(),  int(rowCount / 2));

	VariableInfo info;
	info.setProvider(filterEven);

	QCOMPARE(info.rowCount(),			filterEven->filteredRowCount());
	QCOMPARE(info.variableCount(),		ds->columnCount());

	QStringList vars = info.provider()->provideInfo(varInfoType::VariableNames).toStringList();
	QVERIFY2(vars.size() > 0,			"Should have variable names");
	QVERIFY2(vars.contains(tq(ds->column(0)->name())),		"Should contain first column");
	QVERIFY2(vars.contains(tq(ds->column(1)->name())),		"Should contain second column");

	info.setProvider(filterOdd);
	QCOMPARE(info.rowCount(),			filterOdd->filteredRowCount());
	QCOMPARE(info.variableCount(),		ds->columnCount());

	QStringList varsOdd = info.provider()->provideInfo(varInfoType::VariableNames).toStringList();
	QCOMPARE(vars, varsOdd);

	Filter * defaultF = ds->defaultFilter();
	QCOMPARE(defaultF->filteredRowCount(), rowCount);

	info.setProvider(defaultF);
	QCOMPARE(info.rowCount(), rowCount);
}

void TestEngine::testScriptConstructorFuzz()
{
	QVERIFY2(_data,			"No dataset!");
	QVERIFY2(_engines,		"No EngineSync!");
	QVERIFY2(_engineRep,	"No EngineRepresentation!");

	_engines->startStoppedEngine(_engineRep);

	QSignalSpy spy(_engineRep, &EngineRepresentation::rCodeReturned);
	QVERIFY2(spy.isValid(), "Spy is broken!");

	// JSON node builders (mirror Tests/testall.cpp) restricted to complete, type-valid trees
	// made from numeric/boolean literals only. Leaves are literals so the generated R depends
	// only on base R (no columns, no JASP-only helper functions) and always evaluates without
	// error: R turns bad arithmetic (x/0, log of a negative, sqrt of a negative) into Inf/NaN,
	// not an error.
	auto numNode = [](double v)
	{
		Json::Value j; j["nodeType"] = "Number"; j["value"] = v; return j;
	};
	auto boolNode = [](bool b)
	{
		Json::Value j; j["nodeType"] = "Boolean"; j["value"] = b ? "TRUE" : "FALSE"; return j;
	};
	auto opNode = [](const std::string & op, const Json::Value & l, const Json::Value & r)
	{
		Json::Value j; j["nodeType"] = "Operator"; j["operator"] = op;
		j["leftArgument"] = l; j["rightArgument"] = r; return j;
	};
	auto funcNode = [](const std::string & name, const std::vector<Json::Value> & args)
	{
		Json::Value j; j["nodeType"] = "Function"; j["functionName"] = name; j["arguments"] = Json::arrayValue;
		for(const Json::Value & a : args)
		{
			Json::Value arg; arg["name"] = "x"; arg["dropKeys"] = Json::arrayValue; arg["argument"] = a;
			j["arguments"].append(arg);
		}
		return j;
	};

	static const std::vector<std::string> arithOps	= {"+", "-", "*", "/", "^", "%%"};
	static const std::vector<std::string> compareOps	= {"==", "!=", "<", "<=", ">", ">="};
	// Base-R numeric functions that accept a single numeric argument and return a number.
	static const std::vector<std::string> unaryNumFuncs	= {"abs", "sqrt", "sign", "exp", "log", "log2", "log10", "mean", "sd", "var", "sum", "prod", "min", "max", "median"};

	std::function<Json::Value(std::mt19937 &, int)> makeNumber = [&](std::mt19937 & rng, int depth) -> Json::Value
	{
		std::uniform_int_distribution<int> kind(0, 99);
		std::uniform_real_distribution<double> val(1.0, 50.0);
		std::uniform_int_distribution<int> arithPick(0, static_cast<int>(arithOps.size()) - 1);
		std::uniform_int_distribution<int> funcPick(0, static_cast<int>(unaryNumFuncs.size()) - 1);

		if(depth <= 0)
			return numNode(val(rng));

		int k = kind(rng);
		if(k < 30) return numNode(val(rng));
		if(k < 65) return opNode(arithOps[arithPick(rng)], makeNumber(rng, depth - 1), makeNumber(rng, depth - 1));
		if(k < 80)
		{
			const std::string & fn = unaryNumFuncs[funcPick(rng)];
			return funcNode(fn, {makeNumber(rng, depth - 1)});
		}
		// round needs a numeric "n" as well.
		return funcNode("round", {makeNumber(rng, depth - 1), numNode(std::uniform_int_distribution<int>(0, 3)(rng))});
	};

	std::function<Json::Value(std::mt19937 &, int)> makeBoolean = [&](std::mt19937 & rng, int depth) -> Json::Value
	{
		std::uniform_int_distribution<int> kind(0, 99);
		std::uniform_int_distribution<int> cmpPick(0, static_cast<int>(compareOps.size()) - 1);

		if(depth <= 0)
			return boolNode(kind(rng) % 2 == 0);

		int k = kind(rng);
		if(k < 25) return boolNode(k % 2 == 0);
		if(k < 60) return opNode(compareOps[cmpPick(rng)], makeNumber(rng, depth - 1), makeNumber(rng, depth - 1));
		if(k < 85) return opNode(k % 2 == 0 ? "&" : "|", makeBoolean(rng, depth - 1), makeBoolean(rng, depth - 1));
		return funcNode("!", {makeBoolean(rng, depth - 1)});
	};

	ScriptConstructorModel model;
	model.setMode(ScriptConstructorMode::ComputedColumn);

	int requestId = 0;
	const int cases = 40;

	for(int seed = 0; seed < cases; seed++)
	{
		std::mt19937 rng(seed);

		Json::Value tree;
		tree["formulas"] = Json::arrayValue;
		tree["formulas"].append(seed % 2 == 0 ? makeNumber(rng, 3) : makeBoolean(rng, 3));

		model.fromJson(tree);
		const std::string rCode = model.toR();

		// jaspRCPP_evalRCode returns "null" for any non-string R result, which the engine treats
		// as an error. Wrap the generated expression so it always yields a string: "OK" when it
		// evaluates without error, "ERR" when R raises. This makes the distinction between a real
		// evaluation error and a merely non-string (e.g. numeric) result explicit.
		const std::string wrapped = "tryCatch({" + rCode + "; 'OK'}, error = function(e) 'ERR')";

		_engines->sendRCode(_data->id(), tq(wrapped), requestId, false, "");

		// Wait for the reply for our request id and confirm the R evaluated without error.
		bool gotIt = false;
		QString result;
		const int target = requestId;

		while(!gotIt && spy.wait(120000))
		{
			while(spy.count() > 0)
			{
				QVariantList response = spy.takeFirst();
				if(response[1].toInt() == target)
				{
					gotIt	= true;
					result	= response[0].toString();
				}
			}
		}

		if(!gotIt)
		{
			const std::string msg = "No R reply for request " + std::to_string(target) + " (seed " + std::to_string(seed) + "), rCode: " + rCode;
			QFAIL(msg.c_str());
		}

		if(result != "OK")
		{
			const std::string msg = "Generated R evaluated with error (seed " + std::to_string(seed) + "): " + rCode + " (result: " + fq(result) + ")";
			QFAIL(msg.c_str());
		}

		requestId++;
	}
}


QTEST_MAIN(TestEngine)
