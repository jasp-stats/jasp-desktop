#include "dirs.h"
#include "testengine.h"
#include <QSignalSpy>
#include "testinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "utilities/qutils.h"
#include "engine/enginesync.h"
#include "utilities/appdirs.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "engine/enginerepresentation.h"

void TestEngine::initTestCase()
{

}

void TestEngine::init()
{
	TempFiles	::	clearSessionDir();
	Dirs		::	setLocalAppdataDir(AppDirs::appData(false).toStdString());
	TempFiles	::	init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory

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

	//std::cout << jsonContBinom.toStyledString() << "\n" << jsonV1.toStyledString() << std::endl;
	
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

	QVERIFY2(jsonContBinom["labels"] != jsonV1["labels"], "Labels are the same, but they shouldnt be");
	QVERIFY2(jsonContBinom["data"]   != jsonV1["data"],   "Data is the same, but they shouldnt be");

	Column * col2 = _data->column("contcor1");
	col2->setCodeType(computedColumnType::rCode);
	col2->setRCode("contBinom-1"); //Should make it the same as V1 again

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
