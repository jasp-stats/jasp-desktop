#include "testall.h"
#include "testinfo.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "qutils.h"
#include "databaseinterface.h"
#include "data/datasetpackage.h"
#include "data/importers/csvimporter.h"
#include "data/importers/odsimporter.h"
#include "data/importers/jaspimporter.h"
#include "data/exporters/jaspexporter.h"
#include "data/exporters/dataexporter.h"
#include "data/importers/excelimporter.h"
#include "data/importers/rdataimporter.h"
#include "data/importers/readstatimporter.h"
#include "utilities/settings.h"
#include "gui/preferencesmodel.h"
#include "utilities/desktopcommunicator.h"
#include "datasetsyncer.h"
#include "dataset.h"
#include "workspace.h"
#include "undostack.h"
#include "data/asyncloader.h"
#include "mainwindow.h"
#include "results/resultsjsinterface.h"

#include <QSignalSpy>
#include <QFile>
#include <QFileInfo>
#include <QEventLoop>
#include <QTimer>
#include <sqlite3.h>


void TestAll::initTestCase()
{
	TempFiles::init(ProcessInfo::currentPID()); // needed here so that the LRNAM can be passed the session directory
}

void TestAll::init()
{
	Settings::informSettingsThatThisIsATest();
	//The CSV delimiter scratchpad (_knownCsvDelimiter) is a per-import value in production
	//(reset by DataSetLoader); make sure a leftover value can never leak between tests.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');
	//_pkg->reset(false);
}

void TestAll::cleanup()
{
	
	delete _importer;
	_importer = nullptr;

	//After a test tore down a full MainWindow the session database is gone with it, so the
	//DatabaseInterface the singleton() call would lazily recreate here cannot load anything anymore.
	//closeInterfaces() alone is enough: deleting a null pointer is fine and the destructor closes.
	try
	{
		DatabaseInterface::closeInterfaces();
	}
	catch(const std::exception & e)
	{
		std::cerr << "TestAll::cleanup: skipping database teardown: " << e.what() << std::endl;
	}
	delete _pkg;
	_pkg = nullptr;
}

bool TestAll::_newPkgWithDataSet()
{
	delete _importer;
	_importer = nullptr;
	delete _pkg;
	_pkg = nullptr;

	_pkg = new DataSetPackage(this);

	//Reset the per-import CSV delimiter scratchpad so it can't leak from a previous import.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');

	CSVImporter importer;
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int){});

	return _pkg->dataSet() != nullptr;
}

#define TO_STR2(x) #x
#define TO_STR(x) TO_STR2(x)


void TestAll::testDataImport_data()
{
	QTest::addColumn<QString>("folder");
	QTest::addColumn<QString>("dataFileAbsolutePath");

	for(const QString & folder : _testLibrary().entryList(QDir::Filter::Dirs | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
	{
		if(folder == "jasp")
			continue;

		QDir subDir(_testLibrary());
		subDir.cd(folder);

		for(QFileInfo & i : subDir.entryInfoList(QDir::Filter::Files | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
			if(i.suffix() != "json")
				QTest::newRow(i.fileName().toUtf8()) << folder << i.absoluteFilePath();
	}
}

void TestAll::testDataImport()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	auto getImporter = [&]() -> Importer *
	{
		if(folder == "readstat")	return new ReadStatImporter();
		if(folder == "rdata")		return new RDataImporter();
		if(folder == "excel")		return new ExcelImporter();
		if(folder == "ods")			return new ods::ODSImporter();
		if(folder == "csv")			return new CSVImporter();

		return nullptr;
	};

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);
	_importer = getImporter();

	QVERIFY2(_importer, "Getting importer failed...");

	//Reset the per-import CSV delimiter scratchpad so one file's delimiter can't leak into the next.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');

	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;
	_importer->loadDataSet(fq(dataFileAbsolutePath), _pkg->createDataSet(), [](int i){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,						"No dataset!");

	Json::Value compareMe = dataSet->jsonForCompare();

	QString jsonFilePath = dataFileAbsolutePath,
			ext			 = QFileInfo(dataFileAbsolutePath).suffix();

	jsonFilePath.replace(jsonFilePath.size() - (ext.size() + 1), ext.size() + 1, ".json");

	QFileInfo jsonFileIn(jsonFilePath);

	if(!jsonFileIn.exists())
	{
		std::cerr << "Json does not exist yet, creating it now!" << std::endl;
		QFile jsonFile(jsonFilePath);
		jsonFile.open(QFile::OpenModeFlag::WriteOnly);
		jsonFile.write(compareMe.toStyledString().c_str());
		jsonFile.close();
	}

	QVERIFY(jsonFileIn.exists());

	QFile jsonFile(jsonFilePath);

	jsonFile.open(QFile::OpenModeFlag::ReadOnly);

	std::string jsonTxt  = fq(jsonFile.readAll());

	Json::Reader parser;
	Json::Value  hardcoded;

	QVERIFY2(parser.parse(jsonTxt, hardcoded),	"Parsing json failed!");

	bool hardcodedIsSame = hardcoded == compareMe;

	if(!hardcodedIsSame)
		std::cerr << stringUtils::replaceBy(compareMe.toStyledString(), "\n", " ") << std::endl;

	QVERIFY2(hardcodedIsSame,			"Hardcoded json is different!");

	
	DataSet loadMe(nullptr, dataSet->id());
	QVERIFY2(dataSet->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}


void TestAll::testJaspDataImport_data()
{
	QTest::addColumn<QString>("folder");
	QTest::addColumn<QString>("dataFileAbsolutePath");

	for(const QString & folder : _testLibrary().entryList(QDir::Filter::Dirs | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
	{
		if(folder != "jasp")
			continue;

		QDir subDir(_testLibrary());
		subDir.cd(folder);

		for(QFileInfo & i : subDir.entryInfoList(QDir::Filter::Files | QDir::Filter::NoDotAndDotDot | QDir::Filter::NoSymLinks))
			if(i.suffix() != "json")
				QTest::newRow(i.fileName().toUtf8()) << folder << i.absoluteFilePath();
	}
}

void TestAll::testJaspRoundRobin_data()
{
	testJaspDataImport_data();
}

void TestAll::testJaspRoundRobin()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);
	
	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;
	JASPImporter::loadDataSet(fq(dataFileAbsolutePath),		[](int){});
	
	DataSet *	dataSet		= _pkg->dataSet();
	QVERIFY2(dataSet,			"No dataset!");
	
	Json::Value compareMe	= dataSet->jsonForCompare();
	std::string jaspFile	= TempFiles::createSpecific("testjasp", "temp.jasp");

	std::cerr << "Storing jasp file temporarily to: " << jaspFile << std::endl;
	// Create snapshot before exporting
	JASPExporter::createSnapshot("testjasp_snapshot_");
	JASPExporter().saveDataSet(jaspFile, [](int){});
	
	_pkg->reset();
	QVERIFY2(_pkg->dataSet()->jsonForCompare() != compareMe, "DataSet should be different after resetting DataSetPackage!");
	
	JASPImporter::loadDataSet(jaspFile, [](int){});
	
	dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,									"No dataset!");
	QVERIFY2(dataSet->jsonForCompare() == compareMe,	"DataSet should be the same after reloading!");
}


void TestAll::testJaspDataImport()
{
	QFETCH(QString, folder);
	QFETCH(QString, dataFileAbsolutePath);

	QDir subDir(_testLibrary());
	subDir.cd(folder);

	if(_pkg)
		delete _pkg;

	if(_importer)
		delete _importer;

	_pkg = new DataSetPackage(this);
	
	std::cerr << "Testing " << dataFileAbsolutePath << std::endl;

	JASPImporter::loadDataSet(fq(dataFileAbsolutePath),		[](int){});
	
	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet,						"No dataset!");

	Json::Value compareMe = dataSet->jsonForCompare();

	QString jsonFilePath = dataFileAbsolutePath,
			ext			 = QFileInfo(dataFileAbsolutePath).suffix();

	jsonFilePath.replace(jsonFilePath.size() - (ext.size() + 1), ext.size() + 1, ".json");

	QFileInfo jsonFileIn(jsonFilePath);

	if(!jsonFileIn.exists())
	{
		std::cerr << "Json does not exist yet, creating it now!" << std::endl;
		QFile jsonFile(jsonFilePath);
		jsonFile.open(QFile::OpenModeFlag::WriteOnly);
		jsonFile.write(compareMe.toStyledString().c_str());
		jsonFile.close();

	}

	QVERIFY(jsonFileIn.exists());

	QFile jsonFile(jsonFilePath);
	
	
	jsonFile.open(QFile::OpenModeFlag::ReadOnly);

	std::string jsonTxt  = fq(jsonFile.readAll());

	Json::Reader parser;
	Json::Value  hardcoded;

	QVERIFY2(parser.parse(jsonTxt, hardcoded),	"Parsing json failed!");

	bool hardcodedIsSame = hardcoded == compareMe;

	if(!hardcodedIsSame)
		std::cerr << stringUtils::replaceBy(compareMe.toStyledString(), "\n", " ") << std::endl;

	QVERIFY2(hardcodedIsSame,			"Hardcoded json is different!");

	
	DataSet loadMe(nullptr, dataSet->id());
	QVERIFY2(dataSet->jsonForCompare() == loadMe.jsonForCompare(), "DataSet isnt the same after dbload!");
}

// Regression test for https://github.com/jasp-stats/jasp-desktop/commit/0a90b9a34e9d754f55bc32ec1efd2f67940ef756
// setDataSetSize() pre-allocates rows before initFromLookups() is called, causing rowCount() > 0
// when setValues() checks allTheSame — which skipped the label-detection loop and silently dropped
// all SPSS value labels.
void TestAll::testSavLabels()
{
	if(_pkg)	delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new ReadStatImporter();

	const QString savPath = _testLibrary().absoluteFilePath("readstat/Labelled_data.sav");
	_importer->loadDataSet(fq(savPath), _pkg->createDataSet(), [](int){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet, "No dataset!");

	// These columns have SPSS value labels (e.g. 1->"Soha", 2->"Havonta vagy kevesebbszer", …)
	// and must be imported as labelled (nominal/ordinal) columns.
	const QStringList labelledColumns = {
		"AUDIT_gyakorisag",
		"AUDIT_mennyiség",
		"PHQ14_fejfajas",
		"PHQ14_szivveres",
		"PHQ9_energia"
	};

	for(const QString & colName : labelledColumns)
	{
		Column * col = dataSet->column(fq(colName));
		QVERIFY2(col,				qPrintable("Column not found: "	+ colName));
		QVERIFY2(col->hasLabels(),			qPrintable("Column has no labels: "  + colName));
		QVERIFY2(col->labels().size() > 0,	qPrintable("Label list is empty: "   + colName));
	}

	// Spot-check: AUDIT_gyakorisag label 1 should be "Soha"
	Column * audit = dataSet->column("AUDIT_gyakorisag");
	QVERIFY2(audit, "AUDIT_gyakorisag column not found");

	bool foundSoha = false;
	for(const Label * label : audit->labels())
		if(label->labelDisplay() == "Soha") { foundSoha = true; break; }

	QVERIFY2(foundSoha, "Expected label 'Soha' not found in AUDIT_gyakorisag");

	// Scale columns must NOT have labels
	const QStringList scaleColumns = { "Eletkor", "MHC_SF_Emo", "PSS_10" };
	for(const QString & colName : scaleColumns)
	{
		Column * col = dataSet->column(fq(colName));
		QVERIFY2(col, qPrintable("Column not found: " + colName));
		QVERIFY2(!col->hasLabels(), qPrintable("Scale column should not have labels: " + colName));
	}
}

// Regression test for https://github.com/jasp-stats/jasp-issues/issues/4293
void TestAll::testFilterLabels()
{
	if(_pkg)	delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new ReadStatImporter();

	const QString filePath = _testLibrary().absoluteFilePath("jasp/Directed Reading Activities.jasp");
	JASPImporter::loadDataSet(fq(filePath),		[](int){});

	DataSet * dataSet = _pkg->dataSet();
	QVERIFY2(dataSet, "No dataset!");

	std::string colName = "group";
	Column * col = dataSet->column(colName);
	QVERIFY2(col,										qPrintable("Group Column not found"));
	QVERIFY2(col->hasLabels(),							qPrintable("Group has no labels"));
	QVERIFY2(col->labelsNonEmptyCount() == 2,			qPrintable(tq("Number of labels is not 2: ")) + col->labelsNonEmptyCount());

	Label * controlLabel = col->labelByIndexNonEmpty(0);
	Label * treatLabel = col->labelByIndexNonEmpty(1);
	QVERIFY2(controlLabel->label() == "Control",		qPrintable("First label is not 'Control'"));
	QVERIFY2(controlLabel->filterAllows(),				qPrintable("'Control' label is filtered"));
	QVERIFY2(treatLabel->label() == "Treat",			qPrintable("Second label is not 'Treat'"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Do as if the user clicked on Filter for the Control label in the Label window
	col->setLabelAllowFilter(0, false);
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Not all labels can be unset: nothing should change
	col->setLabelAllowFilter(1, false);
	QVERIFY2(!controlLabel->filterAllows(),				qPrintable("'Control' label is not filtered"));
	QVERIFY2(treatLabel->filterAllows(),				qPrintable("'Treat'label is filtered"));

	// Set first the Control label, and unset the Treat label: this time it should work
	col->setLabelAllowFilter(0, true);
	col->setLabelAllowFilter(1, false);
	QVERIFY2(controlLabel->filterAllows(),				qPrintable("'Control' label is filtered"));
	QVERIFY2(!treatLabel->filterAllows(),				qPrintable("'Treat'label is not filtered"));
}

// ---------- DataSetSyncer tests ----------

void TestAll::testSyncerStartStopFileSyncing()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();
	QVERIFY(!syncer.isFileSyncing());

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("test_startstop.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("a,b,c\n1,2,3\n");
	f.close();

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), testFilePath);
	QVERIFY(ds->dataFileSynch());

	syncer.stopFileSyncing();
	QVERIFY(!syncer.isFileSyncing());
	QVERIFY(!ds->dataFileSynch());
}

void TestAll::testSyncerFileChangeEmitsSignal()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("sync_emit.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n1,2\n");
	f.close();

	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	QTest::qSleep(1200);

	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n3,4\n");
	f.close();

	// The file watcher signal is async; we check via syncRequired spy.
	// Signal args: (int dataSetId, DataSet * dataSet, QString locator, QString extension, QString databaseJson).
	QSignalSpy spy(&syncer, &DataSetSyncer::syncRequired);
	QTRY_COMPARE_WITH_TIMEOUT(spy.count(), 1, 5000);

	QList<QVariant> args = spy.takeFirst();
	QCOMPARE(args.size(), 4); //(DataSet*, locator, extension, databaseJson)
	QCOMPARE(args[1].toString(), testFilePath);
	QCOMPARE(args[2].toString(), QString("csv")); //extension
	QVERIFY(args[3].toString().isEmpty()); //databaseJson

	syncer.stopFileSyncing();
}

void TestAll::testSyncerStartStopDatabaseSyncing()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());

	Json::Value dbJson;
	dbJson["dbType"] = "NOTCHOSEN";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());
	QVERIFY(ds->isDatabase());
	QVERIFY(syncer.databaseJson() != Json::nullValue);

	syncer.stopDatabaseSyncing();
	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());
}

void TestAll::testSyncerSyncNowWithoutDataSource()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QSignalSpy spy(&syncer, &DataSetSyncer::askUserForRelink);

	syncer.syncNow();

	QTRY_COMPARE_WITH_TIMEOUT(spy.count(), 1, 1000);
	QCOMPARE(spy.takeFirst()[0].toInt(), ds->id());
}

void TestAll::testSyncerMultipleStartStop()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString path1 = tempDir.filePath("multi1.csv");
	QString path2 = tempDir.filePath("multi2.csv");

	auto makeFile = [&](const QString & p)
	{
		QFile f(p);
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write("a\n1\n");
		f.close();
	};

	makeFile(path1);
	makeFile(path2);

	syncer.startFileSyncing(path1);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path1);

	syncer.startFileSyncing(path2);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path2);

	syncer.stopFileSyncing();
	QVERIFY(!syncer.isFileSyncing());

	syncer.startFileSyncing(path1);
	QVERIFY(syncer.isFileSyncing());
	QCOMPARE(QString::fromStdString(ds->dataFilePath()), path1);

	syncer.stopFileSyncing();
}


void TestAll::testDataExporterShownDataSetOnly()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString csvPath = tempDir.filePath("export.csv");

	// Import debug.csv — this creates the first dataset
	DataSet * firstDs = nullptr;
	{
		CSVImporter importer;
		importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), _pkg->createDataSet(), [](int){});
		firstDs = _pkg->dataSet();
		QVERIFY(firstDs);
		QVERIFY(firstDs->rowCount() > 0);
		QVERIFY(firstDs->columnCount() > 0);
	}

	// Create a second, empty dataset and make it the shown one
	DataSet * secondDs = _pkg->createDataSet();
	QVERIFY(secondDs);
	_pkg->workspace()->setShownDataSet(secondDs);
	secondDs = _pkg->dataSet();
	QVERIFY(secondDs);
	QVERIFY(secondDs != firstDs);

	secondDs->setColumnCount(1);
	secondDs->setRowCount(1, false);
	secondDs->column(0)->setName("mycol");
	secondDs->column(0)->setDefaultValues(columnType::scale, false);
	QCOMPARE(secondDs->rowCount(), 1);
	QCOMPARE(secondDs->columnCount(), 1);

	// Set a value manually
	QModelIndex idx = secondDs->index(0, 0);
	secondDs->setData(idx, "testval", Qt::DisplayRole);

	// Export using DataExporter — should export the shownDataSet only
	DataExporter exporter(false);
	exporter.saveDataSet(fq(csvPath), [](int){});

	// Read back and verify
	QFile csvFile(csvPath);
	QVERIFY(csvFile.open(QIODevice::ReadOnly));
	QString content = QString::fromUtf8(csvFile.readAll());
	csvFile.close();

	QStringList lines = content.split('\n', Qt::SkipEmptyParts);

	// Only the shown dataset (mycol) should be written
	QCOMPARE(lines.size(), 2); // header + 1 data row
	QVERIFY(lines[1].contains("testval"));

	// Verify that debug.csv columns are NOT present
	QVERIFY(!lines[0].contains("contNormal"));
	QVERIFY(!lines[0].contains("contGamma"));
}


void TestAll::testSyncerExportModifyReimport()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	// Create an initial CSV
	QString srcPath = tempDir.filePath("source.csv");
	QFile src(srcPath);
	QVERIFY(src.open(QIODevice::WriteOnly));
	src.write("a,b,c\n1,2,3\n4,5,6\n");
	src.close();

	// Import it
	CSVImporter importer;
	importer.loadDataSet(fq(srcPath), _pkg->createDataSet(), [](int){});
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QCOMPARE(ds->rowCount(), 2);
	QCOMPARE(ds->columnCount(), 3);

	// Export to a new location
	QString exportPath = tempDir.filePath("exported.csv");
	DataExporter exporter(false);
	exporter.saveDataSet(fq(exportPath), [](int){});

	// Verify the exported file matches the original content
	QFile exported(exportPath);
	QVERIFY(exported.open(QIODevice::ReadOnly));
	QString exportedContent = QString::fromUtf8(exported.readAll());
	exported.close();

	QVERIFY(exportedContent.contains("a,b,c"));
	QVERIFY(exportedContent.contains("1,2,3"));
	QVERIFY(exportedContent.contains("4,5,6"));
}

void TestAll::testSyncerReleasesSyncGuardOnCompletion()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	// A DB-backed dataset that wants to sync. The re-entrancy guard must be released on *completion*
	// (setSyncingResult), so a subsequent sync can run. If the guard were never released, every
	// later sync would be swallowed — the exact wedge this refactor fixes.
	Json::Value dbJson;
	dbJson["dbType"] = "NOTCHOSEN";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());

	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy(&syncer, &DataSetSyncer::syncingFinished);

	// Trigger sync #1 and complete it.
	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 1, 1000);
	syncer.setSyncingResult(true);
	QCOMPARE(finishedSpy.count(), 1);

	// Trigger sync #2; because the guard was released, it must start again rather than early-return.
	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 2, 1000);
	syncer.setSyncingResult(false);
	QCOMPARE(finishedSpy.count(), 2);

	syncer.stopDatabaseSyncing();
}

void TestAll::testSyncerRetriesFileChangeMissedDuringSync()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("sync_retry.csv");
	QFile f(testFilePath);
	QVERIFY(f.open(QIODevice::WriteOnly));
	f.write("x,y\n1,2\n");
	f.close();

	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy syncRequiredSpy(&syncer, &DataSetSyncer::syncRequired);

	//Deterministic trigger helper: instead of waiting for the OS file watcher (and second-resolution
	//mtimes) we invoke the same slot the watcher is connected to, after resetting the stored timestamp
	//so the mtime filter inside fileChanged always passes. The watcher/mtime path is covered end-to-end
	//by testSyncerFileChangeEmitsSignal; here we only exercise the missed-change replay logic.
	auto changeFile = [&](const char * contents)
	{
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write(contents);
		f.close();
		ds->setDataFileAndTimeStamp(fq(testFilePath), 0);
		QVERIFY(QMetaObject::invokeMethod(&syncer, "fileChanged", Q_ARG(QString, testFilePath)));
	};

	// 1) Trigger a sync that stays in-flight (the guard is held until setSyncingResult).
	changeFile("x,y\n3,4\n");
	QCOMPARE(startedSpy.count(), 1);
	QCOMPARE(syncRequiredSpy.count(), 1);

	// 2) A change arrives while the sync is still in-flight: it must be remembered, not dropped, and
	//    must NOT start a new sync yet (the guard is still held).
	changeFile("x,y\n5,6\n");
	QCOMPARE(startedSpy.count(), 1);

	// 3) Completing the in-flight sync must release the guard AND replay the missed change.
	syncer.setSyncingResult(true);
	QCOMPARE(startedSpy.count(), 2);
	QCOMPARE(syncRequiredSpy.count(), 2);

	syncer.stopFileSyncing();
}

void TestAll::testFilterSetFilterVectorResizesToResult()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QVERIFY(ds->rowCount() > 0);

	Filter * filter = ds->defaultFilter();
	QVERIFY(filter);

	const size_t originalRows = static_cast<size_t>(ds->rowCount());

	//Seed a cache matching the current dataset.
	boolvec initial(originalRows, true);
	initial[0] = false;
	filter->setFilterVector(initial);
	QCOMPARE(filter->filtered().size(), originalRows);

	//The dataset grew: the engine result is authoritative and must be adopted in full (new rows at
	//the end get the engine's value), instead of silently dropping everything past the old size.
	boolvec bigger(originalRows + 3, false);
	bigger[0] = false, bigger[1] = true, bigger[bigger.size() - 1] = true;
	filter->setFilterVector(bigger);
	QCOMPARE(filter->filtered().size(), originalRows + 3);
	QVERIFY(filter->filtered() == bigger);

	//And when the result shrinks, stale tail rows must not survive.
	boolvec smaller(originalRows - 2, true);
	filter->setFilterVector(smaller);
	QCOMPARE(filter->filtered().size(), originalRows - 2);
	QVERIFY(filter->filtered() == smaller);
}

void TestAll::testComputedDataSetCycleDetection()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);

	//Workspace::createDataSet reuses the currently-shown (empty) dataset, so make each one
	//non-empty (by importing) before creating the next, to get three distinct datasets.
	CSVImporter importer;
	const std::string csvPath = fq(_testLibrary().absoluteFilePath("csv/debug.csv"));

	DataSet * a = ws->createDataSet();
	QVERIFY(a);
	importer.loadDataSet(csvPath, a, [](int){});
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(csvPath, b, [](int){});
	DataSet * c = ws->createDataSet();
	QVERIFY(c);
	importer.loadDataSet(csvPath, c, [](int){});

	QVERIFY(a->id() != b->id());
	QVERIFY(b->id() != c->id());
	QVERIFY(a->id() != c->id());

	a->setCodeType(computedColumnType::rCode);
	b->setCodeType(computedColumnType::rCode);
	c->setCodeType(computedColumnType::rCode);

	std::string err;
	QVERIFY(!ws->computedDataSetsHaveLoop(err));

	//A valid chain c -> b -> a is accepted and is not a loop.
	QVERIFY(c->setDefaultInputFilterId(b->defaultFilter()->id()));
	QVERIFY(b->setDefaultInputFilterId(a->defaultFilter()->id()));
	QCOMPARE(c->defaultInputFilterId(), b->defaultFilter()->id());
	QCOMPARE(b->defaultInputFilterId(), a->defaultFilter()->id());
	QVERIFY(!ws->computedDataSetsHaveLoop(err));

	//A depending on C would close the chain into a loop (A <- C <- B <- A) and must be refused,
	//leaving A without an input (the value is unchanged).
	QVERIFY(!a->setDefaultInputFilterId(c->defaultFilter()->id()));
	QCOMPARE(a->defaultInputFilterId(), -1);

	//Likewise A depending on B while B depends on A is a loop and must be refused.
	QVERIFY(!a->setDefaultInputFilterId(b->defaultFilter()->id()));
	QCOMPARE(a->defaultInputFilterId(), -1);

	QVERIFY(!ws->computedDataSetsHaveLoop(err));
}

void TestAll::testUndoColumnDropLevels()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	Column * col = ds->column("contNormal");
	QVERIFY(col);

	UndoStack::setCurrent(ds->undoStack());

	col->setDropLevels(dropLevelsType::drop);
	QCOMPARE(col->dropLevels(), dropLevelsType::drop);

	//Regression: the old value used to be stored as an int (0/1/2) while undo/redo restore it via
	//dropLevelsTypeFromQString (which needs the enum name) -> undo threw missingEnumVal.
	ds->undoStack()->pushCommand(new SetColumnPropertyCommand(col,
		dropLevelsTypeToQString(dropLevelsType::keep),
		SetColumnPropertyCommand::ColumnProperty::DropLevels));

	QCOMPARE(col->dropLevels(), dropLevelsType::keep); //push() redoes the command

	ds->undoStack()->undo();
	QCOMPARE(col->dropLevels(), dropLevelsType::drop);

	ds->undoStack()->redo();
	QCOMPARE(col->dropLevels(), dropLevelsType::keep);
}

void TestAll::testEncoderPrefixPerDataset()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * a = _pkg->dataSet();
	QVERIFY(a);
	QVERIFY(a->columnCount() > 0);

	const std::string colName	= a->column(0)->name();
	const std::string prefixA	= "JASPColumn_" + std::to_string(a->id()) + "_";
	const std::string encodedA	= a->encoder().encode(colName);

	QVERIFY2(encodedA.find(prefixA) == 0,	qPrintable("Encoder prefix must carry the dataset id"));
	QVERIFY2(encodedA.find("-1") == std::string::npos,	qPrintable("Encoder prefix must not be the -1 sentinel"));

	//Reload from the DB (the .jasp restore path): the prefix must still carry the id, not -1.
	DataSet loadMe(nullptr, a->id());
	QCOMPARE(loadMe.id(), a->id());
	const std::string encodedReload = loadMe.encoder().encode(colName);
	QVERIFY2(encodedReload.find(prefixA) == 0,	qPrintable("Reloaded dataset must keep the id-based prefix"));
	QCOMPARE(encodedReload, encodedA);

	//A second dataset with a colliding column name must get a distinct prefix.
	CSVImporter importer;
	DataSet * b = _pkg->workspace()->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	const std::string prefixB	= "JASPColumn_" + std::to_string(b->id()) + "_";
	const std::string encodedB	= b->encoder().encode(b->column(0)->name());
	QVERIFY2(encodedB.find(prefixB) == 0,	qPrintable("Second dataset must get its own id-based prefix"));
	QVERIFY(encodedB != encodedA);

	//Instance-level JSON decode must work against the dataset's own encoder (the static
	//ColumnEncoder::decodeJson is a no-op on the desktop: the global current-encoder indirection is
	//only set inside the engine).
	Json::Value json;
	json["axis"] = encodedA;
	a->encoder().decodeJson(json);
	QCOMPARE(json["axis"].asString(), colName);
}

void TestAll::testFilterRemoveFilter()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	const size_t before = ds->filters().size();

	Filter * f = ds->createFilter("testRemoveMe", true);
	QVERIFY(f);
	QCOMPARE(ds->filters().size(), before + 1);
	QVERIFY(ds->filter("testRemoveMe") == f);

	ds->runFilters(); //must be safe while the filter is present

	//The default filter is not removable and must be a no-op.
	ds->removeFilter(ds->defaultFilter());
	QCOMPARE(ds->filters().size(), before + 1);

	ds->removeFilter(f);
	QCOMPARE(ds->filters().size(), before);
	QVERIFY(ds->filter("testRemoveMe") == nullptr);

	ds->runFilters(); //must still be safe after removal (the dangling-pointer regression would crash here)
}

void TestAll::testSyncerExportModifyReimportChangesDetected()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	// Create an initial CSV
	QString srcPath = tempDir.filePath("data.csv");
	QFile src(srcPath);
	QVERIFY(src.open(QIODevice::WriteOnly));
	src.write("x,y\n1,2\n3,4\n");
	src.close();

	// Import it
	CSVImporter importer;
	importer.loadDataSet(fq(srcPath), _pkg->createDataSet(), [](int){});
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QCOMPARE(ds->rowCount(), 2);
	QCOMPARE(ds->columnCount(), 2);
	QCOMPARE(ds->column(0)->name(), "x");
	QCOMPARE(ds->column(1)->name(), "y");

	// Also export the original and verify
	QString exportPath = tempDir.filePath("export.csv");
	DataExporter exporter(false);
	exporter.saveDataSet(fq(exportPath), [](int){});
	QFile exp(exportPath);
	QVERIFY(exp.open(QIODevice::ReadOnly));
	QVERIFY(QString::fromUtf8(exp.readAll()).contains("x,y"));
	exp.close();

	// Now overwrite the source file with different content
	QFile modified(srcPath);
	QVERIFY(modified.open(QIODevice::WriteOnly));
	modified.write("x,z\n1,7\n3,8\n5,9\n");
	modified.close();

	// Reimport into a new dataset to verify fresh import picks up changes
	DataSet * ds2 = _pkg->createDataSet();
	QVERIFY(ds2);
	_pkg->workspace()->setShownDataSet(ds2);

	CSVImporter importer2;
	importer2.loadDataSet(fq(srcPath), ds2, [](int){});
	QCOMPARE(ds2->rowCount(), 3);
	QCOMPARE(ds2->columnCount(), 2);

	DataExporter exporter2(false);
	QString exportPath2 = tempDir.filePath("export2.csv");
	exporter2.saveDataSet(fq(exportPath2), [](int){});

	QFile exp2(exportPath2);
	QVERIFY(exp2.open(QIODevice::ReadOnly));
	QString content = QString::fromUtf8(exp2.readAll());
	exp2.close();

	QStringList lines = content.split('\n', Qt::SkipEmptyParts);
	QCOMPARE(lines.size(), 4); // header + 3 data rows
	QVERIFY(lines[0].contains("x"));
	QVERIFY(lines[0].contains("z"));
	QVERIFY(!lines[0].contains("y"));
	QVERIFY(lines[1].contains("7"));
	QVERIFY(lines[3].contains("9"));
}

void TestAll::testFilterRevisionInvalidatedRoundTrip()
{
	//Regression test: filterLoad used to assign `revision` twice (overwriting it with the
	//`invalidated` column) and never loaded `invalidated`. Save a filter and check every
	//field round-trips, especially revision vs invalidated.
	QVERIFY(_newPkgWithDataSet());
	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	DatabaseInterface & dbi = ds->db();
	const int dataSetId = ds->id();
	QVERIFY(dataSetId > 0);

	const std::string originalRFilter		= "filterResult <- x > 1";
	const std::string originalGenerated		= "generated <- TRUE";
	const std::string originalConstructor	= "{\"formulas\":[]}";
	const std::string originalConstructorR	= "constrR <- 1 + 1";
	const std::string originalName			= "roundtripFilter";

	const int filterId = dbi.filterInsert(dataSetId, originalRFilter, originalGenerated, originalConstructor, originalConstructorR, originalName);
	QVERIFY2(filterId > 0, "filterInsert should return a valid filter id");

	//Update with a marked-invalidated flag; make sure it round-trips.
	const std::string updatedRFilter		= "filterResult <- x > 2";
	const std::string updatedGenerated		= "generated <- FALSE";
	const std::string updatedConstructor	= "{\"formulas\":[1]}";
	const std::string updatedConstructorR	= "constrR <- 2 + 2";
	const std::string updatedName			= "roundtripFilterRenamed";
	const bool		updatedInvalidated		= true;

	dbi.filterUpdate(filterId, updatedRFilter, updatedGenerated, updatedConstructor, updatedConstructorR, updatedName, updatedInvalidated);

	std::string rFilter, generatedFilter, constructorJson, constructorR, name;
	int		revision		= -1;
	bool	invalidated		= false;

	dbi.filterLoad(filterId, rFilter, generatedFilter, constructorJson, constructorR, revision, name, invalidated);

	QCOMPARE(QString::fromStdString(rFilter),			QString::fromStdString(updatedRFilter));
	QCOMPARE(QString::fromStdString(generatedFilter),	QString::fromStdString(updatedGenerated));
	QCOMPARE(QString::fromStdString(constructorJson),	QString::fromStdString(updatedConstructor));
	QCOMPARE(QString::fromStdString(constructorR),		QString::fromStdString(updatedConstructorR));
	QCOMPARE(QString::fromStdString(name),				QString::fromStdString(updatedName));
	QCOMPARE(invalidated, updatedInvalidated);
	QVERIFY2(revision >= 0, "revision must stay an integer revision, not the invalidated flag");

	dbi.filterDelete(filterId);
}

void TestAll::testSyncKeepMissingColumns()
{
	_pkg = new DataSetPackage(this);

	//Importer::syncDataSet reads the preferences singleton; nothing else in the tests creates one.
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);
	QVERIFY(PreferencesModel::prefs());

	//syncDataSet asks permission through checkDoSync; with no MainWindow around nothing answers it and
	//the signal would return a default-constructed false, aborting every sync below.
	connect(DataSetPackage::pkg(), &DataSetPackage::checkDoSync, this, &TestAll::_checkDoSyncFake, Qt::DirectConnection);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	auto writeCsv = [&](const QString & name, const QByteArray & content)
	{
		QString path = tempDir.filePath(name);
		QFile	file(path);
		if(!file.open(QIODevice::WriteOnly))
			return QString();
		file.write(content);
		file.close();
		return path;
	};

	const QString	abc	= writeCsv("abc.csv",	"a,b,c\n1,2,3\n"),
					ab	= writeCsv("ab.csv",	"a,b\n4,5\n"),
					pq	= writeCsv("pq.csv",	"p,q\n6,7\n");

	QVERIFY(!abc.isEmpty() && !ab.isEmpty() && !pq.isEmpty());

	auto freshDataSetFrom = [&](const QString & csv)
	{
		DataSet * ds = _pkg->createDataSet();
		_pkg->workspace()->setShownDataSet(ds);

		CSVImporter importer;
		importer.loadDataSet(fq(csv), ds, [](int){});
		return ds;
	};

	//Without the preference a column that is gone from the new data file is removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(false);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), ds, [](int){});

		QCOMPARE(ds->columnCount(), 2);
		QVERIFY(!ds->column("c"));
	}

	//With the preference that same column is kept instead of removed.
	{
		PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(true);

		DataSet * ds = freshDataSetFrom(abc);
		QCOMPARE(ds->columnCount(), 3);

		CSVImporter syncer;
		syncer.syncDataSet(fq(ab), ds, [](int){});

		QCOMPARE(ds->columnCount(), 3);
		QVERIFY(ds->column("c"));

		//Syncing on against a file that shares no column at all keeps every one of them: p and q are added
		//next to a, b and c rather than taking their place, so the data holds the union of both files. That
		//union keeps growing for as long as one session goes on synchronizing, which is why each data file
		//normally gets a JASP process of its own.
		CSVImporter syncer2;
		syncer2.syncDataSet(fq(pq), ds, [](int){});

		QCOMPARE(ds->columnCount(), 5);
		QVERIFY(ds->column("a"));
		QVERIFY(ds->column("b"));
		QVERIFY(ds->column("c"));
		QVERIFY(ds->column("p"));
		QVERIFY(ds->column("q"));
	}

	PreferencesModel::prefs()->setKeepMissingColsWhenSyncing(false);

	//The singleton registers globally (PreferencesModelBase::_singleton) and is parented to this
	//test, so it would survive this test and then make MainWindow's own PreferencesModel assert.
	delete PreferencesModel::prefs();
}

void TestAll::testFileSyncerFullAsyncFlow()
{
	//Test the complete FileEvent + AsyncLoader sync flow
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	//Create a test CSV file
	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("async_sync.csv");
	{
		QFile f(testFilePath);
		QVERIFY(f.open(QIODevice::WriteOnly));
		f.write("a,b,c\n1,2,3\n");
		f.close();
	}

	//Set the data file and timestamp so the sync has something to compare against
	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	//Create an AsyncLoader on heap so it lives during async operations
	AsyncLoader * loader = new AsyncLoader(this);
	QSignalSpy syncCompletedSpy(loader, &AsyncLoader::syncCompleted);

	//Connect DataSet::syncRequired to AsyncLoader::onSyncRequired (like MainWindow does)
	connect(ds, &DataSet::syncRequired, loader, &AsyncLoader::onSyncRequired, Qt::QueuedConnection);

	//Trigger sync
	DataSetSyncer & syncer = ds->syncer();
	syncer.startFileSyncing(testFilePath);
	QVERIFY(syncer.isFileSyncing());

	//Update timestamp so fileChanged will pass the timestamp check when syncer.syncNow() is called
	ds->setDataFileAndTimeStamp(testFilePath.toStdString(), 0);

	//Trigger syncNow which will call fileChanged -> doSync -> emit syncRequired
	syncer.syncNow();

	//Wait for syncCompleted to be emitted by AsyncLoader (via syncRequired -> onSyncRequired -> loadPackage -> syncCompleted)
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy.count(), 1, 3000);

	//Verify sync completed successfully
	QVERIFY(syncer.isFileSyncing()); //Still syncing because we haven't called stop
	QVERIFY(ds->dataFileSynch());

	syncer.stopFileSyncing();

	//Cleanup
	delete loader;
}

void TestAll::testSyncerDatabaseSyncFromSQLite()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	DataSetSyncer & syncer = ds->syncer();

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	QString testDbPath = tempDir.filePath("test_sync.db");
	QVERIFY(!testDbPath.isEmpty());

	sqlite3 * db = nullptr;
	int ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, QString("Failed to open/create database: %1").arg(testDbPath).toStdString().c_str());

	std::string createTableSql = "CREATE TABLE test_data ("
	                             "id INTEGER PRIMARY KEY, "
	                             "name TEXT, "
	                             "value REAL, "
	                             "category TEXT"
	                             ");";
	ret = sqlite3_exec(db, createTableSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to create table");

	std::string insertDataSql = "INSERT INTO test_data (id, name, value, category) VALUES "
	                            "(1, 'first', 10.5, 'A'), "
	                            "(2, 'second', 20.3, 'B'), "
	                            "(3, 'third', 30.7, 'A');";
	ret = sqlite3_exec(db, insertDataSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to insert initial data");

	sqlite3_close(db);
	db = nullptr;

	Json::Value dbJson;
	dbJson["dbType"] = "QSQLITE";
	dbJson["database"] = testDbPath.toStdString();
	dbJson["query"] = "SELECT id, name, value, category FROM test_data";
	dbJson["interval"] = 1;

	syncer.startDatabaseSyncing(dbJson, false);
	QVERIFY(syncer.isDatabaseSyncing());
	QVERIFY(ds->isDatabase());

	AsyncLoader * loader = new AsyncLoader(this);
	QSignalSpy syncCompletedSpy(loader, &AsyncLoader::syncCompleted);
	connect(ds, &DataSet::syncRequired, loader, &AsyncLoader::onSyncRequired, Qt::QueuedConnection);

	// Fake the checkDoSync signal to return true (no MainWindow in tests)
	connect(DataSetPackage::pkg(), &DataSetPackage::checkDoSync, this, &TestAll::_checkDoSyncFake, Qt::DirectConnection);

	QSignalSpy syncRequiredSpy(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy.count(), 1, 3000);

	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy.count(), 1, 3000);

	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy.count(), 1, 3000);

	QVERIFY(ds->columnCount() >= 4);
	QVERIFY(ds->rowCount() == 3);

	Column * idCol = ds->column("id");
	Column * nameCol = ds->column("name");
	Column * valueCol = ds->column("value");
	Column * categoryCol = ds->column("category");

	QVERIFY(idCol);
	QVERIFY(nameCol);
	QVERIFY(valueCol);
	QVERIFY(categoryCol);

	// Basic verification that data was loaded
	QVERIFY((*idCol)[0] == "1");
	QVERIFY((*valueCol)[0] == "10.5");

	db = nullptr;
	ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, "Failed to reopen database for modification");

	std::string updateDataSql = "UPDATE test_data SET value = value + 5 WHERE id IN (1, 2, 3);";
	ret = sqlite3_exec(db, updateDataSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to update values");

	sqlite3_close(db);
	db = nullptr;

	QSignalSpy syncCompletedSpy2(loader, &AsyncLoader::syncCompleted);
	QSignalSpy syncRequiredSpy2(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy2(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy2(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy2.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy2.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy2.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy2.count(), 1, 3000);

	QVERIFY((*valueCol)[0] == "15.5");
	QVERIFY((*valueCol)[1] == "25.3");
	QVERIFY((*valueCol)[2] == "35.7");

	// Note: Database sync doesn't currently support dynamic schema changes
	// Testing value updates only - schema changes would require re-sync from database

	// Final value update test
	db = nullptr;
	ret = sqlite3_open(testDbPath.toStdString().c_str(), &db);
	QVERIFY2(ret == SQLITE_OK, "Failed to reopen database for final value update");

	std::string finalUpdateSql = "UPDATE test_data SET value = 99.9 WHERE id = 2;";
	ret = sqlite3_exec(db, finalUpdateSql.c_str(), nullptr, nullptr, nullptr);
	QVERIFY2(ret == SQLITE_OK, "Failed to final update");

	sqlite3_close(db);
	db = nullptr;

	QSignalSpy syncCompletedSpy4(loader, &AsyncLoader::syncCompleted);
	QSignalSpy syncRequiredSpy4(&syncer, &DataSetSyncer::syncRequired);
	QSignalSpy startedSpy4(&syncer, &DataSetSyncer::syncingStarted);
	QSignalSpy finishedSpy4(&syncer, &DataSetSyncer::syncingFinished);

	syncer.syncNow();
	QTRY_COMPARE_WITH_TIMEOUT(startedSpy4.count(), 1, 3000);
	syncer.setSyncingResult(true);
	QTRY_COMPARE_WITH_TIMEOUT(finishedSpy4.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncRequiredSpy4.count(), 1, 3000);
	QTRY_COMPARE_WITH_TIMEOUT(syncCompletedSpy4.count(), 1, 3000);

	QVERIFY((*valueCol)[1] == "99.9");

	syncer.stopDatabaseSyncing();

	QVERIFY(!syncer.isDatabaseSyncing());
	QVERIFY(!ds->isDatabase());

	delete loader;
}

void TestAll::testCloseWorkspaceAndDataSets()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	QVERIFY(_pkg->dataSet());

	//Give the workspace several distinct (non-empty) datasets so deleteShownDataSet has to
	//re-pick another shown dataset after each removal.
	CSVImporter importer;
	const std::string csvPath = fq(_testLibrary().absoluteFilePath("csv/debug.csv"));

	DataSet * second = ws->createDataSet();
	QVERIFY(second);
	importer.loadDataSet(csvPath, second, [](int){});
	QVERIFY(second->columnCount() > 0);

	DataSet * third = ws->createDataSet();
	QVERIFY(third);
	importer.loadDataSet(csvPath, third, [](int){});
	QVERIFY(third->columnCount() > 0);

	QCOMPARE(ws->dataSets().size(), size_t(3));

	//Deleting the shown dataset must not crash and must leave the other datasets alive.
	DataSet * shown = ws->shownDataSet();
	QVERIFY(shown);
	ws->deleteShownDataSet();
	QCOMPARE(ws->dataSets().size(), size_t(2));
	QVERIFY(ws->shownDataSet());
	QVERIFY(ws->shownDataSet() != shown);

	//Delete the remaining ones, one at a time, until the workspace is empty. The old crash
	//(ColumnModel::shownDataSetChangedHandler disconnecting a stale dataset) used to segfault here.
	while (ws->shownDataSet())
		ws->deleteShownDataSet();

	QCOMPARE(ws->dataSets().size(), size_t(0));
	QVERIFY(!ws->shownDataSet());

	//Re-populate, then tear the whole workspace down (deleteWorkspace/reset) — must not crash either.
	DataSet * again = ws->createDataSet();
	QVERIFY(again);
	importer.loadDataSet(csvPath, again, [](int){});
	QVERIFY(ws->dataSets().size() == size_t(1));

	_pkg->deleteWorkspace();
	QVERIFY(!_pkg->workspace());

	//A fresh workspace (as DataSetPackage::createDataSet does on first use) still works afterwards.
	DataSet * fresh = _pkg->createDataSet();
	QVERIFY(fresh);
	QVERIFY(_pkg->workspace());

	//_pkg->deleteWorkspace() above destroyed the workspace `ws` pointed at; createDataSet() made a new
	//one, so re-obtain it before touching it.
	ws = _pkg->workspace();
	QVERIFY(ws);

	//Regression: after closing the workspace, opening (i.e. adding) datasets again must keep working
	//instead of targeting a stale/removed workspace. Load data into the fresh dataset and add a couple
	//more, then make sure the workspace holds them all and can still close them without crashing.
	importer.loadDataSet(csvPath, fresh, [](int){});
	QVERIFY(fresh->columnCount() > 0);

	DataSet * secondAfterClose = ws->createDataSet();
	QVERIFY(secondAfterClose);
	importer.loadDataSet(csvPath, secondAfterClose, [](int){});
	QVERIFY(secondAfterClose->columnCount() > 0);

	DataSet * thirdAfterClose = ws->createDataSet();
	QVERIFY(thirdAfterClose);
	importer.loadDataSet(csvPath, thirdAfterClose, [](int){});
	QVERIFY(thirdAfterClose->columnCount() > 0);

	QCOMPARE(ws->dataSets().size(), size_t(3));
	QVERIFY(ws->shownDataSet());

	while (ws->shownDataSet())
		ws->deleteShownDataSet();

	QCOMPARE(ws->dataSets().size(), size_t(0));
	QVERIFY(!ws->shownDataSet());
}

bool TestAll::_checkDoSyncFake()
{
	return true;
}

bool TestAll::_writeTextFile(const QString & path, const QByteArray & contents)
{
	QFile file(path);
	if(!file.open(QIODevice::WriteOnly | QIODevice::Text))
		return false;
	return file.write(contents) == contents.size();
}

//Constructs a full MainWindow the way the command line sees it and detaches exitSignal from
//QApplication::exit (which the constructor wires up and which would quit the test event loop),
//returning a spy on the signal instead. The spy is parented to nothing; delete it after use.
QSignalSpy * TestAll::_newMainWindowWithExitSpy(MainWindow *& mw)
{
	//A previous MainWindow teardown took the whole session directory (and its database) with it,
	//while MainWindow wants to create its own DatabaseInterface in there again.
	TempFiles::init(ProcessInfo::currentPID());

	//Keep the QML engine and the R engines out of the test process: the data/sync chain under test
	//does not need them, and both add many threads (sqlite contention, JS garbage collection) that
	//make the test flaky. See headlessTestMode() in mainwindow.cpp.
	qputenv("JASP_TEST_HEADLESS", "1");

	//Other tests may have left the global PreferencesModel singleton behind; the MainWindow wants
	//to construct its own, and a second one asserts.
	delete PreferencesModel::prefs();

	QCoreApplication::setApplicationName("JASPTest"); //so checkForUpdates() stays out of the way

	try
	{
		mw = new MainWindow(nullptr);
	}
	catch(const std::exception & e)
	{
		std::cerr << "MainWindow construction threw: " << e.what() << std::endl;
		throw;
	}

	//The MainWindow listens for the interactive CSV preview; with no delimiter preset the importers
	//would then block this (GUI) thread on an answer that can never come (regression guard: the
	//per-test init() resets it to '\0').
	DesktopCommunicator::singleton()->setKnownCsvDelimiter(',');

	disconnect(mw, &MainWindow::exitSignal, mw, nullptr); //keep every other listener, lose only qApp

	return new QSignalSpy(mw, &MainWindow::exitSignal);
}

void TestAll::testCliSyncExportChainFromFreshWorkspace()
{
	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	//A genuine JASP file from the test library, so the chain runs against a real saved document:
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	//The sync data holds exactly the columns of that file (in its order), so synchronizing
	//replaces them without adding or deleting any: this pins the plain open -> sync -> exit chain.
	const QByteArray syncCsvData =
		"V1,contNormal,contGamma,contBinom,contExpon,contWide,contNarrow,contOutlier,contcor1,contcor2,"
		"facGender,facExperim,facFive,facFifty,facOutlier,debString,debMiss1,debMiss30,debMiss80,debMiss99,"
		"debBinMiss20,debNaN,debNaN10,debInf,debCollin1,debCollin2,debCollin3,debEqual1,debEqual2,debSame,unicode\n"
		"1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,sixteen,17,18,19,20,21,22,23,24,25,26,27,28,29,thirty\n"
		"31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,fortysix,47,48,49,50,51,52,53,54,55,56,57,58,59,sixty\n";

	const QString	syncCsv		= tempDir.filePath("syncdata.csv");

	QVERIFY(_writeTextFile(syncCsv, syncCsvData));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//The results page finished loading before the command line triggers the file open, and nothing
	//has been loaded into the workspace yet. Note that _open's dataset check still passes, because
	//JASP startup itself creates an empty dataset; the chain therefore binds the *loaded* dataset
	//only after the open has finalized, which is exactly what chain(..., resetDataSet=true) does.
	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, syncCsv, "", false);

	//The open -> synchronize chain should finish by exiting JASP with success:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QCOMPARE(exitSpy->first().first().toInt(), 0);

	//And the workspace should hold the synchronized data:
	DataSet * synced = DataSetPackage::pkg()->dataSet();
	QVERIFY(synced);
	QVERIFY(synced->column("contNormal"));

	try
	{
		delete exitSpy;
		delete mw; //MainWindow::singleton() and other singletons must not survive into the next test
	}
	catch(const std::exception & e)
	{
		std::cerr << "MainWindow teardown threw: " << e.what() << std::endl;
		throw;
	}

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}

void TestAll::testCliSyncExportChainFailsOnBadDataFile()
{
	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp"),
					bogusData	= tempDir.filePath("bogus.xlsx");

	QVERIFY(QFileInfo::exists(jaspPath));
	QVERIFY(_writeTextFile(bogusData, "this is not a spreadsheet\n"));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, bogusData, "", false);

	//A failed synchronization must exit JASP with a non-zero code instead of continuing as if the
	//synchronization had succeeded:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(exitSpy->first().first().toInt() != 0);

	delete exitSpy;
	delete mw; //MainWindow::singleton() and other singletons must not survive into the next test

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}


QTEST_MAIN(TestAll)
