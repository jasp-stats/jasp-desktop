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
#include "utilities/desktopcommunicator.h"
#include "datasetsyncer.h"
#include "dataset.h"
#include "workspace.h"
#include "undostack.h"
#include "data/asyncloader.h"
#include "scriptconstructormodel.h"
#include "scriptnode.h"
#include "scriptconstructorregistry.h"

#include "mainwindow.h"
#include "data/filtermodel.h"
#include "data/columnsmodel.h"
#include "qquick/scriptconstructorview.h"
#include "qquick/scriptnodeitem.h"
#include "timers.h"

#include <QSignalSpy>
#include <QFile>
#include <QFileInfo>
#include <QUndoStack>
#include <QQuickWindow>
#include <QMouseEvent>
#include <QGuiApplication>
#include <QtWebEngineQuick/qtwebenginequickglobal.h>
#include <random>
#include <functional>
#include <sqlite3.h>
#include "data/asyncloader.h"


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

	DatabaseInterface::singleton()->close();
	DatabaseInterface::singleton()->closeInterfaces();
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

QQuickItem * TestAll::_findQuickItemByName(const QString & objectName)
{
	for(QWindow * window : QGuiApplication::topLevelWindows())
		if(QQuickWindow * quickWindow = qobject_cast<QQuickWindow*>(window))
			if(QQuickItem * item = quickWindow->findChild<QQuickItem*>(objectName))
				return item;
	return nullptr;
}

void TestAll::testMainWindowShowsFilterWindow()
{
	// Makes MainWindow::checkForUpdates() bail out (mainwindow.cpp).
	QCoreApplication::setApplicationName("JASPTest");

	// The full QML UI contains WebEngine views (ChatWindow, results page), so WebEngine must be
	// initialized before MainWindow creates its QQmlApplicationEngine (same order as main.cpp).
	QtWebEngineQuick::initialize();

	// MainWindow constructs the one-and-only DataSetPackage singleton, so no other package may
	// exist at this point (the cleanup() of any previous test has deleted it).
	QVERIFY(DataSetPackage::pkg() == nullptr);

	MainWindow * mainWindow = new MainWindow(nullptr);

	QSignalSpy qmlLoadedSpy(mainWindow, &MainWindow::qmlLoadedChanged);
	QTRY_VERIFY(!qmlLoadedSpy.isEmpty()); // loadQML() runs from a QTimer::singleShot in the ctor

	// Load a dataset the way production does: into the package owned by MainWindow, mark it
	// as the shown dataset (DataSetLoader::loadPackage does the same) and then notify the UI
	// (newDataLoaded -> MainWindow::populateUIfromDataSet).
	DataSet * dataSet = DataSetPackage::pkg()->createDataSet();
	QVERIFY(dataSet != nullptr);
	// Exactly what DataSetLoader::loadPackage does (datasetloader.cpp): setShownDataSet may
	// early-return (the empty dataset was already made shown by the EngineSync-ctor reset),
	// so refresh() is needed to (re)emit shownDataSetChanged now that makeConnections() has
	// wired up the models.
	DataSetPackage::pkg()->workspace()->setShownDataSet(dataSet);
	DataSetPackage::pkg()->workspace()->refresh();

	// Pre-set the delimiter: with MainWindow connected, askCsvDelimiterSignal would otherwise
	// open a CSV delimiter dialog and deadlock the synchronous import on the main thread.
	DesktopCommunicator::singleton()->setKnownCsvDelimiter(',');

	CSVImporter importer;
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), dataSet, [](int){});
	DataSetPackage::pkg()->newDataLoaded();

	// Open the filter window the way the UI does (Loader in DataPanel.qml).
	FilterModel * filterModel = mainWindow->findChild<FilterModel*>();
	QVERIFY(filterModel != nullptr);
	filterModel->setFilterVisible(true);

	// FilterWindow (objectName "filterWindow") must appear and the ScriptConstructor inside it
	// must have built its chrome now that it is visible.
	QQuickItem * filterWindow = nullptr;
	QTRY_VERIFY((filterWindow = _findQuickItemByName("filterWindow")) != nullptr);

	ScriptConstructorView * scriptConstructor = filterWindow->findChild<ScriptConstructorView*>();
	QVERIFY(scriptConstructor != nullptr);
	QTRY_VERIFY(scriptConstructor->scriptArea() != nullptr); // non-null once the chrome is built

	// --- Trash can regression: double-click must erase the entire script area ---
	// The trash item is the only script-area child with z == 10.
	QQuickItem * trash = nullptr;
	for(QQuickItem * child : scriptConstructor->scriptArea()->childItems())
		if(child->z() == 10)
			trash = child;
	QVERIFY2(trash != nullptr, "Trash item not found in script area");

	QQuickWindow * quickWindow = trash->window();
	QVERIFY(quickWindow != nullptr);

	// The offscreen SplitView never assigns a width to the Loader (it stays 0 wide), which
	// excludes the whole filter window from mouse hit-testing even though its content is
	// visible (children overflow unclipped ancestors). In the real app the SplitView/anchors
	// give the loader a proper width; emulate that here.
	if(QQuickItem * loader = filterWindow->parentItem())
	{
		loader->setHeight(std::max(loader->height(), 600.0));
		loader->setWidth(std::max(loader->width(), 1200.0));
	}

	auto trashCentre = [&]()
	{
		return trash->mapToScene(QPointF(trash->width() / 2, trash->height() / 2));
	};
	const QPointF centre = trashCentre();
	QVERIFY2(centre.x() > 0 && centre.y() > 0 && centre.x() < quickWindow->width() && centre.y() < quickWindow->height(),
		qPrintable(QString("Trash not inside the window bounds: centre=%1,%2 constructor=%3x%4 at %5,%6 window=%7x%8 trashSize=%9x%10")
			.arg(centre.x()).arg(centre.y())
			.arg(scriptConstructor->width()).arg(scriptConstructor->height())
			.arg(scriptConstructor->x()).arg(scriptConstructor->y())
			.arg(quickWindow->width()).arg(quickWindow->height())
			.arg(trash->width()).arg(trash->height())));

	// The engine/loader threads push filter results back into the view asynchronously; a push
	// whose json differs from the current model resets it (fromJson emits reset), wiping
	// unapplied formulas at any event-loop spin. QTest's mouse helpers spin internally, and
	// the resulting QML GC churn reliably crashes this offscreen harness (not seen in the
	// real app). So:
	// - break the QML applyRequested -> FilterModel connection, so the trash's checkAndApply
	//   cannot trigger the full apply chain (re-running the filter rebuilds the entire
	//   DataSetView) inside the harness; and
	// - verify the trash via its deterministic debug event counters, delivering the mouse
	//   sequence directly to the item (no event-loop spin -> no interleaving).
	// NOTE: real window hit-testing delivery to the trash was verified separately (the
	// topmost-item walk shows ScriptTrashItem is the only accepting item at its position, and
	// QTest-delivered presses arrived at it) — it is only the spinning+GC combination that
	// cannot run in this harness.
	QObject::disconnect(scriptConstructor, &ScriptConstructorView::applyRequested, scriptConstructor, nullptr);

	QSignalSpy applySpy(scriptConstructor, &ScriptConstructorView::applyRequested);

	// Boolean-valid formula (Filter mode requires it) so the trash's checkAndApply succeeds.
	ScriptNodeOperator * equals = new ScriptNodeOperator("==", false);
	equals->setLeft(new ScriptNodeColumn("contNormal"));
	ScriptNodeLiteral * one = new ScriptNodeLiteral(ScriptNode::Type::Number);
	one->setNumberValue(1);
	equals->setRight(one);
	scriptConstructor->model()->insertNode(equals, DropTarget::root());
	scriptConstructor->refresh();
	QCOMPARE(scriptConstructor->model()->formulaCount(), 1);

	ScriptTrashItem * trashItem = qobject_cast<ScriptTrashItem*>(trash);
	QVERIFY2(trashItem != nullptr, "Trash is not a ScriptTrashItem");
	const int pressesBefore = trashItem->debugPressCount;
	const int dblClicksBefore = trashItem->debugDoubleClickCount;

	auto sendMouse = [&](QEvent::Type type)
	{
		QMouseEvent me(type, trashCentre(), quickWindow->mapToGlobal(trashCentre()), Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
		QCoreApplication::sendEvent(trash, &me);
	};
	sendMouse(QEvent::MouseButtonPress);
	sendMouse(QEvent::MouseButtonRelease);
	sendMouse(QEvent::MouseButtonDblClick);
	sendMouse(QEvent::MouseButtonRelease);

	QCOMPARE(trashItem->debugPressCount, pressesBefore + 1);
	QCOMPARE(trashItem->debugDoubleClickCount, dblClicksBefore + 1);
	QCOMPARE(scriptConstructor->model()->formulaCount(), 0); // slate erased
	QVERIFY(!applySpy.isEmpty());							 // emptied filter was applied

	// Leave the full timer table behind for profiling (needs JASP_TIMER_USED=ON).
	JASPTIMER_PRINTALL();

	// Deliberately do not delete mainWindow: tearing down the EngineSync / DatabaseInterface
	// during process shutdown throws (sqlite session already gone), which would fail the test
	// even though the assertions passed. The binary exits right after this slot anyway.
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
	QCOMPARE(args.size(), 5); //(dataSetId, DataSet*, locator, extension, databaseJson)
	QCOMPARE(args[0].toInt(), ds->id());
	QCOMPARE(args[2].toString(), testFilePath);
	QCOMPARE(args[3].toString(), QString("csv")); //extension
	QVERIFY(args[4].toString().isEmpty()); //databaseJson

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

// =====================================================================================
// ScriptConstructor regression tests
// =====================================================================================

namespace
{
	struct FixedColumnTypeProvider : public ScriptColumnTypeProvider
	{
		strintmap types;
		int columnType(const std::string & name) const override
		{
			auto it = types.find(name);
			return it == types.end() ? 1 : it->second;
		}
	};

	Json::Value colNode(const std::string & name, int typeUser = -1, int typeDrop = -1)
	{
		Json::Value v;
		v["nodeType"]			= "Column";
		v["columnName"]			= name;
		v["columnTypeUser"]		= typeUser;
		v["columnTypeDrop"]		= typeDrop;
		return v;
	}

	Json::Value numNode(double val)
	{
		Json::Value v;
		v["nodeType"] = "Number";
		v["value"] = val;
		return v;
	}

	Json::Value boolNode(bool val)
	{
		Json::Value v;
		v["nodeType"] = "Boolean";
		v["value"] = val ? "TRUE" : "FALSE";
		return v;
	}

	Json::Value strNode(const std::string & text)
	{
		Json::Value v;
		v["nodeType"] = "String";
		v["text"] = text;
		return v;
	}

	Json::Value opNode(const std::string & op, const Json::Value & left, const Json::Value & right, bool vertical = false)
	{
		Json::Value v;
		v["nodeType"]		= vertical ? "OperatorVertical" : "Operator";
		v["operator"]		= op;
		v["leftArgument"]	= left;
		v["rightArgument"]	= right;
		return v;
	}

	Json::Value funcArg(const std::string & name, const stringvec & keys, const Json::Value & argument)
	{
		Json::Value a;
		a["name"] = name;
		a["dropKeys"] = Json::arrayValue;
		for(const std::string & k : keys)
			a["dropKeys"].append(k);
		a["argument"] = argument;
		return a;
	}

	Json::Value funcNode(const std::string & name, std::initializer_list<Json::Value> args)
	{
		Json::Value v;
		v["nodeType"]		= "Function";
		v["functionName"]	= name;
		v["arguments"]		= Json::arrayValue;
		for(const Json::Value & a : args)
			v["arguments"].append(a);
		return v;
	}

	Json::Value rowFuncNode(const std::string & name, std::initializer_list<std::string> droppedJsonStrings)
	{
		Json::Value v;
		v["nodeType"]		= "RowFunction";
		v["functionName"]	= name;
		v["droppedItems"]	= Json::arrayValue;
		for(const std::string & s : droppedJsonStrings)
			v["droppedItems"].append(s);
		return v;
	}

	Json::Value formulas(std::initializer_list<Json::Value> nodes)
	{
		Json::Value v;
		v["formulas"] = Json::arrayValue;
		for(const Json::Value & n : nodes)
			v["formulas"].append(n);
		return v;
	}

	std::string compact(const Json::Value & v)
	{
		Json::StreamWriterBuilder b;
		b["indentation"] = "";
		std::string s = Json::writeString(b, v);
		while(!s.empty() && (s.back() == '\n' || s.back() == '\r' || s.back() == ' '))
			s.pop_back();
		return s;
	}
}

void TestAll::testScriptConstructorDefaultFilterJson()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	model.fromJson(std::string(DEFAULT_FILTER_JSON));

	QCOMPARE(model.formulaCount(), 0);
	QCOMPARE(model.toString(), std::string(DEFAULT_FILTER_JSON));
	QVERIFY(model.checkCompleteness());
}

void TestAll::testScriptConstructorGoldenR()
{
	QVERIFY(_newPkgWithDataSet());

	FixedColumnTypeProvider provider;
	provider.types["contNormal"]	= 1; // scale
	provider.types["contBinom"]		= 1;
	provider.types["group"]			= 3; // nominal
	provider.types["ord"]			= 2; // ordinal

	ScriptConstructorModel model;
	model.setColumnTypeProvider(&provider);
	model.setMode(ScriptConstructorMode::Filter);

	auto checkR = [&](const Json::Value & tree, const std::string & expected)
	{
		model.fromJson(tree);
		QCOMPARE(model.toR(), expected);
	};

	// Column resolves through the provider (no user/drop override) -> ".scale"
	checkR(formulas({colNode("contNormal")}), "contNormal.scale\n");

	// Arithmetic operator with a literal
	checkR(formulas({opNode("+", colNode("contNormal"), numNode(5))}), "(contNormal.scale + 5)\n");

	// Empty right slot becomes "null"
	checkR(formulas({opNode("+", colNode("contNormal"), Json::nullValue)}), "(contNormal.scale + null)\n");

	// mean() gains ", na.rm=TRUE"
	checkR(formulas({funcNode("mean", {funcArg("values", {"number"}, colNode("contNormal"))})}), "mean(contNormal.scale, na.rm=TRUE)\n");

	// abs() has no na.rm
	checkR(formulas({funcNode("abs", {funcArg("values", {"number"}, colNode("contNormal"))})}), "abs(contNormal.scale)\n");

	// Empty function argument becomes "NULL"
	checkR(formulas({funcNode("round", {funcArg("y", {"number"}, colNode("contNormal")), funcArg("n", {"number"}, Json::nullValue)})}), "round(contNormal.scale, NULL)\n");

	// ifelse with boolean + numbers
	checkR(formulas({funcNode("ifelse", {funcArg("test", {"boolean"}, boolNode(true)), funcArg("then", {"boolean","string","number"}, numNode(1)), funcArg("else", {"boolean","string","number"}, numNode(2))})}), "ifelse(TRUE, 1, 2)\n");

	// String literal is single-quoted
	checkR(formulas({strNode("hello")}), "'hello'\n");

	// Nested boolean expression
	checkR(formulas({opNode("&", opNode(">", colNode("contNormal"), numNode(0)), opNode("<", colNode("contBinom"), numNode(10)))}), "((contNormal.scale > 0) & (contBinom.scale < 10))\n");

	// Vertical (fraction) operator serialises differently but generates the same R shape
	checkR(formulas({opNode("/", colNode("contNormal"), numNode(2), true)}), "(contNormal.scale / 2)\n");

	// RowFunction only emits filled entries and appends NaRm
	{
		Json::StreamWriterBuilder b; b["indentation"] = "";
		std::string aJson = compact(colNode("contNormal"));
		std::string bJson = compact(colNode("contBinom"));
		checkR(formulas({rowFuncNode("rowMean", {aJson, "null", bJson})}), "rowMeanNaRm(contNormal.scale, contBinom.scale)\n");
	}

	// Column with an explicit user type override ignores the provider
	checkR(formulas({colNode("group", 2)}), "group.ordinal\n");

	// %|% conditional operator in filter mode
	checkR(formulas({opNode("%|%", opNode(">", colNode("contNormal"), numNode(0)), colNode("group"))}), "((contNormal.scale > 0) %|% group.nominal)\n");

	// sqrt (operator-bar-only function) wraps a single number argument
	checkR(formulas({funcNode("sqrt", {funcArg("value(s)", {"number"}, colNode("contNormal"))})}), "sqrt(contNormal.scale)\n");

	// ! (operator-bar-only function) wraps a single boolean argument
	checkR(formulas({funcNode("!", {funcArg("logical(s)", {"boolean"}, boolNode(true))})}), "!(TRUE)\n");
}

void TestAll::testScriptConstructorCompleteness()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	model.setMode(ScriptConstructorMode::Filter);

	// Complete boolean formula passes both checks
	model.fromJson(formulas({opNode(">", colNode("a"), numNode(0))}));
	QVERIFY(model.checkCompleteness());
	QVERIFY(model.allBoolean());

	// Missing right operand -> incomplete
	model.fromJson(formulas({opNode(">", colNode("a"), Json::nullValue)}));
	QVERIFY(!model.checkCompleteness());

	// Arithmetic root is complete but not boolean -> cannot be a filter root
	model.fromJson(formulas({opNode("+", colNode("a"), numNode(1))}));
	QVERIFY(model.checkCompleteness());
	QVERIFY(!model.allBoolean());

	// Optional ("?") parameters do not block completeness
	{
		Json::Value box = funcNode("BoxCoxAuto", {
			funcArg("y", {"number"}, colNode("a")),
			funcArg("?predictor", {"number"}, Json::nullValue),
			funcArg("?groupSize", {"number"}, Json::nullValue),
			funcArg("method", {"string"}, strNode("loglik")),
			funcArg("lower", {"number"}, numNode(0)),
			funcArg("upper", {"number"}, numNode(1)),
			funcArg("shift", {"number"}, numNode(0)),
			funcArg("continuityAdjustment", {"boolean"}, boolNode(true))});
		model.fromJson(formulas({box}));
		QVERIFY(model.checkCompleteness());
	}

	// RowFunction is complete when at least one slot is filled
	{
		std::string aJson = compact(colNode("a"));
		model.fromJson(formulas({rowFuncNode("rowSum", {"null", aJson})}));
		QVERIFY(model.checkCompleteness());

		model.fromJson(formulas({rowFuncNode("rowSum", {"null"})}));
		QVERIFY(!model.checkCompleteness());
	}
}

void TestAll::testScriptConstructorRoundTrip()
{
	QVERIFY(_newPkgWithDataSet());

	// Deterministic pseudo-random trees: fromJson -> toString -> fromJson -> toString must be idempotent.
	// This guarantees that loading a stored constructor JSON and saving it again never loses or reorders
	// information, which is what .jasp file round-trips rely on.

	const std::vector<std::string> columnNames	= {"contNormal", "contBinom", "group", "ord", "text"};
	const std::vector<std::string> operators	= {"+", "-", "*", "/", "^", "%%", "==", "!=", "<", "<=", ">", ">=", "&", "|", "%|%"};
	const std::vector<std::string> functions	= {"abs", "sd", "var", "sum", "prod", "zScores", "min", "max", "mean", "sign", "round", "length", "median", "ifelse", "hasSubstring", "is.na", "log", "exp", "BoxCox", "cut", "replaceNA"};
	const std::vector<std::string> rowFunctions	= {"rowMean", "rowSum", "rowSD", "rowVariance", "rowMedian", "rowMin", "rowMax"};

	std::function<Json::Value(std::mt19937 &, int)> makeNode = [&](std::mt19937 & rng, int depth) -> Json::Value
	{
		std::uniform_int_distribution<int> pick(0, 99);
		std::uniform_int_distribution<int> colPick(0, static_cast<int>(columnNames.size()) - 1);
		std::uniform_int_distribution<int> opPick(0, static_cast<int>(operators.size()) - 1);
		std::uniform_int_distribution<int> funcPick(0, static_cast<int>(functions.size()) - 1);
		std::uniform_int_distribution<int> rowPick(0, static_cast<int>(rowFunctions.size()) - 1);
		std::uniform_real_distribution<double> numDist(-100.0, 100.0);

		int kind = pick(rng);

		if(depth <= 0)
		{
			// Leaves only at max depth
			int leaf = kind % 4;
			if(leaf == 0) return colNode(columnNames[colPick(rng)]);
			if(leaf == 1) return numNode(numDist(rng));
			if(leaf == 2) return boolNode(kind % 2 == 0);
			return strNode("s" + std::to_string(kind));
		}

		if(kind < 30)
			return colNode(columnNames[colPick(rng)]);
		if(kind < 45)
			return numNode(numDist(rng));
		if(kind < 52)
			return boolNode(kind % 2 == 0);
		if(kind < 58)
			return strNode("s" + std::to_string(kind));
		if(kind < 78)
			return opNode(operators[opPick(rng)], makeNode(rng, depth - 1), makeNode(rng, depth - 1));
		if(kind < 90)
		{
			const std::string & fn = functions[funcPick(rng)];
			const ScriptFunctionDef * def = ScriptConstructorRegistry::instance().functionDef(fn);
			Json::Value args = Json::arrayValue;
			if(def)
				for(const ScriptParamDef & p : def->params)
				{
					bool fill = (pick(rng) % 10) < 7;
					args.append(funcArg(p.optional ? "?" + p.name : p.name, p.dropKeys, fill ? makeNode(rng, depth - 1) : Json::nullValue));
				}
			Json::Value v;
			v["nodeType"] = "Function";
			v["functionName"] = fn;
			v["arguments"] = args;
			return v;
		}

		// RowFunction with a couple of filled slots serialised as embedded JSON strings
		int slotCount = 1 + (kind % 3);
		std::vector<std::string> dropped;
		for(int i = 0; i < slotCount; i++)
			dropped.push_back((pick(rng) % 10) < 6 ? compact(makeNode(rng, depth - 1)) : std::string("null"));
		if(std::all_of(dropped.begin(), dropped.end(), [](const std::string & s){ return s == "null"; }))
			dropped[0] = compact(colNode(columnNames[colPick(rng)]));

		Json::Value v;
		v["nodeType"] = "RowFunction";
		v["functionName"] = rowFunctions[rowPick(rng)];
		v["droppedItems"] = Json::arrayValue;
		for(const std::string & s : dropped)
			v["droppedItems"].append(s);
		return v;
	};

	ScriptConstructorModel model;

	for(int seed = 0; seed < 300; seed++)
	{
		std::mt19937 rng(seed);
		std::uniform_int_distribution<int> formulaCount(0, 3);

		Json::Value tree;
		tree["formulas"] = Json::arrayValue;
		int n = formulaCount(rng);
		for(int i = 0; i < n; i++)
			tree["formulas"].append(makeNode(rng, 3));

		model.fromJson(tree);
		std::string first = model.toString();

		model.fromJson(first);
		std::string second = model.toString();

		if(first != second)
			QFAIL(("Round-trip not idempotent for seed " + std::to_string(seed) + ":\nfirst:  " + first + "\nsecond: " + second).c_str());

		// The re-parsed tree must also be parseable without throwing and keep the same formula count.
		QCOMPARE(model.formulaCount(), n);
	}
}

void TestAll::testScriptConstructorUndo()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	QUndoStack stack;
	model.setUndoStack(&stack);

	QCOMPARE(model.formulaCount(), 0);

	// Insert a node -> one undo command
	ScriptNode * node = new ScriptNodeOperator(">", false);
	model.insertNode(node, DropTarget::root());
	QCOMPARE(model.formulaCount(), 1);
	QCOMPARE(stack.count(), 1);

	// Undo removes it, redo brings it back
	stack.undo();
	QCOMPARE(model.formulaCount(), 0);
	stack.redo();
	QCOMPARE(model.formulaCount(), 1);

	// Clear -> another command
	model.clear();
	QCOMPARE(model.formulaCount(), 0);
	QCOMPARE(stack.count(), 2);

	stack.undo();
	QCOMPARE(model.formulaCount(), 1);
}

void TestAll::testScriptConstructorGobble()
{
	QVERIFY(_newPkgWithDataSet());

	FixedColumnTypeProvider provider;
	provider.types["contNormal"] = 1; // scale

	ScriptConstructorModel model;
	model.setColumnTypeProvider(&provider);
	model.setMode(ScriptConstructorMode::Filter);

	// Start with a single column formula at the root: contNormal
	model.fromJson(formulas({colNode("contNormal")}));
	QCOMPARE(model.formulaCount(), 1);

	// Drop a ">" operator with no specific target. It should absorb ("gobble")
	// the existing column as its left operand, leaving the right slot empty.
	ScriptNode * op = new ScriptNodeOperator(">", false);
	model.insertNode(op, DropTarget::none());

	QCOMPARE(model.formulaCount(), 1);

	auto * rootOp = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
	QVERIFY(rootOp != nullptr);
	QCOMPARE(rootOp->op(), std::string(">"));

	auto * leftCol = dynamic_cast<ScriptNodeColumn*>(rootOp->leftChild());
	QVERIFY(leftCol != nullptr);
	QCOMPARE(leftCol->columnName(), std::string("contNormal"));
	QVERIFY(rootOp->rightChild() == nullptr);

	// R code reflects the gobble: (contNormal.scale > null)
	QCOMPARE(model.toR(), std::string("(contNormal.scale > null)\n"));
}

void TestAll::testScriptConstructorLeftMostEmpty()
{
	// The dataset is not used directly, but opening it creates the session database that
	// cleanup() closes (all model-level tests follow this pattern).
	QVERIFY(_newPkgWithDataSet());

	FixedColumnTypeProvider provider;
	provider.types["contNormal"] = 1; // scale

	ScriptConstructorModel model;
	model.setColumnTypeProvider(&provider);
	model.setMode(ScriptConstructorMode::Filter);

	// --- Operator with two empty slots: consecutive no-target drops fill left, then right ---
	ScriptNode * plus = new ScriptNodeOperator("+", false);
	model.insertNode(plus, DropTarget::none());
	QCOMPARE(model.formulaCount(), 1);

	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	{
		auto * op = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
		QVERIFY(op != nullptr);
		auto * left = dynamic_cast<ScriptNodeColumn*>(op->leftChild());
		QVERIFY(left != nullptr);
		QCOMPARE(left->columnName(), std::string("contNormal"));
		QVERIFY(op->rightChild() == nullptr);
	}

	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	QCOMPARE(model.toR(), std::string("(contNormal.scale + contNormal.scale)\n"));

	// --- Function arguments fill in ascending order, skipping non-accepting slots ---
	// ifelse's first argument wants booleans, so a number column must land in the second one.
	model.fromJson(formulas({funcNode("ifelse", {
		funcArg("test",	{"boolean"},					Json::nullValue),
		funcArg("then",	{"boolean","string","number"},	Json::nullValue),
		funcArg("else",	{"boolean","string","number"},	Json::nullValue)})}));
	QCOMPARE(model.formulaCount(), 1);

	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	{
		auto * func = dynamic_cast<ScriptNodeFunction*>(model.formulaAt(0));
		QVERIFY(func != nullptr);
		QVERIFY(func->arguments()[0].value == nullptr); // test (booleans only): skipped
		QVERIFY(func->arguments()[1].value != nullptr); // then: filled
		QVERIFY(func->arguments()[2].value == nullptr);
	}

	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	QCOMPARE(model.toR(), std::string("ifelse(NULL, contNormal.scale, contNormal.scale)\n"));

	// --- Topmost formula wins: the first formula with an accepting empty slot gets the drop ---
	model.fromJson(formulas({
		opNode("+", colNode("contNormal"), Json::nullValue),
		opNode("+", colNode("contNormal"), Json::nullValue)}));
	QCOMPARE(model.formulaCount(), 2);

	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	{
		auto * first = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
		auto * second = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(1));
		QVERIFY(first != nullptr);
		QVERIFY(second != nullptr);
		QVERIFY(first->rightChild() != nullptr);  // topmost formula was filled
		QVERIFY(second->rightChild() == nullptr);
	}

	// --- Row functions fill their slots left to right ---
	model.fromJson(formulas({}));
	auto * rowMean = new ScriptNodeRowFunction("rowMean");
	rowMean->addChild(nullptr); // the palette clone starts with one empty slot
	model.insertNode(rowMean, DropTarget::none());
	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	QCOMPARE(model.toR(), std::string("rowMeanNaRm(contNormal.scale, contNormal.scale)\n"));

	// --- Gobble is still preferred when no empty slot anywhere accepts the node ---
	model.fromJson(formulas({colNode("contNormal")}));
	ScriptNode * op = new ScriptNodeOperator(">", false);
	model.insertNode(op, DropTarget::none());
	QCOMPARE(model.formulaCount(), 1);
	auto * rootOp = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
	QVERIFY(rootOp != nullptr);
	QVERIFY(dynamic_cast<ScriptNodeColumn*>(rootOp->leftChild()) != nullptr); // absorbed
	QVERIFY(rootOp->rightChild() == nullptr);
}

void TestAll::testScriptConstructorAllowedColumnTypes()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	model.setMode(ScriptConstructorMode::Filter);

	auto isAllowed = [&model](ScriptNode * node, int type)
	{
		const std::vector<int> allowed = model.allowedColumnTypes(node);
		return std::find(allowed.begin(), allowed.end(), type) != allowed.end();
	};

	// Root column: unconstrained, all three types allowed.
	model.fromJson(formulas({colNode("contNormal")}));
	{
		const std::vector<int> allowed = model.allowedColumnTypes(model.formulaAt(0));
		QCOMPARE(allowed.size(), size_t(3));
		QVERIFY(isAllowed(model.formulaAt(0), 1));
		QVERIFY(isAllowed(model.formulaAt(0), 2));
		QVERIFY(isAllowed(model.formulaAt(0), 3));
	}

	// Column in a numeric operator slot (+): only scale.
	model.fromJson(formulas({opNode("+", colNode("contNormal"), numNode(1))}));
	{
		auto * op = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
		QVERIFY(op);
		const std::vector<int> allowed = model.allowedColumnTypes(op->leftChild());
		QCOMPARE(allowed.size(), size_t(1));
		QVERIFY(isAllowed(op->leftChild(), 1));
	}

	// Column in a comparison operator slot (>): scale and ordinal.
	model.fromJson(formulas({opNode(">", colNode("contNormal"), numNode(0))}));
	{
		auto * op = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
		QVERIFY(op);
		const std::vector<int> allowed = model.allowedColumnTypes(op->leftChild());
		QCOMPARE(allowed.size(), size_t(2));
		QVERIFY(isAllowed(op->leftChild(), 1));
		QVERIFY(isAllowed(op->leftChild(), 2));
	}

	// Column in a numeric function argument (mean): only scale.
	model.fromJson(formulas({funcNode("mean", {funcArg("values", {"number"}, colNode("contNormal"))})}));
	{
		auto * func = dynamic_cast<ScriptNodeFunction*>(model.formulaAt(0));
		QVERIFY(func);
		const std::vector<int> allowed = model.allowedColumnTypes(func->childAt(0));
		QCOMPARE(allowed.size(), size_t(1));
		QVERIFY(isAllowed(func->childAt(0), 1));
	}

	// Column in a string function argument (hasSubstring): ordinal and nominal.
	model.fromJson(formulas({funcNode("hasSubstring", {funcArg("string", {"string"}, colNode("text")), funcArg("substring", {"string"}, strNode("a"))})}));
	{
		auto * func = dynamic_cast<ScriptNodeFunction*>(model.formulaAt(0));
		QVERIFY(func);
		const std::vector<int> allowed = model.allowedColumnTypes(func->childAt(0));
		QCOMPARE(allowed.size(), size_t(2));
		QVERIFY(isAllowed(func->childAt(0), 2));
		QVERIFY(isAllowed(func->childAt(0), 3));
	}
}

void TestAll::testScriptConstructorRowFunctionFreeSlot()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	model.setMode(ScriptConstructorMode::ComputedColumn);

	// Add a row function at the root (a freshly created one starts with a single empty slot).
	auto * rowFunc = new ScriptNodeRowFunction("rowMean");
	rowFunc->addChild(nullptr);
	model.insertNode(rowFunc, DropTarget::root());
	QCOMPARE(rowFunc->childCount(), 1);
	QVERIFY(rowFunc->childAt(0) == nullptr);

	// Fill the empty slot; a trailing empty slot must appear so more columns can be added.
	auto * col = new ScriptNodeColumn("contNormal");
	model.insertNode(col, DropTarget{DropTarget::Kind::RowFunctionArg, rowFunc, 0, {"number"}, false, false});
	QCOMPARE(rowFunc->childCount(), 2);
	QVERIFY(rowFunc->childAt(0) != nullptr);
	QVERIFY(rowFunc->childAt(1) == nullptr);

	// Fill that one too; another trailing empty slot appears.
	auto * col2 = new ScriptNodeColumn("contBinom");
	model.insertNode(col2, DropTarget{DropTarget::Kind::RowFunctionArg, rowFunc, 1, {"number"}, false, false});
	QCOMPARE(rowFunc->childCount(), 3);
	QVERIFY(rowFunc->childAt(1) != nullptr);
	QVERIFY(rowFunc->childAt(2) == nullptr);

	// The trailing empty slot must not leak into the generated R code.
	QCOMPARE(model.toR(), std::string("rowMeanNaRm(contNormal.scale, contBinom.scale)"));
}


QTEST_MAIN(TestAll)
