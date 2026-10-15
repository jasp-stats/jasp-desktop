#include "testall.h"
#include "testinfo.h"
#include "numbersinlocales.h"
#include "tempfiles.h"
#include "processinfo.h"
#include "dirs.h"
#include <QDir>
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
#include "data/importers/minitabimporter.h"
#include "data/importers/database/databaseimportcolumn.h"
#include "utilities/settings.h"
#include "gui/preferencesmodel.h"
#include "utilities/desktopcommunicator.h"
#include "datasetsyncer.h"
#include <QTemporaryDir>
#include <QScopeGuard>
#include "columnutils.h"
#include "appinfo.h"
#include <cmath>
#include "dataset.h"
#include "workspace.h"
#include "undostack.h"
#include "filter.h"
#include "columnencoder.h"
#include "databridge.h"
#include "analysisbase.h"
#include "analysisform.h"
#include "controls/jaspcontrol.h"
#include "data/asyncloader.h"
#include "data/importers/csv/csvparser.h"
#include "data/datasetloader.h"
#include "data/expanddataproxymodel.h"
#include "archivereader.h"
#include "mainwindow.h"
#include "engine/enginerepresentation.h"
#include "results/resultsjsinterface.h"
#include "scriptconstructormodel.h"
#include "scriptnode.h"
#include "scriptconstructorregistry.h"

#include "mainwindow.h"
#include "data/filtermodel.h"
#include "data/columnmodel.h"
#include "data/columnsmodel.h"
#include "qquick/scriptconstructorview.h"
#include "qquick/scriptnodeitem.h"
#include "timers.h"

#include <QSignalSpy>
#include <QFile>
#include <QFileInfo>
#include <QEventLoop>
#include <QTimer>
#include <QUndoStack>
#include <QQuickWindow>
#include <QMouseEvent>
#include <QGuiApplication>
#include <QStandardItemModel>
#include <QtWebEngineQuick/qtwebenginequickglobal.h>
#include <random>
#include <functional>
#include <sqlite3.h>
#include <archive.h>
#include <archive_entry.h>


void TestAll::initTestCase()
{
	//Dirs::tempDir() falls back to "./" when the appdata dir is never set (main.cpp does it for the
	//real UI), so without this every run drops <pid>/internal.sqlite + <pid>/status in whatever the
	//CWD happens to be - the repo root when run from a build shell.
	Dirs::setLocalAppdataDir(QDir::tempPath().toStdString() + "/jasp-test-runs");
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

	// --- FilterWindow / VariablesWindow mutual exclusion (enforced in DataPanel.qml) ---
	// ColumnModel is parented to the DataSetPackage (QIdentityProxyModel(pkg)), not to MainWindow.
	FilterModel * filterModel = mainWindow->findChild<FilterModel*>();
	QVERIFY(filterModel != nullptr);
	ColumnModel * columnModel = DataSetPackage::pkg()->findChild<ColumnModel*>();
	QVERIFY(columnModel != nullptr);

	// Direction A: opening the FilterWindow must close the VariablesWindow through its usual
	// apply/discard route. Open variables first (filter closed -> nothing to close yet), then
	// open the filter. chosenColumn is -1, so the computed-column dialog cannot pop, and the
	// DataPanel Connections fire synchronously (direct connect) -> modal-free, no event-loop spin.
	// This leaves the FilterWindow open (its Loader builds it exactly once, like the original test).
	columnModel->setVisible(true);
	QVERIFY(columnModel->visible());
	QVERIFY(!filterModel->filterVisible());

	filterModel->setFilterVisible(true);
	QVERIFY(!columnModel->visible());      // VariablesWindow closed because the Filter opened
	QVERIFY(filterModel->filterVisible()); // FilterWindow stayed open

	// Open/keep the filter window the way the UI does (Loader in DataPanel.qml).

	// FilterWindow (objectName "filterWindow") must appear and the ScriptConstructor inside it
	// must have built its chrome now that it is visible.
	QQuickItem * filterWindow = nullptr;
	QTRY_VERIFY((filterWindow = _findQuickItemByName("filterWindow")) != nullptr);

	ScriptConstructorView * scriptConstructor = filterWindow->findChild<ScriptConstructorView*>();
	QVERIFY(scriptConstructor != nullptr);
	QTRY_VERIFY(scriptConstructor->scriptArea() != nullptr); // non-null once the chrome is built

	// Regression: a freshly opened, untouched filter (the default/empty filter, whose json is
	// DEFAULT_FILTER_JSON and equals the empty model's serialization) must NOT report as changed,
	// otherwise closing it wrongly pops the "apply or discard?" prompt even with no user action.
	QVERIFY2(!scriptConstructor->jsonChanged(), "Untouched filter constructor reported changes");
	QVERIFY(scriptConstructor->lastCheckPassed());

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

	// Direction B (reverse), done last: opening the VariablesWindow must close the FilterWindow
	// through its usual apply/discard route. The filter is clean here (just trashed to empty and
	// applied) and applyRequested is disconnected, so no dialog pops and the Loader-destroy churn
	// is minimal. It is last because closing the Filter deactivates its Loader and tears down the
	// FilterWindow/ScriptConstructor that the assertions above relied on.
	columnModel->setVisible(true);
	QVERIFY(!filterModel->filterVisible()); // FilterWindow closed because the VariablesWindow opened
	QVERIFY(columnModel->visible());        // VariablesWindow stayed open

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

///Cancelling the csv preview aborts the import from inside loadDataSet. A model reset that is left open then makes the whole of JASP draw
///nothing at all, which is what a blank window after Cancel comes down to. And nothing of the aborted import should be kept.
void TestAll::testCancelledImportLeavesTheModelUsable()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(true);	//true: this one asks for a delimiter, so it can be cancelled

	DataSet * dataSet = _pkg->createDataSet(); //As DataSetLoader::loadPackage does before it imports

	//The workspace and the dataset it shows are the models the data view draws
	QSignalSpy workspaceResetStarted(	_pkg->workspace(),	&QAbstractItemModel::modelAboutToBeReset);
	QSignalSpy workspaceResetFinished(	_pkg->workspace(),	&QAbstractItemModel::modelReset);
	QSignalSpy dataSetResetStarted(		dataSet,			&QAbstractItemModel::modelAboutToBeReset);
	QSignalSpy dataSetResetFinished(	dataSet,			&QAbstractItemModel::modelReset);

	DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0'); //Otherwise it would not ask at all

	//Answering with '\0' is what pressing Cancel in the preview window comes down to
	QMetaObject::Connection cancelling = connect(DesktopCommunicator::singleton(), &DesktopCommunicator::askCsvDelimiterSignal,
		[](const QString &, char) { DesktopCommunicator::singleton()->delimiterChosen('\0'); });

	QDir csvDir(_testLibrary());
	csvDir.cd("csv");

	bool threw = false;

	try
	{
		_importer->loadDataSet(fq(csvDir.absoluteFilePath("Data Import Test CSV.csv")), dataSet, [](int){});
	}
	catch(const std::exception &)
	{
		threw = true; //"No data loaded", the way an aborted import always ends
	}

	disconnect(cancelling);

	QVERIFY2(threw, "cancelling the preview should abort the import");
	QCOMPARE(workspaceResetFinished.count(),	workspaceResetStarted.count());	//Every begin got its end, so the models are usable again
	QCOMPARE(dataSetResetFinished.count(),		dataSetResetStarted.count());
	QCOMPARE(dataSet->columnCount(),			0);								//Nothing of the aborted import is kept, so the next import takes this empty dataset (see Workspace::createDataSet)
}

///A NULL in a numeric column of a database is missing, not 0: PostgreSQL, MySQL and ODBC hand it over as a number that is not there
void TestAll::testDatabaseImportNulls()
{
	DatabaseImportColumn column(nullptr, "x", QMetaType::fromType<double>());

	column.addValue(QVariant(1.5));
	column.addValue(QVariant(QMetaType::fromType<double>()));	//That NULL

	QCOMPARE(column.valueLookup(0), std::string("1.5"));
	QCOMPARE(column.valueLookup(1), std::string(""));
}

///Has (Q)ColumnUtils read and write numbers the way JASP does with its interface set to locale, until what is returned goes out of scope.
///The tests here otherwise run without any locale of the interface, just like the numbers in C.
static auto interfaceLocale(const QLocale & locale, bool useThousandSeparators = false)
{
	const QLocale defaultBefore;

	QColumnUtils::setCallbacksAndDefaultLocale(locale, useThousandSeparators);

	return qScopeGuard([defaultBefore]
	{
		QLocale::setDefault(defaultBefore);
		ColumnUtils::setCurrentQLocaleId("C");
		ColumnUtils::setDecimalPoint(".");
		ColumnUtils::setAlternativeDoubleToString(nullptr, nullptr);
		ColumnUtils::setExtraStringToNumber(nullptr, nullptr);
	});
}

///Has a csv import take ';' as delimiter and read its numbers as picked (nothing picked: the way the interface does),
///as if the csv preview just closed, until what is returned goes out of scope
static auto csvPreviewPicked(std::optional<QLocale> picked)
{
	DesktopCommunicator::singleton()->setKnownCsvDelimiter(';');
	DesktopCommunicator::singleton()->setKnownImportLocale(picked);

	return qScopeGuard([]
	{
		DesktopCommunicator::singleton()->setKnownCsvDelimiter('\0');
		DesktopCommunicator::singleton()->setKnownImportLocale(std::nullopt);
	});
}

///The locale picked in the csv preview decides how the numbers of that file are read, whatever the locale of the interface:
///"1,234" in a German file is one point two three four, and "1,234.56" is no German number at all even though English reads it fine.
void TestAll::testCsvImportLocale()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);	//false: take the delimiter below instead of asking for it

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));
	QVERIFY(csv.open(QIODevice::WriteOnly));
	csv.write("x;y\n1,234;1,234.56\n86,298;2\n0,5;3\n1.234,56;4\n");
	csv.close();

	auto english = interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates));

	auto german = csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

	QVERIFY(_pkg->dataSet() && _pkg->dataSet()->columnCount() == 2);

	const doublevec & values = _pkg->dataSet()->column(0)->dbls();

	QCOMPARE(values.size(),	size_t(4));
	QCOMPARE(values[0],		1.234);
	QCOMPARE(values[1],		86.298);
	QCOMPARE(values[2],		0.5);
	QCOMPARE(values[3],		1234.56);

	QCOMPARE(_pkg->dataSet()->column(1)->getValue(0), std::string("1,234.56")); //Text, not the number the interface would make of it
}

///A csv with a locale of its own hands its numbers over written the way C does (see CSVImportColumn), but the data shows them the way the interface
///writes them, so that is the width the column needs: with thousand separators 1234567.5 takes up 11 characters, not 9
void TestAll::testCsvImportLocaleColumnWidth()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));
	QVERIFY(csv.open(QIODevice::WriteOnly));
	csv.write("x\n1234567,5\n");
	csv.close();

	auto english = interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates), true);

	auto german = csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

	QVERIFY(_pkg->dataSet() && _pkg->dataSet()->columnCount() == 1);

	Column * column = _pkg->dataSet()->column(0);

	QCOMPARE(column->getValue(0),										std::string("1,234,567.5"));
	QCOMPARE(column->getMaximumWidthInCharacters(true, true, 0),		size_t(11));
}

///A locale that groups thousands with a space reads numbers grouped with any kind of space: French prescribes U+202F and Russian U+00A0,
///but a file has whichever one the software that wrote it knew, or the plain space of a keyboard
void TestAll::testNumbersGroupedWithSpaces()
{
	const QList<QChar> spaces = { u' ', QChar(0x00A0), QChar(0x202F), QChar(0x2009) }; //Plain, no-break, narrow no-break and thin

	for(const QLocale & locale : { QLocale(QLocale::French, QLocale::France), QLocale(QLocale::French, QLocale::Canada), QLocale(QLocale::Russian, QLocale::Russia), QLocale(QLocale::Swedish, QLocale::Sweden) })
	{
		auto interfaceIsSet	= interfaceLocale(locale); //For reading whole numbers, ColumnUtils::getIntValue
		auto readNumbersAs	= QColumnUtils::stringToDoubleFor(locale);

		for(QChar space : spaces)
		{
			const QString	wholeNumber	= "1" + QString(space) + "234",
							number		= wholeNumber + locale.decimalPoint() + "5",
							what		= " in " + locale.bcp47Name() + " with U+" + QString::number(space.unicode(), 16).toUpper().rightJustified(4, u'0');
			double			read		= 0;
			int				readWhole	= 0;

			QVERIFY2(readNumbersAs(fq(number), read) && read == 1234.5,						qPrintable(number		+ what));
			QVERIFY2(ColumnUtils::getIntValue(fq(wholeNumber), readWhole) && readWhole == 1234,	qPrintable(wholeNumber	+ what));
		}
	}

	//A locale that groups with something else does not take a space for its group separator
	double read;
	QVERIFY(!QColumnUtils::stringToDoubleFor(QLocale(QLocale::English, QLocale::UnitedStates))(fq("1" + QString(QChar(0x00A0)) + "234.5"), read));
}

///Writes NumbersInLocales::samples() to a csv, each in a column of its own so that no sample decides what the others are, rows times
static QString writeNumberSamples(const QTemporaryDir & dir, int rows = 1)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	QByteArray names, values;

	for(size_t i=0; i<samples.size(); i++)
	{
		names	+= (i ? ";" : "") + QByteArray("sample") + QByteArray::number(qulonglong(i));
		values	+= (i ? ";" : "") + QByteArray(samples[i].written);
	}

	QFile csv(dir.filePath("numbers.csv"));

	if(!csv.open(QIODevice::WriteOnly))
		return "";

	csv.write(names + "\n" + QByteArray(values + "\n").repeated(rows));

	return csv.fileName();
}

///Where a sample is in the data (see writeNumberSamples), for a message
static QString sampleAt(const NumbersInLocales::Sample & sample, size_t row)
{
	return QString("\"%1\" in row %2").arg(QString::fromUtf8(sample.written)).arg(row + 1);
}

///Every sample in every row of the data (see writeNumberSamples) that is not the number, or the text, it is in the language readIn
static QStringList numberSamplesReadWrong(DataSet * data, const QLocale & readIn)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	if(!data || data->columnCount() != int(samples.size()))
		return { "the data should have a column for every sample" };

	QStringList readWrong;

	for(size_t i=0; i<samples.size(); i++)
		for(size_t row=0; row<data->column(i)->rowCount(); row++)
		{
			const double	number		= samples[i].readIn(readIn);
			double			read		= 0;
			const bool		readNumber	= data->column(i)->numberAt(row, read);

			if(std::isnan(number) ? readNumber : !readNumber || !qFuzzyCompare(read, number))
				readWrong.push_back(QString("%1 should be %2, but is %3").arg(sampleAt(samples[i], row), std::isnan(number) ? "text" : QString::number(number, 'g', 12), readNumber ? QString::number(read, 'g', 12) : "text"));
		}

	return readWrong;
}

///Every sample in every row of the data (see writeNumberSamples) that is not shown the way the interface shows what it is in the language readIn
static QStringList numberSamplesShownWrong(DataSet * data, const QLocale & readIn, const QLocale & interface, bool thousandSeparators)
{
	const std::vector<NumbersInLocales::Sample> & samples = NumbersInLocales::samples();

	if(!data || data->columnCount() != int(samples.size()))
		return { "the data should have a column for every sample" };

	QStringList shownWrong;

	for(size_t i=0; i<samples.size(); i++)
	{
		Column		*	column	= data->column(i);
		const double	number	= samples[i].readIn(readIn);

		//Text stays as it was written, and a column of whole numbers comes out nominal, where the label writes its number plainly: without thousand separators
		const QString	shouldShow	= std::isnan(number)					? QString::fromUtf8(samples[i].written)
									: column->type() == columnType::scale	? NumbersInLocales::shownIn(interface, number, thousandSeparators)
																			: QString::number(number, 'g', 10);

		for(size_t row=0; row<column->rowCount(); row++)
		{
			const QString shows = tq(column->getValue(row));

			if(shows != shouldShow)
				shownWrong.push_back(QString("%1 should show as \"%2\", but shows as \"%3\"").arg(sampleAt(samples[i], row), shouldShow, shows));
		}
	}

	return shownWrong;
}

///Every combination of an interface in English, German or French, with and without thousand separators, and a csv whose numbers are written in
///English, German or French, or in a language nobody picked (the csv preview was never shown, as when synchronising a file imported before it existed)
void TestAll::testCsvImportNumbers_data()
{
	QTest::addColumn<QLocale>(	"interface");
	QTest::addColumn<bool>(		"thousandSeparators");
	QTest::addColumn<bool>(		"picked");
	QTest::addColumn<QLocale>(	"readIn");

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			const QString interfaceIs = QLocale::languageToString(interface.language()) + " interface" + (thousandSeparators ? " with thousand separators" : "");

			for(const QLocale & file : NumbersInLocales::locales())
				QTest::addRow("%s, %s file", qPrintable(interfaceIs), qPrintable(QLocale::languageToString(file.language())))
					<< interface << thousandSeparators << true << file;

			//Nothing picked: read the way the interface reads
			QTest::addRow("%s, nothing picked", qPrintable(interfaceIs))
				<< interface << thousandSeparators << false << interface;
		}
}

///A number is read in the language of its file and shown in the language of the interface, whichever combination of the two it is
void TestAll::testCsvImportNumbers()
{
	QFETCH(QLocale,	interface);
	QFETCH(bool,	thousandSeparators);
	QFETCH(bool,	picked);
	QFETCH(QLocale,	readIn);

	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	const QString	csv = writeNumberSamples(dir);
	QVERIFY(!csv.isEmpty());

	auto interfaceIsSet	= interfaceLocale(interface, thousandSeparators);
	auto fileIsPicked	= csvPreviewPicked(picked ? std::optional<QLocale>(readIn) : std::nullopt);

	_importer->loadDataSet(fq(csv), _pkg->createDataSet(), [](int){});

	const QStringList	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn),
						shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));
}

void TestAll::testCsvSyncNumbers_data()
{
	testCsvImportNumbers_data();
}

///Synchronising reads the file with the same choices again (see DataSetLoader::syncPackage), so an unchanged file changes nothing:
///the values read are compared with the values shown (see ImportColumn::valueLookupAsShown), in whichever language each of them is.
///A file that did change fills its columns again, and then every value has to find back the label it had (see Column::setValue).
void TestAll::testCsvSyncNumbers()
{
	QFETCH(QLocale,	interface);
	QFETCH(bool,	thousandSeparators);
	QFETCH(bool,	picked);
	QFETCH(QLocale,	readIn);

	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	const QString	csv = writeNumberSamples(dir);
	QVERIFY(!csv.isEmpty());

	auto interfaceIsSet	= interfaceLocale(interface, thousandSeparators);
	auto fileIsPicked	= csvPreviewPicked(picked ? std::optional<QLocale>(readIn) : std::nullopt);

	_importer->loadDataSet(fq(csv), _pkg->createDataSet(), [](int){});

	//MainWindow asks the user whether to synchronise, here the answer is always yes
	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	auto synchronise = [&]
	{
		CSVImporter syncer(false);
		syncer.syncDataSet(fq(csv), _pkg->dataSet(), [](int){});
	};

	auto samplesChanged = [&]
	{
		QStringList samples;
		for(const QString & column : changed.last()[1].toStringList()) //changedColumns (after dataSetID), named by writeNumberSamples
			samples.push_back(QString::fromUtf8(NumbersInLocales::samples()[column.mid(QString("sample").size()).toULongLong()].written));
		return samples;
	};

	synchronise();

	QCOMPARE(changed.count(), 1);
	QVERIFY2(samplesChanged().isEmpty(), qPrintable("these samples count as changed: " + samplesChanged().join(", ")));

	QStringList	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn),
				shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));

	QVERIFY(!writeNumberSamples(dir, 2).isEmpty()); //One row more changes every column

	synchronise();

	QCOMPARE(changed.count(),		2);
	QCOMPARE(samplesChanged().size(),	int(NumbersInLocales::samples().size()));
	QCOMPARE(_pkg->dataSet()->rowCount(),	2);

	readWrong	= numberSamplesReadWrong(	_pkg->dataSet(), readIn);
	shownWrong	= numberSamplesShownWrong(	_pkg->dataSet(), readIn, interface, thousandSeparators);

	QVERIFY2(readWrong	.isEmpty(), qPrintable("\n" + readWrong	.join("\n")));
	QVERIFY2(shownWrong	.isEmpty(), qPrintable("\n" + shownWrong	.join("\n")));
}

///A sync compares numbers as numbers (however they are shown), but text as text: also text the interface would read as that same number,
///because the import did not. "1,234.56" is no German number, so in a German file it is text, even if JASP shows 1234.56 just like that in English.
void TestAll::testCsvSyncTextAndNumbersStayApart()
{
	if(_pkg)		delete _pkg;
	if(_importer)	delete _importer;

	_pkg		= new DataSetPackage(this);
	_importer	= new CSVImporter(false);

	QTemporaryDir	dir;
	QFile			csv(dir.filePath("german.csv"));

	auto writeCsv = [&](const QByteArray & content)
	{
		if(!csv.open(QIODevice::WriteOnly))
			return false;
		csv.write(content);
		csv.close();
		return true;
	};

	auto english	= interfaceLocale(QLocale(QLocale::English, QLocale::UnitedStates));
	auto german		= csvPreviewPicked(QLocale(QLocale::German, QLocale::Germany));

	QVERIFY(writeCsv("becomesNumber;becomesText\n1,234.56;1234,56\n"));
	_importer->loadDataSet(fq(csv.fileName()), _pkg->createDataSet(), [](int){});

	double number;
	QVERIFY(!_pkg->dataSet()->column("becomesNumber")	->numberAt(0, number));
	QVERIFY( _pkg->dataSet()->column("becomesText")		->numberAt(0, number));

	//The same number the other way around, and read the way the interface does, the file would be the same
	QVERIFY(writeCsv("becomesNumber;becomesText\n1234,56;1,234.56\n"));

	QMetaObject::Connection syncing = connect(_pkg, &DataSetPackage::checkDoSync, this, []{ return true; });
	auto stopSyncing = qScopeGuard([syncing]{ disconnect(syncing); });

	QSignalSpy changed(_pkg, &DataSetPackage::datasetChanged);

	CSVImporter syncer(false);
	syncer.syncDataSet(fq(csv.fileName()), _pkg->dataSet(), [](int){});

	QCOMPARE(changed.count(), 1);

	QStringList changedColumns = changed[0][1].toStringList(); //After dataSetID
	changedColumns.sort();
	QCOMPARE(changedColumns, QStringList({ "becomesNumber", "becomesText" }));

	QVERIFY( _pkg->dataSet()->column("becomesNumber")	->numberAt(0, number));
	QCOMPARE(number, 1234.56);
	QVERIFY(!_pkg->dataSet()->column("becomesText")		->numberAt(0, number));
	QCOMPARE(_pkg->dataSet()->column("becomesText")->strs()[0], std::string("1,234.56")); //Still a scale column, which shows text as missing
}

///The delimiter and locale of a csv hold for a single load or sync, also one that fails: left behind, the next csv opened
///would not show its preview but be split with that delimiter (see DesktopCommunicator::askCsvDelimiter)
void TestAll::testFailedLoadOrSyncForgetsCsvChoices()
{
	if(_pkg) delete _pkg;

	_pkg = new DataSetPackage(this);
	_pkg->createDataSet();

	DesktopCommunicator * communicator = DesktopCommunicator::singleton();

	//An empty csv cannot be read (CSV::open throws), as happens when a synchronised file is being written just then
	QTemporaryDir	dir;
	QFile			empty(dir.filePath("empty.csv"));
	QVERIFY(empty.open(QIODevice::WriteOnly));
	empty.close();

	_pkg->dataSet()->setCsvChoices(';', "de");

	QVERIFY_THROWS_EXCEPTION(std::exception, DataSetLoader::syncPackage(fq(empty.fileName()), ".csv", _pkg->dataSet()));
	QCOMPARE(communicator->knownCsvDelimiter(), '\0');
	QVERIFY (!communicator->knownImportLocale());

	communicator->setKnownCsvDelimiter(';'); //Which is how data_load in MainWindow opens a csv without its preview

	QVERIFY_THROWS_EXCEPTION(std::exception, DataSetLoader::loadPackage(fq(empty.fileName()), ".csv"));
	QCOMPARE(communicator->knownCsvDelimiter(), '\0');
	QVERIFY (!communicator->knownImportLocale());
}

///The numbers in an .ods file are written the way C writes them (office:value), whatever the locale of the spreadsheet or of JASP,
///so an interface in whatever language must read them just like C does: a German one must not read 0.111 as one hundred and eleven.
void TestAll::testOdsImportLocale()
{
	QDir odsDir(_testLibrary());
	odsDir.cd("ods");

	auto importIt = [&]()
	{
		DatabaseInterface::closeInterfaces(); //Like cleanup() does, a DataSetPackage wants a database of its own
		delete _pkg;
		delete _importer;

		_pkg		= new DataSetPackage(this);
		_importer	= new ods::ODSImporter();

		_importer->loadDataSet(fq(odsDir.absoluteFilePath("Data Import Test ODS.ods")), _pkg->createDataSet(), [](int){});

		std::vector<doublevec> values;
		for(Column * column : _pkg->dataSet()->columns())
			values.push_back(column->dbls());

		return values;
	};

	const std::vector<doublevec> inC = importIt();

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			auto interfaceIsSet = interfaceLocale(interface, thousandSeparators);

			const std::vector<doublevec>	inInterface	= importIt();
			const QString					interfaceIs	= QLocale::languageToString(interface.language()) + (thousandSeparators ? " with thousand separators" : "");

			QCOMPARE(inInterface.size(), inC.size());

			for(size_t c=0; c<inC.size(); c++)
			{
				QCOMPARE(inInterface[c].size(), inC[c].size());

				for(size_t r=0; r<inC[c].size(); r++)
					QVERIFY2((std::isnan(inC[c][r]) && std::isnan(inInterface[c][r])) || inC[c][r] == inInterface[c][r], qPrintable(QString("column %1 row %2: %3 in C but %4 in %5").arg(c).arg(r).arg(inC[c][r]).arg(inInterface[c][r]).arg(interfaceIs)));
			}
		}
}

///Writes a Minitab worksheet (.mwx): a zip holding a metadata file and the sheet it points to, both json (see Minitab::findMetadataPath)
static bool writeMinitabWorksheet(const QString & path, const std::string & sheetJson)
{
	const std::map<std::string, std::string> files =
	{
		{ "sheet_metadata.json",	R"({ "Worksheet": { "Uri": "sheets/0/sheet" } })"	},
		{ "sheets/0/sheet.json",	sheetJson											}
	};

	struct archive * zip = archive_write_new();
	archive_write_set_format_zip(zip);

	bool written = archive_write_open_filename(zip, fq(path).c_str()) == ARCHIVE_OK;

	for(const auto & [name, content] : files)
	{
		struct archive_entry * entry = archive_entry_new();
		archive_entry_set_pathname(	entry, name.c_str());
		archive_entry_set_size(		entry, content.size());
		archive_entry_set_filetype(	entry, AE_IFREG);
		archive_entry_set_perm(		entry, 0644);

		written = written && archive_write_header(zip, entry) == ARCHIVE_OK && archive_write_data(zip, content.data(), content.size()) == la_ssize_t(content.size());

		archive_entry_free(entry);
	}

	written = archive_write_close(zip) == ARCHIVE_OK && written;
	archive_write_free(zip);

	return written;
}

///A Minitab worksheet stores numbers, not text written in some locale, so whatever the interface a number is read as the number it is:
///one with three decimals is what an interface grouping thousands with a dot would take for a thousand times as much (1.234 in German).
void TestAll::testMinitabImportLocale()
{
	QTemporaryDir	dir;
	const QString	worksheet	= dir.filePath("numbers.mwx");
	const doublevec	numbers		= { 1.234, 0.125, 12.375, 1234.5, 2 };

	QVERIFY(writeMinitabWorksheet(worksheet, R"({ "MaxRows_DEP": 5, "MaxColumns_DEP": 1, "Data": { "Columns": [
		{ "WorksheetVarBody": { "Name": "x", "VarData": { "VarDataBody": { "NumericData": [ 1.234, 0.125, 12.375, 1234.5, 2 ] } } } } ] } })"));

	for(const QLocale & interface : NumbersInLocales::locales())
		for(bool thousandSeparators : { false, true })
		{
			auto interfaceIsSet = interfaceLocale(interface, thousandSeparators);

			DatabaseInterface::closeInterfaces(); //Like cleanup() does, a DataSetPackage wants a database of its own
			delete _pkg;
			delete _importer;

			_pkg		= new DataSetPackage(this);
			_importer	= new MinitabImporter();

			_importer->loadDataSet(fq(worksheet), _pkg->createDataSet(), [](int){});

			Column * column = _pkg->dataSet() ? _pkg->dataSet()->column("x") : nullptr;
			QVERIFY(column);
			QCOMPARE(column->rowCount(), numbers.size());

			for(size_t row=0; row<numbers.size(); row++)
			{
				double read = 0;
				QVERIFY2(column->numberAt(row, read) && read == numbers[row], qPrintable(QString("%1 in row %2 is read as %3 by an interface in %4%5")
					.arg(numbers[row]).arg(row + 1).arg(read, 0, 'g', 12).arg(QLocale::languageToString(interface.language()), thousandSeparators ? " with thousand separators" : "")));
			}
		}
}

///A jaspfile saved before csvDelimiter and importLocale existed gets them when it is opened, also when it says it was saved by the version this is
void TestAll::testDataSetsTableUpgrade()
{
	if(_pkg)	delete _pkg;

	_pkg = new DataSetPackage(this);	//Gives a freshly created internal database

	const int dataSetId = _pkg->createDataSet()->id();

	DatabaseInterface * db = DatabaseInterface::singleton();

	db->runStatements("ALTER TABLE DataSets DROP COLUMN importLocale;");
	db->runStatements("ALTER TABLE DataSets DROP COLUMN csvDelimiter;");

	db->upgradeDBFromVersion(AppInfo::version);

	std::string	title, dataFilePath, description, databaseJson, emptyValuesJson, importLocale = "not read";
	long		dataFileTimestamp;
	int			revision;
	bool		dataSynch;
	char		csvDelimiter = 'x';

	//Throws "no such column" when the upgrade left one of them out
	db->dataSetLoad(dataSetId, title, dataFilePath, dataFileTimestamp, description, databaseJson, emptyValuesJson, revision, dataSynch, csvDelimiter, importLocale);

	QCOMPARE(csvDelimiter,	'\0');
	QCOMPARE(importLocale,	std::string(""));
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

void TestAll::testCsvParserTrimsOnlyUnquotedPadding()
{
	CSVParser parser(',');

	const CSVParser::Grid grid = parser.parse(std::string(
		" a , b\t,\tc \n"
		"\" a \",\" b\",\"c \"\n"
		" \"a\" ,\t\"b\"\t,\"c\"\n"
		"   ,\"\",\"  \"\n"
		"\"a,b\",\"a\"\"b\",d\n"));

	QCOMPARE(grid.size(), size_t(5));

	//Unquoted padding is not data, so it goes.
	QCOMPARE(grid[0], stringvec({"a", "b", "c"}));

	//What sits inside the quotes is data, so it stays.
	QCOMPARE(grid[1], stringvec({" a ", " b", "c "}));

	//And padding around a quoted field is padding too.
	QCOMPARE(grid[2], stringvec({"a", "b", "c"}));

	//A field of nothing but whitespace is empty (which the importers read as a missing value), while
	//quoted whitespace is a value of its own.
	QCOMPARE(grid[3], stringvec({"", "", "  "}));

	//None of this may disturb the quoting itself.
	QCOMPARE(grid[4], stringvec({"a,b", "a\"b", "d"}));
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

void TestAll::testManualEditStopsExternalSynching()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QVERIFY(!_pkg->synchingExternally());

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	QString testFilePath = tempDir.filePath("manual_edit.csv");
	QVERIFY(_writeTextFile(testFilePath, "a,b,c\n1,2,3\n"));

	QSignalSpy synchingSpy(_pkg, &DataSetPackage::synchingExternallyChanged);

	ds->syncer().startFileSyncing(testFilePath);
	QVERIFY(_pkg->synchingExternally());
	//Setting the data file and turning the synching on each announce themselves, so what matters is
	//that the last thing everybody heard is the state we are actually in.
	QVERIFY(synchingSpy.count() > 0);
	QVERIFY(synchingSpy.last().first().toBool());

	//Changing a value by hand means the data file no longer reflects the workspace, so the synching
	//has to stop and everybody watching (the ribbon) has to hear about it.
	synchingSpy.clear();
	const QModelIndex	cell		= ds->index(0, 0);
	const QString		oldValue	= ds->data(cell, int(dataPkgRoles::value)).toString();
	QVERIFY(ds->setData(cell, QVariant(oldValue + "9"), int(dataPkgRoles::value)));

	QVERIFY(_pkg->manualEdits());
	QVERIFY(!ds->dataFileSynch());
	QVERIFY(!_pkg->synchingExternally());
	QVERIFY(synchingSpy.count() > 0);
	QVERIFY(!synchingSpy.last().first().toBool());

	//Turning it back on is what the ribbon button does. That also clears the manual-edits flag, so a
	//next edit can disable the synching again.
	synchingSpy.clear();
	_pkg->setSynchingExternally(true);

	QVERIFY(_pkg->synchingExternally());
	QVERIFY(!_pkg->manualEdits());
	QVERIFY(synchingSpy.count() > 0);
	QVERIFY(synchingSpy.last().first().toBool());

	ds->syncer().stopFileSyncing();
}

void TestAll::testReloadDataFileDiscardsManualEdits()
{
	_pkg = new DataSetPackage(this);

	//Importer::syncDataSet reads the preferences singleton; nothing else in the tests creates one.
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);
	QVERIFY(PreferencesModel::prefs());

	//syncDataSet asks permission through checkDoSync; with no MainWindow around nothing answers it.
	connect(DataSetPackage::pkg(), &DataSetPackage::checkDoSync, this, &TestAll::_checkDoSyncFake, Qt::DirectConnection);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());

	const QString csvPath = tempDir.filePath("reload.csv");
	QVERIFY(_writeTextFile(csvPath, "a,b\n1,2\n3,4\n"));

	DataSet * ds = _pkg->createDataSet();
	QVERIFY(ds);
	_pkg->workspace()->setShownDataSet(ds);

	CSVImporter importer;
	importer.loadDataSet(fq(csvPath), ds, [](int){});

	ds->syncer().startFileSyncing(csvPath);
	QVERIFY(_pkg->synchingExternally());

	const QModelIndex cell = ds->index(0, 0);
	QCOMPARE(ds->data(cell, int(dataPkgRoles::value)).toString(), QString("1"));

	//Editing by hand is what turns the synching off, and what "Reload Data File" has to undo.
	QVERIFY(ds->setData(cell, QVariant("999"), int(dataPkgRoles::value)));
	QCOMPARE(ds->data(cell, int(dataPkgRoles::value)).toString(), QString("999"));
	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	//This is what picking "Reload Data File" does: turn the synching back on and re-import the
	//(unchanged) data file. That has to bring the original value back.
	_pkg->setSynchingExternally(true);
	QVERIFY(_pkg->synchingExternally());

	CSVImporter reloader;
	reloader.syncDataSet(fq(csvPath), ds, [](int){});

	QCOMPARE(ds->data(cell, int(dataPkgRoles::value)).toString(), QString("1"));
	QVERIFY(!_pkg->manualEdits());

	//A sync replaces the data, but those changes are not edits by the user, so it must not switch the
	//synching off again - the Synchronisation button would turn itself off after every sync.
	QVERIFY(ds->dataFileSynch());
	QVERIFY(_pkg->synchingExternally());

	//Which also has to hold for a sync that finds nothing to change...
	CSVImporter reloadAgain;
	reloadAgain.syncDataSet(fq(csvPath), ds, [](int){});

	QVERIFY(!_pkg->manualEdits());
	QVERIFY(_pkg->synchingExternally());

	//...and for one that brings in a new column, which DataSet::createColumn announces as a manual edit.
	QVERIFY(_writeTextFile(csvPath, "a,b,c\n1,2,5\n3,4,6\n"));

	CSVImporter reloadWithNewColumn;
	reloadWithNewColumn.syncDataSet(fq(csvPath), ds, [](int){});

	QCOMPARE(ds->columnCount(), 3);
	QVERIFY(!_pkg->manualEdits());
	QVERIFY(_pkg->synchingExternally());

	ds->syncer().stopFileSyncing();
}

void TestAll::testUndoingManualEditRestoresSynching()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString csvPath = tempDir.filePath("undo_synch.csv");
	QVERIFY(_writeTextFile(csvPath, "a,b\n1,2\n"));

	ds->syncer().startFileSyncing(csvPath);
	QVERIFY(_pkg->synchingExternally());

	const QModelIndex	cell		= ds->index(0, 0);
	const QString		oldValue	= ds->data(cell, int(dataPkgRoles::value)).toString();

	//Exactly what editing a cell in the data view does: push the change as an undo command.
	ds->undoStack()->push(new SetDataCommand(ds, 0, 0, QVariant(oldValue + "9"), int(dataPkgRoles::value)));

	QCOMPARE(ds->data(cell, int(dataPkgRoles::value)).toString(), oldValue + "9");
	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	//Undoing puts the data back to what the data file holds, so the synching must come back on.
	ds->undoStack()->undo();

	QCOMPARE(ds->data(cell, int(dataPkgRoles::value)).toString(), oldValue);
	QVERIFY(!_pkg->manualEdits());
	QVERIFY(_pkg->synchingExternally());

	//And redoing the edit switches it off again.
	ds->undoStack()->redo();

	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	ds->syncer().stopFileSyncing();
}

void TestAll::testSynchRestoreIsPerDataSet()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString	csvA = tempDir.filePath("a.csv"),
					csvB = tempDir.filePath("b.csv");
	QVERIFY(_writeTextFile(csvA, "a,b\n1,2\n"));
	QVERIFY(_writeTextFile(csvB, "p,q\n3,4\n"));

	auto loadInto = [&](DataSet * ds, const QString & csv)
	{
		_pkg->workspace()->setShownDataSet(ds);
		CSVImporter importer;
		importer.loadDataSet(fq(csv), ds, [](int){});
	};

	//A synchs with its data file and then gets edited by hand, which switches its synching off.
	DataSet * dsA = _pkg->createDataSet();
	QVERIFY(dsA);
	loadInto(dsA, csvA);
	dsA->syncer().startFileSyncing(csvA);
	QVERIFY(_pkg->synchingExternally());

	dsA->undoStack()->push(new SetDataCommand(dsA, 0, 0, QVariant("99"), int(dataPkgRoles::value)));
	QVERIFY(dsA->synchTurnedOffByManualEdits());
	QVERIFY(!_pkg->synchingExternally());

	//B has a data file too, but is deliberately not synching with it.
	DataSet * dsB = _pkg->createDataSet();
	QVERIFY(dsB);
	loadInto(dsB, csvB);
	dsB->setDataFile(fq(csvB));
	QVERIFY(!dsB->dataFileSynch());
	QVERIFY(!_pkg->synchingExternally());

	//Undoing something in B brings *B's* undo stack back to clean. A's hand edits must not make that
	//turn B's synching on behind the user's back.
	dsB->undoStack()->push(new SetDataCommand(dsB, 0, 0, QVariant("77"), int(dataPkgRoles::value)));
	dsB->undoStack()->undo();

	QVERIFY(!dsB->synchTurnedOffByManualEdits());
	QVERIFY(!dsB->dataFileSynch());
	QVERIFY(!_pkg->synchingExternally());

	//While undoing in A, which is where the edits were made, does turn it back on.
	_pkg->workspace()->setShownDataSet(dsA);
	dsA->undoStack()->undo();

	QVERIFY(dsA->dataFileSynch());
	QVERIFY(_pkg->synchingExternally());

	dsA->syncer().stopFileSyncing();
}

void TestAll::testManualEditsAreTrackedPerDataSet()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString	csvA = tempDir.filePath("a.csv"),
					csvB = tempDir.filePath("b.csv");
	QVERIFY(_writeTextFile(csvA, "a,b\n1,2\n"));
	QVERIFY(_writeTextFile(csvB, "p,q\n3,4\n"));

	auto synchedDataSetFrom = [&](const QString & csv)
	{
		DataSet * ds = _pkg->createDataSet();
		_pkg->workspace()->setShownDataSet(ds);

		CSVImporter importer;
		importer.loadDataSet(fq(csv), ds, [](int){});
		ds->syncer().startFileSyncing(csv);

		return ds;
	};

	//A synchs with its data file and is then edited by hand, which switches its synching off.
	DataSet * dsA = synchedDataSetFrom(csvA);
	QVERIFY(dsA);
	QVERIFY(dsA->dataFileSynch());

	dsA->undoStack()->push(new SetDataCommand(dsA, 0, 0, QVariant("99"), int(dataPkgRoles::value)));
	QVERIFY(dsA->manualEdits());
	QVERIFY(!dsA->dataFileSynch());

	//B is a second dataset, synching with a data file of its own. A's hand edits are not B's.
	DataSet * dsB = synchedDataSetFrom(csvB);
	QVERIFY(dsB);
	QVERIFY(dsB->dataFileSynch());
	QVERIFY(!dsB->manualEdits());
	QVERIFY(!_pkg->manualEdits());		//B is the shown one, and B was not edited
	QVERIFY(_pkg->synchingExternally());

	//Editing B by hand has to switch B's synching off too, even though A was already edited.
	dsB->undoStack()->push(new SetDataCommand(dsB, 0, 0, QVariant("77"), int(dataPkgRoles::value)));

	QVERIFY(dsB->manualEdits());
	QVERIFY(!dsB->dataFileSynch());
	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	//And none of that touched A.
	QVERIFY(dsA->manualEdits());
	QVERIFY(!dsA->dataFileSynch());

	//Undoing B's edit brings B's synching back, and leaves A alone.
	dsB->undoStack()->undo();

	QVERIFY(!dsB->manualEdits());
	QVERIFY(dsB->dataFileSynch());
	QVERIFY(dsA->manualEdits());
	QVERIFY(!dsA->dataFileSynch());

	dsA->syncer().stopFileSyncing();
	dsB->syncer().stopFileSyncing();
}

void TestAll::testLabelEditDoesNotStopSynching()
{
	_pkg = new DataSetPackage(this);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString csvPath = tempDir.filePath("labels.csv");
	QVERIFY(_writeTextFile(csvPath, "a,b\nx,1\ny,2\n"));

	DataSet * ds = _pkg->createDataSet();
	QVERIFY(ds);
	_pkg->workspace()->setShownDataSet(ds);

	CSVImporter importer;
	importer.loadDataSet(fq(csvPath), ds, [](int){});

	ds->syncer().startFileSyncing(csvPath);
	QVERIFY(_pkg->synchingExternally());
	QVERIFY(!_pkg->manualEdits());

	Column * col = ds->column(0);
	QVERIFY(col);

	//A label is not data: renaming one must leave the synching alone.
	QVERIFY(col->setLabelDisplay(0, "a nicer label"));

	QVERIFY(!_pkg->manualEdits());
	QVERIFY(_pkg->synchingExternally());

	//Same for resetting the label filter.
	col->resetFilterAllows();

	QVERIFY(!_pkg->manualEdits());
	QVERIFY(_pkg->synchingExternally());

	ds->syncer().stopFileSyncing();
}

void TestAll::testStartFileSyncingDirectlyAlsoClearsManualEdits()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString csvPath = tempDir.filePath("regen.csv");
	QVERIFY(_writeTextFile(csvPath, "a,b\n1,2\n"));

	ds->syncer().startFileSyncing(csvPath);
	QVERIFY(_pkg->synchingExternally());

	const QModelIndex cell = ds->index(0, 0);
	QVERIFY(ds->setData(cell, QVariant("9"), int(dataPkgRoles::value)));

	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	//Turning the synching back on *directly on the syncer*, not through
	//DataSetPackage::setSynchingExternally, has to clear the manual-edits flag as well. That is what
	//FileMenu::setCurrentDataFile does when a generated data file lands (and FileMenu::
	//setDataFileWatcher(true) on the way back). A stale flag would block the next edit from switching
	//the synching off again: DataSet::setManualEdits(true) would see no change and return early.
	ds->syncer().startFileSyncing(csvPath);

	QVERIFY(_pkg->synchingExternally());
	QVERIFY(!_pkg->manualEdits());

	//...so a next hand edit must again switch the synching off, instead of leaving it on while the
	//data diverges from the file that the watcher will then sync over.
	QVERIFY(ds->setData(cell, QVariant("8"), int(dataPkgRoles::value)));

	QVERIFY(_pkg->manualEdits());
	QVERIFY(!_pkg->synchingExternally());

	ds->syncer().stopFileSyncing();
}

void TestAll::testSynchingExternallyRequiresWatcher()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	QTemporaryDir tempDir;
	QVERIFY(tempDir.isValid());
	const QString csvPath = tempDir.filePath("stale_flag.csv");
	QVERIFY(_writeTextFile(csvPath, "a,b\n1,2\n"));

	//What loading a workspace that was saved *while synching* looks like: the dataFileSynch flag
	//comes back from the database, but the file watcher died with the previous session and is only
	//re-registered on open (MainWindow::fileEventRequestFinalize). Until then the ribbon must not
	//claim the data file is leading - nothing is watching it.
	ds->setDataFile(fq(csvPath));
	ds->setDataFileSynch(true);

	QVERIFY(ds->dataFileSynch());
	QVERIFY(!ds->syncer().isFileSyncing());
	QVERIFY(!_pkg->synchingExternally());

	//Re-arming the watcher makes the answer honest again...
	ds->syncer().startFileSyncing(csvPath);
	QVERIFY(_pkg->synchingExternally());

	//...and stopping it takes the flag out of the picture entirely.
	ds->syncer().stopFileSyncing();
	QVERIFY(!_pkg->synchingExternally());
}

void TestAll::testUndoChangedSurvivesWorkspaceRecreation()
{
	_pkg = new DataSetPackage(this);

	//Stands in for the one model behind the data view: created once, while the first workspace exists.
	ExpandDataProxyModel model(this);

	//Loading data throws that workspace away and makes a new one, just like opening a data file does.
	_pkg->reset();

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);
	QCOMPARE(UndoStack::singleton(), ds->undoStack());

	QSignalSpy undoSpy(&model, &ExpandDataProxyModel::undoChanged);

	UndoModelCommand * command = new UndoModelCommand(ds);
	command->setText("test edit");
	UndoStack::singleton()->push(command);

	//The ribbon's Undo/Redo buttons only ever update on this signal.
	QCOMPARE(undoSpy.count(), 1);
	QCOMPARE(model.undoText(), QString("test edit"));

	//And tearing the workspace down must not leave the singleton pointing at a freed stack.
	_pkg->deleteWorkspace();
	QVERIFY(!UndoStack::singleton());
	QCOMPARE(model.undoText(), QString());
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
	QVERIFY(a->defaultFilter());

	const std::string colName	= a->column(0)->name();
	const std::string prefixA	= "JASPColumn_D" + std::to_string(a->id()) + "_F" + std::to_string(a->defaultFilter()->id()) + "_";

	ColumnEncoder * viewA = a->defaultFilter()->columnEncoder();
	QVERIFY2(viewA, "a live filter must own its namespace view (mds-plan C3)");
	const std::string encodedA = viewA->encode(colName);

	QVERIFY2(encodedA.find(prefixA) == 0,	qPrintable(tq("Filter view prefix must carry the dataset AND filter id, got: " + encodedA)));
	QVERIFY2(encodedA.size() > prefixA.size() + 8 && encodedA.compare(encodedA.size() - 8, 8, "_Encoded") == 0,	qPrintable(tq("tokens end in the _Encoded postfix, got: " + encodedA)));
	QVERIFY2(encodedA.find("-1") == std::string::npos,	qPrintable("Encoder prefix must not be the -1 sentinel"));

	//The canonical holds the mint but mints no tokens itself (C2): it answers membership, encode throws.
	QVERIFY2(a->encoder().shouldEncode(colName), "the canonical must know its own column names");
	bool canonicalEncodeThrew = false;
	try { a->encoder().encode(colName); } catch(const std::runtime_error &) { canonicalEncodeThrew = true; }
	QVERIFY2(canonicalEncodeThrew, "the canonical must never answer full-string encode");

	//Reload from the DB (the .jasp restore path): dataset AND filter rowids are stable, so the
	//reloaded dataset's default-filter view mints the exact same tokens (see
	//testDataSetFilterIdsSurviveStorageReload for the id-persistence pin).
	DataSet loadMe(nullptr, a->id());
	QCOMPARE(loadMe.id(), a->id());
	QCOMPARE(loadMe.defaultFilter()->id(), a->defaultFilter()->id());
	const std::string encodedReload = loadMe.defaultFilter()->columnEncoder()->encode(colName);
	QCOMPARE(encodedReload, encodedA);

	//A second dataset with a colliding column name must get a distinct prefix.
	CSVImporter importer;
	DataSet * b = _pkg->workspace()->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	const std::string prefixB	= "JASPColumn_D" + std::to_string(b->id()) + "_F" + std::to_string(b->defaultFilter()->id()) + "_";
	const std::string encodedB	= b->defaultFilter()->columnEncoder()->encode(b->column(0)->name());
	QVERIFY2(encodedB.find(prefixB) == 0,	qPrintable("Second dataset must get its own id-based prefix"));
	QVERIFY(encodedB != encodedA);

	//Decoding is context-free since mds-plan C4: the ONE global table holds a block per live filter
	//of every loaded dataset, so a single sweep resolves tokens of both datasets (the old
	//instance-rooted decodeJson only saw its own dataset).
	Json::Value json;
	json["axis"]	= encodedA;
	json["series"]	= encodedB;
	ColumnEncoder::decodeJson(json);
	QCOMPARE(json["axis"].asString(),	colName);
	QCOMPARE(json["series"].asString(),	b->column(0)->name());
}

void TestAll::testDataSetFilterDropDownList()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a);

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	a->addFilter(); //"Filter 0", shown on a (b stays the shown dataset)
	const int aExtraFilterId = a->shownFilter()->id();
	QVERIFY(a->defaultFilter());
	QVERIFY(aExtraFilterId != a->defaultFilter()->id());
	QVERIFY(b->defaultFilter());

	const QVariantList entries = ws->dataSetFilterDropDownList();

	QCOMPARE(entries.size(), 3); //a-default, a-"Filter 0", b-default

	for(const QVariant & entryVar : entries)
	{
		const QVariantMap entry = entryVar.toMap();
		const QString value		= entry["value"].toString();
		const QString label		= entry["label"].toString();

		Filter * f = ws->filterById(value.toInt());
		QVERIFY2(f, qPrintable("Every dropdown value must be a (workspace-wide unique) filterId, got: " + value));
		QCOMPARE(label, f->data()->title() + " - " + f->title());
	}

	QSet<QString> values;
	for(const QVariant & entryVar : entries)
		values.insert(entryVar.toMap()["value"].toString());

	QVERIFY(values.contains(QString::number(a->defaultFilter()->id())));
	QVERIFY(values.contains(QString::number(aExtraFilterId)));
	QVERIFY(values.contains(QString::number(b->defaultFilter()->id())));

	//The computed-dataset input list keeps excluding the shown dataset; the selection list never does:
	for(const QVariant & entryVar : ws->inputFilterDropDownList())
	{
		Filter * f = ws->filterById(entryVar.toMap()["value"].toString().toInt());
		QVERIFY(f);
		QVERIFY2(f->data()->id() != ws->shownDataSetId(), "inputFilterDropDownList must keep excluding the shown dataset");
	}
}

void TestAll::testPerDataSetEncodingUsesOwnDatasetEncoder()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a && a->columnCount() > 0);

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	//Same csv, so the very same column exists in both datasets (contNormal is a scale column):
	const std::string colName	= "contNormal";
	Column			*	colInA	= a->column(colName);
	Column			*	colInB	= b->column(colName);
	QVERIFY2(colInA, qPrintable(tq("First dataset should have column " + colName)));
	QVERIFY2(colInB, qPrintable(tq("Second dataset should have column " + colName)));

	const std::string qualified	= colName + "." + columnTypeToString(colInB->type());

	//The engine always refreshes each dataset's canonical (with types!) via provideAndUpdateDataSet
	//before encoding against it; do the same here because the desktop import path may have left
	//untyped names.
	a->encoder().setCurrentNames(a->getColumnTypesMap());
	b->encoder().setCurrentNames(b->getColumnTypesMap());

	//Encode through the shown filter's VIEW (mds-plan C3): the canonical itself mints no tokens.
	ColumnEncoder * viewA = a->shownEncoder(), * viewB = b->shownEncoder();
	QVERIFY(viewA && viewB);

	Json::Value options(Json::objectValue);

	options["dependent"]					= Json::Value(Json::objectValue);
	options["dependent"]["value"].append(colName);
	options["dependent"]["types"].append(columnTypeToString(colInA->type()));
	options["covariate"]					= Json::Value(Json::objectValue);
	options["covariate"]["value"].append(colName);
	options["covariate"]["types"].append(columnTypeToString(colInB->type()));

	options[".meta"]["dependent"]["shouldEncode"]	= true;
	options[".meta"]["dependent"]["dataSetId"]		= a->id();
	options[".meta"]["dependent"]["filterId"]		= a->shownFilter()->id();
	options[".meta"]["covariate"]["shouldEncode"]	= true;
	options[".meta"]["covariate"]["dataSetId"]		= b->id();
	options[".meta"]["covariate"]["filterId"]		= b->shownFilter()->id();

	ColumnEncoder::perDataSetColsPlusTypes colsPerDataSet = ColumnEncoder::encodeColumnNamesinOptionsPerDataSet(
			options, true,
			[ws](int dataSetId, int) -> ColumnEncoder *
			{
				//Encode against the dataset's shown-filter VIEW (mds-plan C3) - exactly what the
				//D1.6 resolver falls back to for un-stamped options; the stamps here ARE the shown
				//filters, so stamp-resolution would return the very same views.
				DataSet * ds = ws->dataSetById(dataSetId);
				return ds ? ds->shownEncoder() : nullptr;
			},
			a->id());

	QCOMPARE(colsPerDataSet.size(), size_t(2));
	QVERIFY(colsPerDataSet.count(a->id()) == 1);
	QVERIFY(colsPerDataSet.count(b->id()) == 1);

	const std::string encodedInA = viewA->encode(qualified),
					encodedInB = viewB->encode(qualified);

	QVERIFY2(encodedInA != encodedInB, "The same column in two datasets must not encode to the same name");
	QCOMPARE(QString::fromStdString(options["dependent"][0].asString()),	QString::fromStdString(encodedInA));
	QCOMPARE(QString::fromStdString(options["covariate"][0].asString()),	QString::fromStdString(encodedInB));

	//Decoding is context-free since mds-plan C4: ONE sweep of the global table resolves both
	//datasets' slices - there is no instance-rooted decode left to route around (D1.5):
	Json::Value fromA(encodedInA), fromB(encodedInB);
	ColumnEncoder::decodeJson(fromA);
	ColumnEncoder::decodeJson(fromB);
	QCOMPARE(QString::fromStdString(fromA.asString()), QString::fromStdString(colName));
	QCOMPARE(QString::fromStdString(fromB.asString()), QString::fromStdString(colName));

	//The old "another dataset's encoder must not decode this slice" pin is replaced by its
	//Phase-1 counterpart: the table resolving a foreign live token is exactly the point (C4)
	//- only STALE namespaces leave tokens raw.
	Json::Value wrongWay(encodedInA);
	ColumnEncoder::decodeJson(wrongWay);
	QCOMPARE(QString::fromStdString(wrongWay.asString()), QString::fromStdString(colName));

	std::string staleToken;
	{
		ColumnEncoder doomed("JASPColumn_D999999_F999999_");		//a namespace that lives only for a moment (transient filter analogue)
		doomed.setCurrentNames(a->getColumnTypesMap());
		staleToken = doomed.encode(qualified);
		QCOMPARE(ColumnEncoder::decode(staleToken), colName);
	}
	QCOMPARE(ColumnEncoder::decode(staleToken), staleToken);	//its block was erased with it (C16): raw

	// The encode -> decode -> encode -> decode cycle: decoding hands back the plain column name
	// (the ".type" qualifier is deliberately dropped - decode gives the name as it is in the data),
	// and that plain name re-encodes to the current type's encoded name, so the cycle is stable
	// from there on:
	const std::string decoded1	= ColumnEncoder::decode(encodedInA);
	QCOMPARE(QString::fromStdString(decoded1), QString::fromStdString(colName));

	const std::string again		= viewA->encode(decoded1);
	QCOMPARE(QString::fromStdString(again), QString::fromStdString(encodedInA));
	QCOMPARE(QString::fromStdString(ColumnEncoder::decode(again)), QString::fromStdString(colName));

	// And the emitted token format is dataset-AND-filter-namespaced (mds-plan C1), so results
	// decode sweeps need no routing table and deleted namespaces simply stop answering:
	QVERIFY2(encodedInA.rfind("JASPColumn_D" + std::to_string(a->id()) + "_F" + std::to_string(a->shownFilter()->id()) + "_", 0) == 0,
	         qPrintable(tq("encoded names must start with JASPColumn_D<dataSetId>_F<filterId>_, got: " + encodedInA)));
	QVERIFY2(encodedInA.size() > 8 && encodedInA.compare(encodedInA.size() - 8, 8, "_Encoded") == 0,
	         "encoded names must end in the _Encoded postfix");
}

void TestAll::testEncoderMutationFunnel()
{
	//Every mutation class funnels into the canonical mint (setCurrentNames / updateColumnTypesOnly)
	//or the view (register/erase), and the ONE global decode table must reflect exactly the current
	//state after each - no caller has to remember a second step (mds-plan D1.4).
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a && a->columnCount() > 1);
	QVERIFY(a->defaultFilter());

	ColumnEncoder * viewOne = a->defaultFilter()->columnEncoder();
	QVERIFY(viewOne);

	const std::string colName = a->column(0)->name();

	//The app's bookkeeping path (setShownDataSet / provideAndUpdateDataSet do this after mutations):
	auto remint = [a]() { a->encoder().setCurrentNames(a->getColumnTypesMap()); };

	//Every token of a namespace must map back to a name the mint currently holds:
	auto tableMatchesMint = [a](ColumnEncoder * view)
	{
		const ColumnEncoder::colTypeMap & types = a->encoder().currentNames();

		for(const auto & nameType : types)
		{
			const std::string token = view->encode(nameType.first);
			QCOMPARE(QString::fromStdString(ColumnEncoder::decode(token)), QString::fromStdString(nameType.first));
		}
	};

	remint();
	tableMatchesMint(viewOne);

	// --- type-only change (Column::setType -> updateColumnTypesOnly): no token churn ------------
	Column * first = a->column(0);
	const columnType oldType = first->type();
	const columnType newType = oldType == columnType::scale ? columnType::nominal : columnType::scale;
	const std::string bareTokenBefore = viewOne->encode(colName);

	first->setType(newType);

	QCOMPARE(viewOne->encode(colName), viewOne->encode(colName + "." + columnTypeToString(newType)));
	QCOMPARE(ColumnEncoder::decode(bareTokenBefore), colName);	//the old token still resolves (nothing was re-minted)
	tableMatchesMint(viewOne);

	// --- filter create: a second live namespace on the same mint --------------------------------
	a->addFilter();												//"Filter 0", shown
	Filter * extra = a->shownFilter();
	QVERIFY2(extra && extra != a->defaultFilter(), "addFilter must show the new filter");
	ColumnEncoder * viewTwo = extra->columnEncoder();
	QVERIFY(viewTwo);

	const std::string tokenOne = viewOne->encode(colName),
					tokenTwo = viewTwo->encode(colName);

	QVERIFY2(tokenOne != tokenTwo, "the same name in two filters of one dataset must mint distinct tokens");
	QCOMPARE(ColumnEncoder::decode(tokenOne), colName);
	QCOMPARE(ColumnEncoder::decode(tokenTwo), colName);		//both decode, context-free (C4)
	tableMatchesMint(viewTwo);

	// --- column add: grow ------------------------------------------------------------------------
	QVERIFY(a->createColumn("FunnelFresh", columnType::scale));
	remint();
	QCOMPARE(ColumnEncoder::decode(viewOne->encode("FunnelFresh")), std::string("FunnelFresh"));
	QCOMPARE(ColumnEncoder::decode(viewTwo->encode("FunnelFresh")), std::string("FunnelFresh"));

	// --- column rename: the table maps current names only ----------------------------------------
	const std::string freshToken = viewOne->encode("FunnelFresh");
	Column * fresh = a->column("FunnelFresh");
	QVERIFY(fresh);
	fresh->setNameManually(tq("RenewedFun"));
	remint();

	QCOMPARE(ColumnEncoder::decode(viewOne->encode("RenewedFun")), std::string("RenewedFun"));
	QVERIFY2(ColumnEncoder::decode(freshToken) != "FunnelFresh",
			 "a renamed-away name must not survive anywhere in the table");
	tableMatchesMint(viewOne);

	// --- column remove: shrink --------------------------------------------------------------------
	const std::string renewedToken = viewOne->encode("RenewedFun");
	a->removeColumn("RenewedFun");
	remint();

	QVERIFY2(ColumnEncoder::decode(renewedToken) != "RenewedFun",
			 "a removed column must not stay decodable under its own name");
	QCOMPARE(ColumnEncoder::decode(tokenOne), colName);		//the untouched part of the space keeps working
	tableMatchesMint(viewOne);
	tableMatchesMint(viewTwo);

	// --- filter destroy: its block goes raw, everything else is untouched -------------------------
	a->removeFilter(extra);

	QCOMPARE(ColumnEncoder::decode(tokenTwo), tokenTwo);		//deleted filter's tokens go raw (C16)
	QVERIFY2(!ColumnEncoder::shouldDecode(tokenTwo), "the destroyed filter's namespace must be gone");
	QCOMPARE(ColumnEncoder::decode(tokenOne), colName);		//the default filter is unaffected
	tableMatchesMint(viewOne);

	// --- dataset unload: every block of the unloaded dataset goes raw, the others stay -------------
	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	b->encoder().setCurrentNames(b->getColumnTypesMap());
	QVERIFY(b->defaultFilter()->columnEncoder());
	const std::string tokenB = b->defaultFilter()->columnEncoder()->encode(b->column(0)->name());
	QCOMPARE(ColumnEncoder::decode(tokenB), b->column(0)->name());

	ws->setShownDataSet(b);
	ws->deleteShownDataSet();

	QCOMPARE(ColumnEncoder::decode(tokenB), tokenB);			//unloaded: raw
	QVERIFY2(!ColumnEncoder::shouldDecode(tokenB), "an unloaded dataset must leave nothing decodable");
	QCOMPARE(ColumnEncoder::decode(tokenOne), colName);		//dataset a keeps decoding
}

void TestAll::testDecodeSupersetSemanticsAcrossLiveNamespaces()
{
	//mds-plan D1.5/C4+C16: every decode site is the static entry, so a single sweep must resolve
	//tokens from ANY live namespace (any filter of any loaded dataset, plus extras) regardless of
	//which one the payload was minted against - and must leave everything that is NOT a live
	//namespace's token exactly raw. Superset for live namespaces, strict for dead ones.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a && a->columnCount() > 0);

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});

	//The provideAndUpdateDataSet remint (what the engine does per request), then a NAMED filter on
	//b so its view is a second live namespace on a second dataset while dataset a stays shown.
	a->encoder().setCurrentNames(a->getColumnTypesMap());
	b->encoder().setCurrentNames(b->getColumnTypesMap());
	b->addFilter();
	Filter * bNamed = b->shownFilter();
	QVERIFY(bNamed && bNamed != b->defaultFilter());
	b->encoder().setCurrentNames(b->getColumnTypesMap());

	//Extras (C6): a first-class block of the table, fed the way factorlevellistbase feeds it.
	ColumnEncoder extras("JaspExtraOptions_");
	Json::Value extrasMeta(Json::objectValue);
	extrasMeta[".meta"]["levels"]["encodeThis"] = "a factor level";
	extras.setCurrentNamesFromOptionsMeta(extrasMeta);

	const std::string tokenA	= a->shownEncoder()->encode(a->column(0)->name());
	const std::string tokenB	= bNamed->columnEncoder()->encode("contNormal");
	const std::string tokenEx	= extras.encode("a factor level");

	//A namespace that dies before the sweep (deleted filter analogue): captured while live, raw after.
	std::string tokenDeleted;
	{
		a->addFilter();						//shown on a
		Filter * doomed = a->shownFilter();
		QVERIFY(doomed && doomed != a->defaultFilter());
		a->encoder().setCurrentNames(a->getColumnTypesMap());
		tokenDeleted = doomed->columnEncoder()->encode(a->column(0)->name());
		QCOMPARE(ColumnEncoder::decode(tokenDeleted), a->column(0)->name());
		a->removeFilter(doomed);
	}

	const std::string tokenForeign	= "JASPColumn_D987654_F987654_0_Encoded";		//never minted anywhere

	// --- one static sweep resolves every live namespace (the superset), unknown stays raw ----------
	Json::Value payload(Json::objectValue);
	payload["fromShownDataset"]	= tokenA;
	payload["fromOtherFilter"]	= tokenB;
	payload["fromExtras"]		= tokenEx;
	payload["fromDeleted"]		= tokenDeleted;
	payload["neverMinted"]		= tokenForeign;
	ColumnEncoder::decodeJson(payload);

	QCOMPARE(payload["fromShownDataset"].asString(),	a->column(0)->name());
	QCOMPARE(payload["fromOtherFilter"].asString(),		std::string("contNormal"));
	QCOMPARE(payload["fromExtras"].asString(),			std::string("a factor level"));
	QCOMPARE(payload["fromDeleted"].asString(),			tokenDeleted);		//C16: deleted filter goes raw
	QCOMPARE(payload["neverMinted"].asString(),			tokenForeign);		//unknown goes raw

	// --- the scalar entries behave identically ------------------------------------------------------
	QCOMPARE(ColumnEncoder::decode(tokenB), std::string("contNormal"));		//a's decode of b's token: superset (C4)
	QCOMPARE(ColumnEncoder::decode(tokenForeign), tokenForeign);
	QVERIFY2(!ColumnEncoder::shouldDecode(tokenDeleted), "the destroyed filter's block must be gone");
	QCOMPARE(ColumnEncoder::decodeAll("see " + tokenEx + " and " + tokenForeign),
			std::string("see a factor level and " + tokenForeign));

	// --- and every converged results-site uses exactly these entries: the sweep the desktop results
	// decode performs (EngineRepresentation::checkForComputedColumns / Analysis::loadPlotlyJsonInResults
	// twins) resolves the extras token too - the gap C6 called out was this site decoding dataset-only.
	Json::Value resultsJson(Json::objectValue);
	resultsJson["columnName"] = tokenEx;
	ColumnEncoder::decodeJson(resultsJson);
	QCOMPARE(resultsJson["columnName"].asString(), std::string("a factor level"));
}

void TestAll::testComputedColumnSiteDecodesExtrasTokens()
{
	//mds-plan C6 called out the one site that decoded dataset-only: EngineRepresentation's
	//computed-column reporting (enginerepresentation.cpp). With D1.5 every site is the static
	//entry, so a columnName that is an EXTRAS token (not a dataset token) resolves too. The
	//observable: the site only reaches Workspace::computedColumnSucceeded - which clears the
	//column error on the warning-less success path - when the reported name decodes to a real
	//column of the dataset.
	QVERIFY(_newPkgWithDataSet());

	DataSet * a = _pkg->workspace()->shownDataSet();
	QVERIFY(a);

	Column * col = a->createComputedColumn("ExtraReported", columnType::scale);
	QVERIFY(col);
	QVERIFY(col->setError("stale error"));

	std::string token;
	{
		ColumnEncoder extras("JaspExtraOptions_");
		Json::Value extrasMeta(Json::objectValue);
		extrasMeta[".meta"]["lv"]["encodeThis"] = "ExtraReported";	//the factorlevellistbase shape (C6)
		extras.setCurrentNamesFromOptionsMeta(extrasMeta);

		token = extras.encode("ExtraReported");
		QVERIFY2(token.rfind("JaspExtraOptions_", 0) == 0, qPrintable(tq("extras token expected, got: " + token)));

		Json::Value results(Json::objectValue);
		results["columnName"]	= token;
		results["columnType"]	= int(columnType::scale);
		results["dataChanged"]	= true;

		EngineRepresentation rep(0, new QProcess());	//no engine process needed for this site
		rep.checkForComputedColumns(a, results);
	}

	QCOMPARE(QString::fromStdString(col->error()), QString());		//decoded through the extras block

	//Negative control (C16 at this very site): with the extras encoder dead the token is raw, no
	//column matches it and the site leaves the error alone.
	QVERIFY(col->setError("stale error again"));

	Json::Value results(Json::objectValue);
	results["columnName"]	= token;
	results["columnType"]	= int(columnType::scale);
	results["dataChanged"]	= true;

	EngineRepresentation rep(0, new QProcess());
	rep.checkForComputedColumns(a, results);

	QCOMPARE(QString::fromStdString(col->error()), QString("stale error again"));
}

void TestAll::testMultiDataSetQueueHandout()
{
	MultiDataSetSliceQueue	queue;

	//No queue: nothing to hand out (legacy read-path):
	QVERIFY(queue.take() == nullptr);

	std::vector<MultiDataSetSliceQueue::Slice> slices;
	slices.push_back({ 11, 111, ColumnEncoder::colsPlusTypes() });
	slices.push_back({ 11, 112, ColumnEncoder::colsPlusTypes() });   //same dataset, other filter: still a slice of its own
	slices.push_back({ 12, 112, ColumnEncoder::colsPlusTypes() });
	slices.push_back({ 13,  13, ColumnEncoder::colsPlusTypes() });

	queue.set(std::move(slices));

	//Slices are handed out in order, once each, then the queue is empty again:
	const MultiDataSetSliceQueue::Slice * first = queue.take();
	QVERIFY(first);
	QCOMPARE(first->dataSetId, 11);
	QCOMPARE(first->filterId, 111);

	const MultiDataSetSliceQueue::Slice * second = queue.take();
	QVERIFY(second);
	QCOMPARE(second->dataSetId, 11);
	QCOMPARE(second->filterId, 112);

	const MultiDataSetSliceQueue::Slice * third = queue.take();
	QVERIFY(third);
	QCOMPARE(third->dataSetId, 12);

	const MultiDataSetSliceQueue::Slice * fourth = queue.take();
	QVERIFY(fourth);
	QCOMPARE(fourth->dataSetId, 13);

	QVERIFY(queue.take() == nullptr);

	//And a new request (empty queue) fully disables the multi-dataset read-path again:
	queue.set({});
	QVERIFY(queue.take() == nullptr);
}

void TestAll::testMultiDataSetQueueClearedAtRequestBoundary()
{
	//DataBridge-level pin of the request-boundary discipline: both Engine::receiveAnalysisMessage
	//(engine.cpp) and the entry of syntaxBridgeLoadQmlAndParseOptionsStatus clear the queue with
	//exactly setMultiDataSetQueue({}) before the request branches; the aware branch then refills it
	//through prepareMultiDataSetRun. This pins the fill (on a real workspace) and the reset, so an
	//aware parse that queued slices and an R side that died mid-consumption can never leak a stale
	//slice into the next request's dataset reads.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a);

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});

	DataBridge bridge(ProcessInfo::currentPID());	//borrows the process-wide workspace, just like the syntax bridge does in a host

	Json::Value options(Json::objectValue);
	options[".meta"]["dependent"]["dataSetId"]	= a->id();
	options[".meta"]["dependent"]["filterId"]	= a->shownFilter()->id();
	options[".meta"]["covariate"]["dataSetId"]	= b->id();
	options[".meta"]["covariate"]["filterId"]	= b->shownFilter()->id();

	const DataBridge::MultiDataSetRunPlan plan = bridge.prepareMultiDataSetRun(options, a->id(), "", "requestBoundaryTest");

	//Two distinct stamped filters (a's and b's shown/default one): two slices, handed out in order:
	QCOMPARE(plan.multiDataSetJson["ids"].size(), 2u);
	QVERIFY(bridge.takeMultiDataSetSlice() != nullptr);
	QVERIFY(bridge.takeMultiDataSetSlice() != nullptr);
	QVERIFY(bridge.takeMultiDataSetSlice() == nullptr);

	//Refill and abort mid-consumption (the leak scenario: aware parse, R error before every slice
	//was read), then run the request-boundary reset: the next request gets nothing, no matter what
	//the previous one left queued.
	bridge.prepareMultiDataSetRun(options, a->id(), "", "requestBoundaryTest");
	QVERIFY(bridge.takeMultiDataSetSlice() != nullptr);	//one slice consumed, one still queued
	bridge.setMultiDataSetQueue({});						//<- the exact call every bridge/engine request starts with
	QVERIFY(bridge.takeMultiDataSetSlice() == nullptr);
}

void TestAll::testPrepareMultiDataSetRunEncodesPerStampView()
{
	//mds-plan D1.6: prepareMultiDataSetRun resolves each option's encoder from its .meta stamp's
	//FILTER VIEW (not the shown encoder), with a dual-read fallback to the shown-filter view for
	//un-stamped options (C7). The landmine: the form's HOST filter differs from the shown dataset
	//filter - the stamped option must encode in the host's namespace, the un-stamped one in the
	//shown one, and the global table must decode both.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a && a->columnCount() > 0);

	//A second dataset so we can point the shown dataset elsewhere than a, proving the resolver
	//follows the stamp's filter, not whatever dataset/filter is currently shown.
	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->id() != a->id());

	a->addFilter();						//"Filter 0" on a, shown on a
	Filter * hostFilter = a->shownFilter();
	QVERIFY2(hostFilter && hostFilter != a->defaultFilter(), "addFilter must show the new host filter");
	const int hostFilterId = hostFilter->id();

	//The landmine proper: switch a's SHOWN filter back to the default. The form keeps the named
	//filter as its host (stamped on its options), while the dataset shows another filter from here
	//on - option encoding must follow the stamp, not the shown filter.
	a->showFilter(a->defaultFilter());

	//Now switch the workspace's shown dataset to b: a's host filter is neither the shown dataset
	//nor its shown filter, exactly the mixed-run scenario.
	ws->setShownDataSet(b);
	ColumnEncoder::setCurrentEncoder(b->shownEncoder());

	const std::string colName	= "contNormal";
	Column * colA = a->column(colName);
	QVERIFY2(colA && colA->type() != columnType::unknown, "debug.csv must give a typed contNormal in the first dataset too");
	const std::string qualified	= colName + "." + columnTypeToString(colA->type());

	//Remint a's canonical so both a-filters' blocks hold the current names (provideAndUpdateDataSet
	//does this per request; the resolver returns live views either way).
	a->encoder().setCurrentNames(a->getColumnTypesMap());

	const std::string hostToken		= hostFilter->columnEncoder()->encode(qualified);
	const std::string shownDefault	= a->defaultFilter()->columnEncoder()->encode(qualified);
	QVERIFY2(hostToken != shownDefault, "the host filter and the default filter must mint distinct tokens");

	Json::Value options(Json::objectValue);

	options["dependent"]				= Json::Value(Json::objectValue);
	options["dependent"]["value"]		= Json::Value(Json::arrayValue);
	options["dependent"]["value"].append(colName);
	options["dependent"]["types"]		= Json::Value(Json::arrayValue);
	options["dependent"]["types"].append(columnTypeToString(colA->type()));
	options["legacy"]					= Json::Value(Json::objectValue);
	options["legacy"]["value"]			= Json::Value(Json::arrayValue);
	options["legacy"]["value"].append(colName);
	options["legacy"]["types"]			= Json::Value(Json::arrayValue);
	options["legacy"]["types"].append(columnTypeToString(colA->type()));

	options[".meta"]["dependent"]["shouldEncode"]	= true;
	options[".meta"]["dependent"]["dataSetId"]		= a->id();
	options[".meta"]["dependent"]["filterId"]		= hostFilterId;		//the host filter, NOT the shown one
	options[".meta"]["legacy"]["shouldEncode"]		= true;
	options[".meta"]["legacy"]["dataSetId"]			= a->id();			//no filterId: dual-read -> shown fallback

	DataBridge bridge(ProcessInfo::currentPID());
	const DataBridge::MultiDataSetRunPlan plan = bridge.prepareMultiDataSetRun(options, a->id(), "", "perStampViewTest");

	//prepareMultiDataSetRun re-points currentEncoder at a's shown filter (default) as part of
	//provideAndUpdateDataSet(primaryDataSetId); the STAMPED option must still land in the host's
	//namespace, proving the resolver used the stamp's filter view, not the (just-changed) current.
	QCOMPARE(QString::fromStdString(options["dependent"][0].asString()), QString::fromStdString(hostToken));

	//The un-stamped option falls back to the shown filter's view of the primary dataset (default).
	QCOMPARE(QString::fromStdString(options["legacy"][0].asString()), QString::fromStdString(shownDefault));

	//The host filter must be in the slice queue (its stamp collected it):
	bool hostQueued = false;
	for(const Json::Value & id : plan.multiDataSetJson["ids"])
		if(id.asString() == std::to_string(hostFilterId))
			hostQueued = true;
	QVERIFY2(hostQueued, "the stamped host filter must be queued as a slice");

	//And the one global table decodes both namespaces back to the real name, context-free (C4):
	Json::Value sweep(Json::arrayValue);
	sweep.append(options["dependent"][0].asString());
	sweep.append(options["legacy"][0].asString());
	ColumnEncoder::decodeJson(sweep);
	QCOMPARE(sweep[0].asString(), colName);
	QCOMPARE(sweep[1].asString(), colName);
}

void TestAll::testLoadFilteredDataFilterFirstProvenance()
{
	//DataBridge::updateOptionsAccordingToMeta loadFilteredData block: the stamped filterId is the
	//anchor (globally unique, implies its dataset), dataSetId is verification-only. b is made
	//smaller than a: the old dataSetId-anchored path would index a's 99 rows against b's short
	//filter vector (unchecked out-of-bounds read), the filter-anchored one cannot.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a);

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	b->setRowCount(5);
	b->runFilters();

	Filter * bFilter = b->shownFilter();
	QVERIFY(bFilter);
	boolvec keepFive(5, true);
	keepFive[1] = false;						//rows 2 of 5 filtered out
	bFilter->setFilterVector(keepFive);

	QVERIFY(a->rowCount() > 5);					//the mismatched sizes the old code would run past
	QCOMPARE(b->rowCount(), 5);

	DataBridge bridge(ProcessInfo::currentPID());

	Json::Value options(Json::objectValue);
	options["filtered"] = Json::Value(Json::objectValue);
	options[".meta"]["filtered"]["loadFilteredData"]["column"]	= a->column(0)->name();
	options[".meta"]["filtered"]["loadFilteredData"]["filter"]	= "NoSuchFilterName";	//forces the provenance path

	//1) Cross-dataset provenance (dataSetId names a, filterId lives in b): trust the filter, anchor
	//on b. The old code anchored on a, failed the name lookup and filtered a's rows with b's short
	//vector; the new code never mixes the two (no OOB read, values computed over b).
	options[".meta"]["filtered"]["dataSetId"]	= a->id();
	options[".meta"]["filtered"]["filterId"]	= bFilter->id();
	bridge.updateOptionsAccordingToMeta(options);
	QCOMPARE(options["filtered"]["rowIndices"].size(), 4u);	//5 rows of b minus the one b filters out

	//2) Dead dataSetId, live filterId (agent-1 m1 scenario): re-anchor the request at a so the old
	//dataSetId-first path would keep a; the filterId must still resolve to b.
	ws->setShownDataSet(a->id());
	options["filtered"] = Json::Value(Json::objectValue);
	options[".meta"]["filtered"]["dataSetId"]	= 987654;
	bridge.updateOptionsAccordingToMeta(options);
	QCOMPARE(options["filtered"]["rowIndices"].size(), 4u);
	QCOMPARE(options["filtered"]["values"].size(), 4u);

	//3) Legacy option: no filterId stamped at all -> the old name-based path, unchanged.
	Filter * aFilter = a->createFilter("keepAllButFirst", true);
	QVERIFY(aFilter);
	boolvec keepN(a->rowCount(), true);
	keepN[0] = false;
	aFilter->setFilterVector(keepN);

	options["filtered"] = Json::Value(Json::objectValue);
	options[".meta"]["filtered"]["loadFilteredData"]["filter"]	= "keepAllButFirst";
	options[".meta"]["filtered"]["dataSetId"]					= a->id();
	options[".meta"]["filtered"].removeMember("filterId");
	bridge.updateOptionsAccordingToMeta(options);
	QCOMPARE(options["filtered"]["rowIndices"].size(), static_cast<Json::ArrayIndex>(a->rowCount()) - 1);
	QCOMPARE(options["filtered"]["rowIndices"][0].asInt(), 2);	//row 1 filtered out, so it starts at 2
}

void TestAll::testPrepareMultiDataSetRunPrimarySliceKey()
{
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a);
	QVERIFY(a->defaultFilter());

	DataBridge bridge(ProcessInfo::currentPID());
	Json::Value options(Json::objectValue);

	//A primary dataset with its default filter: "primary" carries that filter id and is a real key
	//of the queued ids.
	const DataBridge::MultiDataSetRunPlan withData = bridge.prepareMultiDataSetRun(options, a->id(), "", "primarySliceKeyTest");
	const std::string primary = std::to_string(a->defaultFilter()->id());

	QVERIFY2(withData.multiDataSetJson.isMember("primary"), "primary must be present when a primary filter exists");
	QCOMPARE(withData.multiDataSetJson["primary"].asString(), primary);
	QCOMPARE(withData.multiDataSetJson["ids"][0].asString(), primary);

	//No dataset at all (empty workspace): there is no primary filter, so the member must be ABSENT,
	//not "". jaspBase's runJaspResults keys on !is.null: an "" would surface as a non-NULL
	//primarySliceKey attribute that is not a key of any slice.
	while (ws->shownDataSet())
		ws->deleteShownDataSet();

	const DataBridge::MultiDataSetRunPlan empty = bridge.prepareMultiDataSetRun(options, -1, "", "primarySliceKeyTest");
	QVERIFY2(!empty.multiDataSetJson.isMember("primary"), "no primary filter: the primary member must be absent, not empty");
	QCOMPARE(empty.multiDataSetJson["ids"].size(), 0u);
}

void TestAll::testAnalysisBaseUsesDataSetWhenAware()
{
	AnalysisBase analysis;

	Json::Value boundValues(Json::objectValue);
	boundValues["dependent"] = "Score";

	Json::Value meta(Json::objectValue);
	meta["dependent"]["dataSetId"]	= 7;
	meta["dependent"]["filterId"]	= 71;
	boundValues[".meta"] = meta;

	analysis.setBoundValues(boundValues);

	//Not aware and unbound: the legacy "applies to any dataset" semantics (reports etc.):
	QCOMPARE(analysis.usesDataSet(7), true);

	//Aware: exactly the datasets referenced through the options are in use, and every other is
	//not - the "unbound applies to all" shortcut must not swallow that (C: guards).
	analysis.setMultiDataSetAware(true);
	QCOMPARE(analysis.usesDataSet(7), true);
	QCOMPARE(analysis.usesDataSet(42), false);
}

void TestAll::testAnalysisBaseDataSpecWhenAware()
{
	//C3/M7: dataSpec() for a multi-dataset aware analysis must come from the datasets its bound
	//values reference, not from dataSet() - which an aware analysis deliberately refuses (and
	//Desktop::Analysis logs about). Before this the aware branch was dead code: the early return
	//on !data already yielded "", so the results header showed nothing about the data.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);
	DataSet * a = ws->shownDataSet();
	QVERIFY(a);
	QVERIFY(a->defaultFilter());

	CSVImporter importer;
	DataSet * b = ws->createDataSet();
	QVERIFY(b);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), b, [](int){});
	QVERIFY(b->defaultFilter());

	AnalysisBase analysis;

	Json::Value boundValues(Json::objectValue);
	boundValues["dependent"]	= "x";
	boundValues["covariate"]	= "y";
	boundValues["extra"]		= "z";

	Json::Value meta(Json::objectValue);
	meta["dependent"]["dataSetId"]		= a->id();
	meta["dependent"]["filterId"]		= a->defaultFilter()->id();
	meta["dependent"]["shouldEncode"]	= true;
	meta["covariate"]["dataSetId"]		= b->id();
	meta["covariate"]["filterId"]		= b->defaultFilter()->id();
	meta["covariate"]["shouldEncode"]	= true;
	meta["extra"]["dataSetId"]			= a->id();				//second stamp of the same dataset: deduped
	meta["extra"]["filterId"]			= a->defaultFilter()->id();
	meta["extra"]["shouldEncode"]		= true;
	boundValues[".meta"] = meta;

	analysis.setBoundValues(boundValues);

	//Not aware: bit-identical legacy behaviour - the base dataSet() is nullptr, so is the spec
	//(the aware branch must not change anything on this path, not even call dataSet()):
	QCOMPARE(analysis.dataSpec(), QString(""));

	//Aware: one entry per referenced dataset (deduped), in id order, " + " joined:
	analysis.setMultiDataSetAware(true);
	const QString spec = analysis.dataSpec();

	QVERIFY2(spec == a->title() + " + " + b->title(),
			 qPrintable(QString("dataSpec should name both referenced datasets, got: '%1'").arg(spec)));

	//A reference that resolves to no dataset anymore contributes nothing (and must not print a
	//bare separator):
	Json::Value dead = meta;
	dead["gone"]["dataSetId"]	= 987654;
	dead["gone"]["filterId"]	= 987655;
	boundValues[".meta"]		= dead;
	analysis.setBoundValues(boundValues);

	QCOMPARE(analysis.dataSpec(), a->title() + " + " + b->title());

	//And with nothing referenced at all the spec is simply empty:
	analysis.setBoundValues(Json::Value(Json::objectValue));
	QCOMPARE(analysis.dataSpec(), QString(""));
}

void TestAll::testAnalysisBaseReferencedDataSets()
{
	AnalysisBase analysis;

	Json::Value boundValues(Json::objectValue);
	boundValues["dependent"] = "Score.scale";

	Json::Value meta(Json::objectValue);
	meta["dependent"]["shouldEncode"]	= true;
	meta["dependent"]["dataSetId"]		= 7;
	meta["dependent"]["filterId"]		= 71;
	meta["nested"]["group"]["dataSetId"]	= 9;   //no filterId here
	boundValues[".meta"] = meta;

	analysis.setBoundValues(boundValues);

	std::map<int, int> refs = analysis.referencedDataSets();

	QCOMPARE(refs.size(), size_t(2));
	QCOMPARE(refs[7], 71);
	QCOMPARE(refs[9], -1);

	//The plain base class is never aware; Desktop::Analysis derives it from the module entry:
	QCOMPARE(analysis.multiDataSetAware(), false);
}

void TestAll::testDataSetFilterIdsSurviveStorageReload()
{
	//Opening a .jasp restores internal.sqlite verbatim (ArchiveReader) and Workspace::dbLoad
	//enumerates db().dataSetIds(), calling DataSet::dbLoad(id) per dataset - ids are rowids of
	//the restored file, not reassigned. This pins that invariant (the premise on which
	//Analyses::remapSavedProvenance was removed): a reload hands back identical dataset and
	//filter ids, so provenance saved against them resolves without any remapping.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);

	CSVImporter importer;
	DataSet * first = ws->shownDataSet();
	QVERIFY(first);
	DataSet * second = ws->createDataSet();
	QVERIFY(second);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), second, [](int){});
	Filter * named = second->createFilter("KeepMyId", true);
	QVERIFY(named);

	const int idFirst	= first->id(),
			  idSecond	= second->id(),
			  defFirst	= first->defaultFilter()->id(),
			  defSecond = second->defaultFilter()->id(),
			  namedId	= named->id();

	QVERIFY(idFirst != idSecond);

	//Drop the package (its workspace owns the Workspace singleton) and reopen the same storage
	//the way opening a .jasp does: a fresh Workspace reading the (restored) database.
	delete _pkg;
	_pkg = nullptr;

	Workspace reopen;
	reopen.dbLoad();

	DataSet * reFirst	= reopen.dataSetById(idFirst);
	DataSet * reSecond	= reopen.dataSetById(idSecond);
	QVERIFY(reFirst);
	QVERIFY(reSecond);

	QCOMPARE(reFirst->defaultFilter()->id(),	defFirst);
	QCOMPARE(reSecond->defaultFilter()->id(),	defSecond);

	//C2/EAGER: dbLoad materialises EVERY stored filter while the file opens - no showFilter
	//priming, no lazy attach needed (agent1 m4/m5). The row is attached to, not replaced by,
	//exactly the id saved provenance refers to.
	QCOMPARE(DatabaseInterface::singleton()->filterGetId(idSecond, "KeepMyId"), namedId);
	QVERIFY2(reSecond->filter(namedId) != nullptr, "the named filter must be live right after reopen (eager dbLoad)");
	QVERIFY2(reSecond->filter(fq("KeepMyId")) != nullptr, "the named filter must be live by name too");
	QCOMPARE(reSecond->filters().size(), size_t(2));	//default + KeepMyId, attached at their old ids
	QCOMPARE(reSecond->filter(fq("KeepMyId"))->id(), namedId);
	QCOMPARE(reopen.filterById(namedId), reSecond->filter(namedId));

	//The dropdown the aware selection offers lists live filters only - after the eager load it
	//contains the reopened named filter, so it is selectable again (agent1 m4).
	bool namedInDropDown = false;
	for(const QVariant & entry : reopen.dataSetFilterDropDownList())
		if(entry.toMap().value("value").toString() == QString::number(namedId))
			namedInDropDown = true;
	QVERIFY2(namedInDropDown, "dataSetFilterDropDownList must offer the reopened named filter");

	//A selection option value (a filter id as string) resolves to the very same filter in the
	//reopened workspace - which is all save/load round-tripping needs from these ids.
	Filter * reNamed = reSecond->showFilter(fq("KeepMyId"));
	QVERIFY(reNamed);
	QCOMPARE(reNamed->id(), namedId);
	QVERIFY(reopen.filterById(namedId) == reNamed);

	//The lazy db-fallback of Workspace::filterById stays as safety net for rows that never went
	//through dbLoad (e.g. written by another process): a bare db row, no live object, must still
	//materialise to exactly its own id (this half was unpinned before - agent1 m5).
	const int rawId = DatabaseInterface::singleton()->filterInsert(reFirst->id(), "TRUE", DEFAULT_FILTER_GEN, "{}", "", "RawRowFilter");
	QVERIFY(rawId > 0);
	QVERIFY2(reFirst->filter(rawId) == nullptr, "a raw db row has no live filter yet");
	Filter * lazily = reopen.filterById(rawId);
	QVERIFY(lazily != nullptr);
	QCOMPARE(lazily->id(), rawId);
	QVERIFY(lazily->data() == reFirst);
}

void TestAll::testDataSetSelectionSurfaceMap()
{
	//mds-plan C4/RESOLVER: option values stay filter ids on the wire, but syntax text carries
	//human strings only and resolves by EXACT lookup against the live workspace - no parsing,
	//no numeric-id path. Fixture: "Alpha" (default only), "Second" (+ named "KeepMyId"),
	//a dataset titled "42" (a number is a title here, nothing more) and one titled exactly
	//like another pair's dropdown label (the collision the precedence rule is about).
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);

	CSVImporter importer;
	const QString csv = _testLibrary().absoluteFilePath("csv/debug.csv");

	DataSet * alpha = ws->shownDataSet();
	QVERIFY(alpha && alpha->defaultFilter());
	alpha->setTitle("Alpha");

	DataSet * second = ws->createDataSet();
	QVERIFY(second);
	importer.loadDataSet(fq(csv), second, [](int){});
	second->setTitle("Second");
	Filter * named = second->createFilter("KeepMyId", true);
	QVERIFY(named);

	DataSet * numeric = ws->createDataSet();
	QVERIFY(numeric);
	importer.loadDataSet(fq(csv), numeric, [](int){});
	numeric->setTitle("42");

	DataSet * shadow = ws->createDataSet();
	QVERIFY(shadow);
	importer.loadDataSet(fq(csv), shadow, [](int){});
	shadow->setTitle("Second - KeepMyId");   //equal to second's named-filter label

	const QMap<QString, int> surface = ws->datasetSelectionSurfaceMap();

	//Bare title -> default filter; dropdown label -> its filter:
	QCOMPARE(surface.value("Alpha"), alpha->defaultFilter()->id());
	QCOMPARE(surface.value("Second"), second->defaultFilter()->id());
	QCOMPARE(surface.value("Second - KeepMyId"), shadow->defaultFilter()->id());   //bare title WINS the collision (logged)
	QCOMPARE(surface.value("42"), numeric->defaultFilter()->id());                 //a number is only ever a title

	//The exact-match resolver agrees with the map, and there is no id path: "1" resolves only
	//if a dataset is titled "1" (none is), so a dataset/filter id written into syntax fails:
	int		 filterId	= -1;
	QString  error;

	QVERIFY2(ws->resolveDataSetSelectionValue("dataSetA", Json::Value("Second - KeepMyId"), filterId, error) && filterId == shadow->defaultFilter()->id(),
			 qPrintable(QString("the shadowing title must resolve to its own dataset, got %1 (%2)").arg(filterId).arg(error)));
	QVERIFY2(ws->resolveDataSetSelectionValue("dataSetA", Json::Value("Second - " + fq(second->defaultFilter()->title())), filterId, error) && filterId == second->defaultFilter()->id(),
			 qPrintable(QString("the default filter's own label must resolve too, got %1 (%2)").arg(filterId).arg(error)));

	filterId = 12345;
	QVERIFY2(!ws->resolveDataSetSelectionValue("dataSetA", Json::Value(std::to_string(alpha->defaultFilter()->id())), filterId, error) && filterId == -1,
			 "a bare filter/dataset id is not syntax anymore (M4): the numeric-id acceptance is gone");
	QVERIFY2(error.contains("available:"), qPrintable(QString("the rejection must list what IS selectable, got: %1").arg(error)));
	QVERIFY2(error.contains("Alpha") && error.contains("Second - KeepMyId"), qPrintable(QString("the listing must name the datasets, got: %1").arg(error)));

	//Desktop-shaped {types,value}: a plain string/int inside is unwrapped, anything else is
	//rejected with the direct-title hint (M9's unhelpful error replaced):
	Json::Value shaped(Json::objectValue);
	shaped["types"]	= "unknown";
	shaped["value"] = "42";
	QVERIFY2(ws->resolveDataSetSelectionValue("dataSetA", shaped, filterId, error) && filterId == numeric->defaultFilter()->id(),
			 qPrintable(QString("{types,value} with a plain title inside must unwrap, got %1 (%2)").arg(filterId).arg(error)));

	shaped["value"] = 42;   //int is equally a plain value
	QVERIFY2(ws->resolveDataSetSelectionValue("dataSetA", shaped, filterId, error) && filterId == numeric->defaultFilter()->id(),
			 "{types,value} with a plain int inside must unwrap too");

	shaped["value"] = Json::Value(Json::arrayValue);
	shaped["value"].append("42");
	filterId = 999;
	QVERIFY2(!ws->resolveDataSetSelectionValue("dataSetA", shaped, filterId, error) && filterId == -1, "a non-plain value must be rejected");
	QVERIFY2(error.contains("pass the dataset title directly"), qPrintable(QString("the rejection must hint the surface contract, got: %1").arg(error)));

	Json::Value empty(Json::objectValue);
	empty["types"]	= "unknown";
	empty["value"]	= "";
	QVERIFY2(!ws->resolveDataSetSelectionValue("dataSetB", empty, filterId, error) && error.startsWith("The dataset selection option 'dataSetB'"),
			 "an empty selection fails loudly, naming the option");

	//C4/RENDER reverse direction: bound id -> surface string (bare for default, label for named,
	//empty for nothing/dead id - never an id leaking back into syntax text):
	QCOMPARE(ws->datasetSelectionSurface(alpha->defaultFilter()->id()), QString("Alpha"));
	QCOMPARE(ws->datasetSelectionSurface(second->defaultFilter()->id()), QString("Second"));
	QCOMPARE(ws->datasetSelectionSurface(-1),							 QString(""));
	QCOMPARE(ws->datasetSelectionSurface(987654),						 QString(""));

	const QString namedSurface = ws->datasetSelectionSurface(named->id());
	QCOMPARE(namedSurface, QString("Second - KeepMyId"));   //the second dataset's own named filter

	//The map is temporary BY DESIGN: always built from the live workspace, so a rename needs no
	//cache invalidation anywhere (C4/LANG leans on this):
	alpha->setTitle("Alpha2");
	QVERIFY2(ws->resolveDataSetSelectionValue("dataSetA", Json::Value("Alpha2"), filterId, error) && filterId == alpha->defaultFilter()->id(),
			 "a renamed dataset resolves under its new title immediately");
	QVERIFY2(!ws->resolveDataSetSelectionValue("dataSetA", Json::Value("Alpha"), filterId, error),
			 "the old title is gone from the surface immediately");
}

void TestAll::testSelectionRegistryLateHostAppliesPendingValue()
{
	//mds-plan C4/REGISTRY (M9): the bridge hands the raw options JSON of the current parse to
	//the form; a selection host that registers after the parse (async ComponentsList
	//instantiation) resolves its own option out of that pending JSON and writes the resulting
	//filter id into the bound values at registration time, before its DropDown ever binds.
	//Driven through the C++ API here; the QML lifecycle side (register on late instantiation,
	//prune on destroy) is pinned in tst_dataSetSelectionVariablesList.qml.
	QVERIFY(_newPkgWithDataSet());

	Workspace * ws = _pkg->workspace();
	QVERIFY(ws);

	CSVImporter importer;
	DataSet * second = ws->createDataSet();
	QVERIFY(second);
	importer.loadDataSet(fq(_testLibrary().absoluteFilePath("csv/debug.csv")), second, [](int){});
	second->setTitle("Second");
	Filter * named = second->createFilter("KeepMyId", true);
	QVERIFY(named);

	//The AnalysisForm ctor connects to PreferencesModelBase::preferences(); let it lazily create
	//a PLAIN base there and the PreferencesModel-based tests later in this binary could no longer
	//install their own model (the base ctor asserts on an existing singleton). Hand them the
	//guarded real one, exactly like testSyncKeepMissingColumns does:
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);

	AnalysisForm form;
	AnalysisBase analysis(&form);   //parenting is how AnalysisBase picks up _analysisForm
	form.setAnalysis(&analysis);
	Json::Value raw(Json::objectValue);
	raw["dataSetA"]	= "Second - KeepMyId";
	raw["dataSetB"]	= "NoSuchDataSet";
	form.setPendingDataSetSelectionRawValues(raw);

	//Nothing registered yet, nothing applied:
	QVERIFY(form.dataSetSelectionOptionNames().isEmpty());
	QVERIFY(!analysis.boundValues().isMember("dataSetA"));

	JASPControl * earlyHost = new JASPControl();
	form.registerDataSetSelectionOption("dataSetA", earlyHost);

	QCOMPARE(form.dataSetSelectionOptionNames(), QStringList({ "dataSetA" }));
	QCOMPARE(analysis.boundValues()["dataSetA"].asString(), std::to_string(named->id()));   //resolved+applied at registration

	//A late host only ever touches its own option - the unresolvable dataSetB stays untouched
	//(its DropDown will hold whatever it gets pending, mds-plan C2) - but registers anyway, so
	//the NEXT parse translates it properly:
	JASPControl * laterHost = new JASPControl();
	form.registerDataSetSelectionOption("dataSetB", laterHost);
	QVERIFY(!analysis.boundValues().isMember("dataSetB"));
	QVERIFY(form.dataSetSelectionOptionNames().contains("dataSetB"));

	//Request boundary: a fresh parse replaces the pending JSON wholesale; re-registering the
	//same (host, option) is a no-op and nothing gets applied out of thin air:
	form.setPendingDataSetSelectionRawValues(Json::Value());
	form.registerDataSetSelectionOption("dataSetB", laterHost);
	QVERIFY(!analysis.boundValues().isMember("dataSetB"));

	//Dead hosts drop out at read time - the bridge never demands a value for an option nobody
	//owns anymore (the old findChildren scan got this half right, the registry must not regress
	//it):
	delete earlyHost;
	QStringList names = form.dataSetSelectionOptionNames();
	QVERIFY(!names.contains("dataSetA"));
	QVERIFY(names.contains("dataSetB"));

	delete laterHost;
	QVERIFY(form.dataSetSelectionOptionNames().isEmpty());
}

void TestAll::testDeadControlsArePrunedFromFormCaches()
{
	//mds-plan C5 (chunk-4 finding #1): JASPControl::componentComplete registers every
	//static-branch control on the form (AnalysisForm::addControl), but nothing unregistered -
	//destroying such a control after setup left raw pointers in _controls, and in
	//_dependsOrderedCtrls once a bind pass had run over them. The next pass (setAnalysisUp,
	//bindTo, clearFormErrors, filterByNameDone, usedVariables) dereferenced freed memory:
	//deterministic SEGV (reproduced in tst_dataSetSelectionVariablesList.qml pre-fix, which now
	//drives the same cycle through the real QML lifecycle). The form prunes on the control's
	//destroyed signal now, wired at the addControl chokepoint.
	if(!PreferencesModel::prefs())
		new PreferencesModel(this);   //AnalysisForm's ctor touches preferences(), same guard as testSelectionRegistryLateHostAppliesPendingValue

	AnalysisForm form;
	AnalysisBase analysis(&form);   //parenting is how AnalysisBase picks up _analysisForm
	form.setAnalysis(&analysis);

	JASPControl * keeper = new JASPControl();
	keeper->setName("keeper");
	form.addControl(keeper);

	JASPControl * doomed = new JASPControl();
	doomed->setName("doomed");
	form.addControl(doomed);

	QCOMPARE(form.getControl("keeper"), keeper);
	QCOMPARE(form.getControl("doomed"), doomed);

	//complete the form the way Form.qml's Component.onCompleted does: the setAnalysisUp pass
	//pushes every registered control into _dependsOrderedCtrls, so after this the doomed control
	//dangles in BOTH caches once destroyed.
	emit form.formCompletedSignal();
	QVERIFY2(form.initialized(), "the dummy analysis must have completed the setup pass");

	delete doomed;   //~QObject fires destroyed() - the form must drop it from every container

	QVERIFY2(!form.getControl("doomed"), "a destroyed control must not resolve from the form cache anymore");
	QCOMPARE(form.getControl("keeper"), keeper);

	//the passes that used to walk the freed pointers:
	form.clearFormErrors();
	form.clearFormWarnings();
	form.filterByNameDone(1, "someFilter", "");
	QVERIFY(form.usedVariables().empty());
	form.optionMeta(false);   //walks _dependsOrderedCtrls

	//the freed option name is reusable: the pruned entry must not resurface as a bogus
	//"2 controls have the same name" collision:
	JASPControl * revived = new JASPControl();
	revived->setName("doomed");
	form.addControl(revived);
	QCOMPARE(form.getControl("doomed"), revived);

	//and the full bind pass (setOptions -> setAnalysisUp -> _setUpModels/_setUp/bindTo) must
	//survive with the revived control, then with everything dead:
	form.setOptions(QVariantMap({ { "someOption", true } }));

	delete keeper;
	delete revived;
	form.clearFormErrors();
	QVERIFY(!form.getControl("keeper"));
	QVERIFY(!form.getControl("doomed"));
}

void TestAll::testTermFilterIdIsPartOfIdentity()
{
	//mds-plan D2.1/C9: Term carries the id of the filter it was selected through (-1 =
	//unqualified) as INTERNAL typed state, and it is part of identity. Flagship case: two terms
	//named "TestInts" from two different filters must never compare equal and must coexist in
	//one Terms; lookups distinguish stamped terms but keep the C13 fallback (an unqualified
	//lookup matches on name alone, so every pre-existing name-based flow keeps working).
	Term plain(tq("TestInts"), columnType::scale);
	QCOMPARE(plain.filterId(), -1);						//fresh terms are unqualified

	Term inA(plain);		inA.setFilterId(11);
	Term inAcopy(plain);	inAcopy.setFilterId(11);	//same everything, separately built
	Term inB(plain);		inB.setFilterId(12);

	QVERIFY(plain == Term(tq("TestInts"), columnType::scale));	//-1 == -1: legacy equality unchanged
	QVERIFY(inA   == inAcopy);
	QVERIFY(inA   != inB);									//the flagship: same name, other filter
	QVERIFY(inA   != plain);								//equality is strict, -1 included (C9):
	QVERIFY(plain != inA);									//qualified and unqualified are different facts

	//interaction terms: components compare as a set (always did), the term-level filterId must agree:
	Term interA(QStringList({"x", "y"}));		interA.setFilterId(11);
	Term interAotherOrder(QStringList({"y", "x"}));	interAotherOrder.setFilterId(11);
	Term interB(QStringList({"x", "y"}));			interB.setFilterId(12);
	QVERIFY(interA == interAotherOrder);
	QVERIFY(interA != interB);

	//operator< is a total order over the identity, so ordered containers keep both terms apart
	//(Terms::indexesFromTerms leans on this):
	QVERIFY(inA < inB);
	QVERIFY(!(inB < inA));
	QVERIFY(!(plain < plain));

	//qHash is consistent with operator==: QHash/QSet hold same-name terms of two filters apart:
	QHash<Term, int> byHash;
	byHash.insert(inA, 1);
	byHash.insert(inB, 2);
	QCOMPARE(byHash.size(), 2);
	QCOMPARE(byHash.value(inA), 1);
	QCOMPARE(byHash.value(inB), 2);

	//one Terms container, same name through two filters (the flagship containment):
	Terms terms;
	terms.add(inA);
	terms.add(inB);
	QCOMPARE(terms.size(), size_t(2));				//both survive the uniqueness check
	QVERIFY(terms.containsValue(inA));
	QVERIFY(terms.containsValue(inB));
	QVERIFY(terms.indexOfValue(inA) >= 0 && terms.indexOfValue(inB) >= 0);
	QVERIFY(terms.indexOfValue(inA) != terms.indexOfValue(inB));
	Term inC(plain);	inC.setFilterId(13);
	QVERIFY(!terms.containsValue(inC));				//a third filter is NOT in there
	QVERIFY(terms.containsValue(plain));			//C13: unqualified lookup matches by name
	QVERIFY(terms.containsValue("TestInts"));		//the QString overload stays a name lookup
	QVERIFY(!terms.containsValue("NoSuchColumn"));

	//adding an exact stamped twin is still a duplicate (no second entry), and an unqualified
	//insert UPDATES the stamped entry in place - it never demotes it to -1:
	terms.add(Term(inA));
	QCOMPARE(terms.size(), size_t(2));
	terms.add(plain);
	QCOMPARE(terms.size(), size_t(2));
	for(const Term &t : terms)
		QVERIFY(t.filterId() != -1);				//the stamps survived the unqualified insert

	//remove() finds by identity: removing one filter's copy leaves the other one alone and the
	//value map reindexes:
	terms.remove(inA);
	QCOMPARE(terms.size(), size_t(1));
	QVERIFY(!terms.containsValue(inA));
	QVERIFY(terms.containsValue(inB));
	QCOMPARE(terms.indexOfValue(inB), 0);

	//container equality inherits the strictness:
	Terms tA, tB;
	tA.add(inA);	tB.add(inB);
	QVERIFY(tA != tB);

	//ordered containers keep both stamped copies apart (operator< tiebreak; Terms::indexesFromTerms
	//and the std::map<Term,...> inside it lean on exactly this):
	std::map<Term, int> ordered;
	ordered[inA] = 0;
	ordered[inB] = 1;
	QCOMPARE(ordered.size(), size_t(2));
	QCOMPARE(ordered[inA], 0);
	QCOMPARE(ordered[inB], 1);

	//combination terms carry the source filter when uniform (interaction components share the
	//term-level filterId, mds-plan D6.1) and stay unqualified when the sources mix:
	Term x(tq("x"), columnType::nominal);		x.setFilterId(11);
	Term y(tq("y"), columnType::nominal);		y.setFilterId(11);
	Terms uniform;
	uniform.add(x);
	uniform.add(y);
	Terms crossed = uniform.crossCombinations();
	QVERIFY(crossed.size() > 1);
	for(const Term &t : crossed)
		QCOMPARE(t.filterId(), 11);
	Term way = uniform.wayCombinations(2).at(0);
	QCOMPARE(way.components().size(), size_t(2));	//the interaction product itself
	QCOMPARE(way.filterId(), 11);

	Term yOther(tq("y"), columnType::nominal);	yOther.setFilterId(12);
	Terms mixed;
	mixed.add(x);
	mixed.add(yOther);
	Terms mixedProducts = mixed.crossCombinations();
	bool sawQualified = false, sawInteraction = false;
	for(const Term &t : mixedProducts)
	{
		if(t.components().size() > 1)
		{
			sawInteraction = true;
			QCOMPARE(t.filterId(), -1);				//mixed provenance cannot claim one source
		}
		else
			sawQualified = t.filterId() >= 0;		//the single-term combinations keep theirs
	}
	QVERIFY(sawInteraction);
	QVERIFY(sawQualified);

	//NO wire field yet (mds-plan D2.1, serialization is Phase 3): the json of a stamped term is
	//byte-identical to the unqualified one, and parsing options yields unqualified terms.
	QCOMPARE(tq(inA.toJson().toStyledString()), tq(plain.toJson().toStyledString()));
	Json::Value stamped = inA.toJson();
	QVERIFY(!stamped.isMember("filterId"));
	Term reparsed(stamped, "value", "", inA.types());
	QCOMPARE(reparsed.filterId(), -1);
	QVERIFY(reparsed == plain);
}

void TestAll::testJaspArchiveVersionBlocksOlderBuilds()
{
	//mds-plan C4/VERSION: the archive version written into every manifest is 7.0.0 (major).
	//JASPImporter::isCompatible blocks an older build on `archiveMajor > their own major`, so
	//aware .jasp files (filter-id selections, eagerly materialised per-file filters) cannot be
	//half-read by a pre-feature JASP; the same build round trips its own files.
	QVERIFY(_newPkgWithDataSet());

	const std::string jaspFile = TempFiles::createSpecific("mdsarchiveversion", "temp.jasp");

	Json::Value compareMe = _pkg->dataSet()->jsonForCompare();
	JASPExporter::createSnapshot("mdsarchiveversion_snapshot_");
	JASPExporter().saveDataSet(jaspFile, [](int){});

	const ManifestInfo manifest = ArchiveReader::readManifest(jaspFile);
	//Version::asString minimises (trailing zeroes dropped), so the manifest reads "7": compare
	//as Versions, which is exactly what JASPImporter::isCompatible does with it.
	QVERIFY2(Version(manifest.jaspArchiveVersion) == JASPExporter::jaspArchiveVersion
	         && Version(manifest.jaspArchiveVersion).major() == 7,
	         qPrintable(QString("manifest should carry the 7.0.0 archive version, got: %1")
	                    .arg(tq(manifest.jaspArchiveVersion))));

	//Same-build round trip: the importer's own gate must accept what this build wrote...
	_pkg->reset();
	JASPImporter::loadDataSet(jaspFile, [](int){});
	QVERIFY2(_pkg->dataSet(), "same-build round trip must open");
	QVERIFY2(_pkg->dataSet()->jsonForCompare() == compareMe, "same-build round trip must preserve the dataset");
	QCOMPARE(_pkg->archiveVersion().major(), 7);

	//...and the gate's direction is the MAJOR comparison that blocks older builds (a 6.x build
	//reading this file sees 7 > 6 and refuses; the other direction stays readable):
	QVERIFY(Version("7.0.0").major() > Version("6.0.0").major());
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

void TestAll::testRunFiltersColumnAware()
{
	QVERIFY(_newPkgWithDataSet());

	DataSet * ds = _pkg->dataSet();
	QVERIFY(ds);

	Column * scaleCol	= ds->column("contNormal");
	Column * otherCol	= ds->column("contGamma");
	Column * nominalCol	= ds->column("facGender");
	QVERIFY2(scaleCol && otherCol && nominalCol, "debug.csv must contain contNormal, contGamma and facGender");
	QVERIFY2(nominalCol->hasLabels() && nominalCol->labelsNonEmptyCount() >= 2, "facGender must have at least 2 labels");

	// --- Named filter: columnUsed() must come from the columns scanned out of the R filter ---
	Filter * f = ds->createFilter("testColumnAware", true);
	QVERIFY(f);
	f->setRFilter(scaleCol->name() + " > 0"); //setRFilter runs rescanForColumns()
	QVERIFY2(f->columnUsed(tq(scaleCol->name())),	"R filter referencing a column must report it as used");
	QVERIFY2(!f->columnUsed(tq(otherCol->name())),	"R filter must not report an unrelated column as used");

	f->setInvalidated(false);
	QVERIFY2(!f->invalidated(), "precondition: filter starts fresh");

	//An edit to an unrelated column must NOT re-run (invalidate) the filter.
	ds->runFilters(tq(otherCol->name()));
	QVERIFY2(!f->invalidated(), "runFilters with an unrelated column must not invalidate the filter");

	//An edit to a used column must invalidate it.
	ds->runFilters(tq(scaleCol->name()));
	QVERIFY2(f->invalidated(), "runFilters with a used column must invalidate the filter");

	//Unknown editor (empty column name) must invalidate everything (the old blanket behaviour).
	f->setInvalidated(false);
	ds->runFilters();
	QVERIFY2(f->invalidated(), "runFilters without a column must invalidate all filters");

	// --- Default filter: label-filtered columns must be reported as used ---
	//A value edit can move a row between label levels without changing the generated filter
	//code, so Filter::columnUsed() must also look at the active label filters (its _labelGen
	//branch). Named filters have no label generator and must not pick up this path.
	Filter * def = ds->defaultFilter();
	QVERIFY(def);
	QVERIFY2(!nominalCol->hasLabelFilter(), "precondition: no label filter active on facGender yet");
	QVERIFY2(!def->columnUsed(tq(nominalCol->name())), "without an active label filter the column must not count as used");

	nominalCol->setLabelAllowFilter(0, false);
	QVERIFY2(nominalCol->hasLabelFilter(), "deactivating a label must activate the label filter");

	QVERIFY2(def->columnUsed(tq(nominalCol->name())), "default filter must treat label-filtered columns as used");
	QVERIFY2(!f->columnUsed(tq(nominalCol->name())), "named filters must not pick up the label-filter path");

	//The label-filtered column must drive runFilters for the default filter only.
	def->setInvalidated(false);
	f->setInvalidated(false);

	ds->runFilters(tq(otherCol->name()));
	QVERIFY2(!def->invalidated(), "editing a column without label filter must not invalidate the default filter");

	ds->runFilters(tq(nominalCol->name()));
	QVERIFY2(def->invalidated(),	"editing a label-filtered column must invalidate the default filter");
	QVERIFY2(!f->invalidated(),		"editing a label-filtered column must not invalidate a named filter that does not use it");
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

//No modules are loaded in this backendless test process, so every module-backed analysis in a jasp
//file reports that its module is missing and the batch run exits non-zero for that alone. Anything
//else in the report means the chain under test actually went wrong.
bool TestAll::_batchErrorsAreOnlyMissingModules(MainWindow * mw)
{
	for(const QString & error : mw->_batchResult.errors)
		if(!error.contains("Module is not available"))
		{
			qWarning() << "Unexpected batch error:" << error;
			return false;
		}

	return true;
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
	//does not need them (no backend at all, though the tests do run under a display), and both add
	//many threads (sqlite contention, JS garbage collection) that make the test flaky. See
	//backendlessTestMode() in mainwindow.cpp.
	qputenv("JASP_TEST_BACKENDLESS", "1");

	//Other tests may have left the global PreferencesModel singleton behind; the MainWindow wants
	//to construct its own, and a second one asserts.
	delete PreferencesModel::prefs();

	QCoreApplication::setApplicationName("JASPTest"); //so checkForUpdates() stays out of the way

	try
	{
		//In batch mode, because that is what the command-line chain under test is: since #6318 the
		//chain ends in finishBatchRun(), which returns right away when the window was not started
		//for a batch, so without this nothing ever exits.
		mw = new MainWindow(nullptr, true);
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

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//A scratch folder inside JASP's own session directory: QTemporaryDir can trigger a permission
	//dialog on some systems, while TempFiles already has a session dir set up by
	//_newMainWindowWithExitSpy. The MainWindow teardown removes the session dir, so no manual
	//cleanup of the folder or its file is needed.
	const QString	syncCsv		= QString::fromStdString(TempFiles::createTmpFolder()) + "/syncdata.csv";

	QVERIFY(_writeTextFile(syncCsv, syncCsvData));

	//The results page finished loading before the command line triggers the file open, and nothing
	//has been loaded into the workspace yet. Note that _open's dataset check still passes, because
	//JASP startup itself creates an empty dataset; the chain therefore binds the *loaded* dataset
	//only after the open has finalized, which is exactly what chain(..., resetDataSet=true) does.
	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, syncCsv, "", false);

	//The open -> synchronize chain should finish by exiting JASP, and complain about nothing but the
	//modules this process does not have:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(_batchErrorsAreOnlyMissingModules(mw));
	QCOMPARE(exitSpy->first().first().toInt(), mw->_batchResult.errors.isEmpty() ? 0 : 1);

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

void TestAll::testCliSyncExportWaitsForAnalysesToSettle()
{
	//Regression for the export capturing the intermediate emptied results: synchronizing can
	//schedule analysis refreshes deferred, and a refresh that starts *after* the export began
	//wipes the results page to its empty in-between state while the export is capturing it.
	//The export must therefore wait until the analyses have stopped changing status. Without a
	//webview the exported HTML itself cannot be checked here, so this test simulates the wipe by
	//flipping an analysis' status right after the export was queued and verifies the export only
	//starts once the analysis is finished again.
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//Short-circuit the exporter's wait for the (nonexistent) webview: prepForExport is emitted from
	//the exporter thread and only the QML results page would answer it, so answer it here instead.
	connect(mw->_resultsJsInterface, &ResultsJsInterface::prepForExport, mw->_resultsJsInterface, &ResultsJsInterface::exportPrepFinished, Qt::QueuedConnection);

	const QString	syncCsv		= QString::fromStdString(TempFiles::createTmpFolder()) + "/syncdata.csv",
					outHtml		= QString::fromStdString(TempFiles::createTmpFolder()) + "/results.html";

	QVERIFY(_writeTextFile(syncCsv, "V1\n1\n2\n"));

	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, syncCsv, outHtml, false);

	//The open -> synchronize chain queues an export that waits for the analyses:
	QTRY_VERIFY_WITH_TIMEOUT(mw->_waitingEvent != nullptr, 30000);
	QVERIFY(!mw->_waitingEvent->isStarted()); //the settle-debounce guarantees it cannot start this early

	//In backendless mode the module-backed analyses from the jasp file are skipped, so inject a
	//report-style analysis (created without a module, and never run: Analysis::run ignores reports):
	Json::Value analysisData;
	analysisData["id"]		= 1;
	analysisData["title"]	= "SettleTest";
	analysisData["isReport"]= true;
	analysisData["status"]	= "complete";
	Analysis * analysis = mw->_analyses->createFromJaspFileEntry(analysisData, nullptr);
	QVERIFY(analysis);
	QVERIFY(mw->_analyses->allFinished());

	//Simulate a refresh starting (status Complete -> Empty, like Analysis::run() does) right after
	//the export was queued: the pending start must be postponed until the analyses finish again:
	analysis->setStatus(Analysis::Empty);
	QVERIFY(mw->_waitingEvent); //not consumed by a premature start
	QVERIFY(!mw->_waitingEvent->isStarted());

	analysis->setStatus(Analysis::Complete);

	//Only now may the export start, which is visible as the waiting event being taken over:
	QTRY_VERIFY_WITH_TIMEOUT(mw->_waitingEvent == nullptr, 10000);

	//The exporter waits for the (nonexistent) webview to deliver the HTML, and ResultsJsInterface::
	//exportHTML resets the ready flag right before that wait; keep setting it so the exporter sees
	//it ready no matter when exactly its wait starts (it dies with mw):
	QTimer * readySetter = new QTimer(mw);
	readySetter->setInterval(100);
	connect(readySetter, &QTimer::timeout, this, [](){ DataSetPackage::pkg()->setAnalysesHTMLReady(); });
	readySetter->start();

	//The export finishing is what exits JASP, again complaining about nothing but the missing modules:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(_batchErrorsAreOnlyMissingModules(mw));
	QCOMPARE(exitSpy->first().first().toInt(), mw->_batchResult.errors.isEmpty() ? 0 : 1);
	QVERIFY(QFileInfo::exists(outHtml));

	delete exitSpy;
	delete mw; //MainWindow::singleton() and other singletons must not survive into the next test

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
}

void TestAll::testCliSyncExportChainFailsOnBadDataFile()
{
	const QString	jaspPath	= _testLibrary().absoluteFilePath("jasp/Descriptives-Debug.jasp");

	QVERIFY(QFileInfo::exists(jaspPath));

	MainWindow * mw = nullptr;
	QSignalSpy * exitSpy = _newMainWindowWithExitSpy(mw);

	//A scratch folder inside JASP's own session directory: QTemporaryDir can trigger a permission
	//dialog on some systems, while TempFiles already has a session dir set up by
	//_newMainWindowWithExitSpy. The MainWindow teardown removes the session dir, so no manual
	//cleanup of the folder or its file is needed.
	const QString	bogusData	= QString::fromStdString(TempFiles::createTmpFolder()) + "/bogus.xlsx";

	QVERIFY(_writeTextFile(bogusData, "this is not a spreadsheet\n"));

	mw->_resultsJsInterface->setResultsLoaded(true);

	mw->open(jaspPath, bogusData, "", false);

	//A failed synchronization must exit JASP with a non-zero code instead of continuing as if the
	//synchronization had succeeded:
	QTRY_COMPARE_WITH_TIMEOUT(exitSpy->count(), 1, 30000);
	QVERIFY(exitSpy->first().first().toInt() != 0);

	//...and for the right reason: the modules missing from this backendless process already make the
	//run non-zero, so check the import failure is actually in the report.
	QVERIFY2(!mw->_batchResult.errors.filter("Could not import data").isEmpty(),
			 qPrintable("Batch errors: " + mw->_batchResult.errors.join(" | ")));

	delete exitSpy;
	delete mw; //MainWindow::singleton() and other singletons must not survive into the next test

	//The MainWindow teardown took the session directory (and its database) with it; following tests
	//still expect it to exist.
	TempFiles::init(ProcessInfo::currentPID());
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

	QStringList keyList(const stringvec & keys)
	{
		QStringList out;
		for(const std::string & k : keys)
			out << tq(k);
		return out;
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

	// String literal escaping: quotes and backslashes must reach R escaped
	checkR(formulas({strNode("it's a \\ test")}), "'it\\'s a \\\\ test'\n");

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

void TestAll::testScriptConstructorRobustJson()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptConstructorModel model;
	model.setMode(ScriptConstructorMode::Filter);

	// Garbage (e.g. a corrupt .jasp file) must neither throw nor crash: parse failure -> empty tree.
	model.fromJson(std::string("this is definitely not json {"));
	QCOMPARE(model.formulaCount(), 0);
	QCOMPARE(model.toR(), std::string(""));

	// A formula with a non-string nodeType is skipped; valid formulas next to it survive.
	model.fromJson(std::string(
		"{\"formulas\":["
		"{\"nodeType\":123},"
		"{\"nodeType\":\"Number\",\"value\":3},"
		"{\"nodeType\":\"Column\",\"columnName\":\"a\",\"columnTypeUser\":\"bogus\",\"columnTypeDrop\":null}"
		"]}"));
	QCOMPARE(model.formulaCount(), 2);
	QCOMPARE(model.toR(), std::string("3\n& a.scale\n"));

	// A RowFunction with a mistyped dropped item gets an empty slot instead of exploding.
	model.fromJson(std::string(
		"{\"formulas\":[{\"nodeType\":\"RowFunction\",\"functionName\":\"rowMean\",\"droppedItems\":[42,\"null\"]}]}"));
	QCOMPARE(model.formulaCount(), 1);
	QVERIFY(!model.checkCompleteness());

	// A mistyped function argument is skipped without shifting the values of the arguments after it.
	model.fromJson(std::string(
		"{\"formulas\":[{\"nodeType\":\"Function\",\"functionName\":\"round\",\"arguments\":["
		"null,"
		"{\"name\":\"y\",\"dropKeys\":[\"number\"],\"argument\":{\"nodeType\":\"Number\",\"value\":3.5}},"
		"{\"name\":\"n\",\"dropKeys\":[\"number\"],\"argument\":{\"nodeType\":\"Number\",\"value\":1}}"
		"]}]}"));
	QCOMPARE(model.formulaCount(), 1);
	QCOMPARE(model.toR(), std::string("round(3.5, 1)\n"));
}

void TestAll::testScriptConstructorFunctionPalette()
{
	auto paletteNames = [](ScriptConstructorMode mode)
	{
		QStringList names;
		for(const ScriptFunctionDef & def : ScriptConstructorRegistry::instance().functionsForMode(mode))
			names << tq(def.name);
		names.sort();
		return names;
	};

	// The deleted FilterWindow.qml functionModel (its row functions come from rowFunctions(), and
	// sqrt and ! live in the operator bar).
	QStringList filterPalette = { "abs", "sd", "var", "sum", "prod", "zScores", "min", "max", "mean", "sign", "round", "length", "median",
												   "ifelse", "hasSubstring", "is.na" };
	filterPalette.sort();
	QCOMPARE(paletteNames(ScriptConstructorMode::Filter), filterPalette);

	// The deleted ComputeColumnWindow.qml functionModel, used by both column modes.
	QStringList columnPalette = { "abs", "sd", "var", "sum", "prod", "zScores", "min", "max", "mean", "sign", "round", "length", "median",
												   "log", "log2", "log10", "logb", "exp", "fishZ", "invFishZ", "logit", "invLogit",
												   "BoxCox", "BoxCoxAuto", "invBoxCox", "powerTransform", "powerTransformAuto", "YeoJohnson", "YeoJohnsonAuto", "Johnson",
												   "cut", "replaceNA", "ifElse", "hasSubstring", "is.na",
												   "normalDist", "tDist", "chiSqDist", "fDist", "binomDist", "negBinomDist", "geomDist", "poisDist",
												   "betaDist", "unifDist", "gammaDist", "expDist", "logNormDist", "weibullDist" };
	columnPalette.sort();
	QCOMPARE(paletteNames(ScriptConstructorMode::ComputedColumn),	columnPalette);
	QCOMPARE(paletteNames(ScriptConstructorMode::ComputedDataSet),	columnPalette);
}

void TestAll::testScriptConstructorNumberLiteralText()
{
	ScriptNodeLiteral		lit(ScriptNode::Type::Number);	// outlives the view and its items
	ScriptConstructorView	view;

	const std::vector<std::pair<double, QString>> cases = {
		{ 123456789,		"123456789"		},
		{ 0.123456789,	"0.123456789"	},
		{ -2.5,			"-2.5"				},
		{ 3,				"3"					}
	};

	for(const auto & [value, text] : cases)
	{
		lit.setNumberValue(value);
		ScriptNodeItem * item = view.makeNodeItem(&lit, &view);

		QQuickItem * input = nullptr;
		for(QQuickItem * child : item->childItems())
			if(QByteArray(child->metaObject()->className()) == "QQuickTextInput")
				input = child;
		QVERIFY(input);
		QCOMPARE(input->property("text").toString(), text);

		// The editor writes its text back into the model when it loses focus.
		QMetaObject::invokeMethod(input, "editingFinished");
		QVERIFY2(lit.numberValue() == value, qPrintable(QString("%1 became %2").arg(text).arg(lit.numberValue(), 0, 'g', 17)));
	}
}

void TestAll::testScriptConstructorModeDropKeys()
{
	QVERIFY(_newPkgWithDataSet());

	ScriptNodeOperator split("%|%", false);
	QCOMPARE(keyList(split.slotDropKeys(0, ScriptConstructorMode::Filter)),			QStringList({"boolean"}));
	QCOMPARE(keyList(split.slotDropKeys(0, ScriptConstructorMode::ComputedColumn)),	QStringList({"number"}));
	QCOMPARE(keyList(split.slotDropKeys(0, ScriptConstructorMode::ComputedDataSet)),	QStringList({"number"}));
	QCOMPARE(keyList(split.slotDropKeys(1, ScriptConstructorMode::ComputedColumn)),	QStringList({"string", "boolean"}));

	// A scale column dropped on a computed column's %|% fills its left slot and stays scale.
	FixedColumnTypeProvider provider;
	provider.types["contNormal"] = 1; // scale

	ScriptConstructorModel model;
	model.setColumnTypeProvider(&provider);
	model.setMode(ScriptConstructorMode::ComputedColumn);
	model.insertNode(new ScriptNodeOperator("%|%", false),	DropTarget::root());
	model.insertNode(new ScriptNodeColumn("contNormal"),	DropTarget::none());
	QCOMPARE(model.formulaCount(), 1);
	QCOMPARE(model.toR(), std::string("(contNormal.scale %|% null)"));
}

void TestAll::testScriptConstructorMirroredKeys()
{
	QVERIFY(_newPkgWithDataSet());

	const ScriptConstructorMode filter = ScriptConstructorMode::Filter;

	FixedColumnTypeProvider provider;
	provider.types["contNormal"] = 1; // scale

	ScriptConstructorModel model;
	model.setColumnTypeProvider(&provider);
	model.setMode(filter);

	// An empty == accepts anything on either side...
	auto * eq = new ScriptNodeOperator("==", false);
	model.insertNode(eq, DropTarget::root());
	QCOMPARE(keyList(eq->slotDropKeys(0, filter)), QStringList({"boolean", "string", "number"}));

	// ...but once the right side holds a string, the left side only takes strings.
	auto * text = new ScriptNodeLiteral(ScriptNode::Type::String);
	text->setStringValue("abc");
	model.insertNode(text, DropTarget{DropTarget::Kind::OperatorRight, eq, 1, eq->slotDropKeys(1, filter), false, true});
	QCOMPARE(keyList(eq->slotDropKeys(0, filter)), QStringList({"string"}));

	ScriptNodeLiteral number(ScriptNode::Type::Number);
	QVERIFY(!DropTarget({DropTarget::Kind::OperatorLeft, eq, 0, eq->slotDropKeys(0, filter), false, true}).accepts(&number, filter));

	// A scale column still fits, as a type that compares with strings (like the old JASPColumn.qml).
	model.insertNode(new ScriptNodeColumn("contNormal"), DropTarget::none());
	QCOMPARE(model.toR(), std::string("(contNormal.ordinal == 'abc')\n"));

	// The mirroring works in both directions: a scale column on the left of != wants numbers.
	model.fromJson(formulas({opNode("!=", colNode("contNormal", -1, 1), Json::nullValue)}));
	auto * ne = dynamic_cast<ScriptNodeOperator*>(model.formulaAt(0));
	QVERIFY(ne);
	QCOMPARE(keyList(ne->slotDropKeys(1, filter)), QStringList({"number"}));
}

void TestAll::testScriptConstructorLengthAcceptsBooleans()
{
	// One registry entry serves both modes, so the filter accepts booleans here as well.
	for(ScriptConstructorMode mode : {ScriptConstructorMode::Filter, ScriptConstructorMode::ComputedColumn})
	{
		ScriptNodeFunction	length("length");
		ScriptNodeLiteral	truth(ScriptNode::Type::Boolean);
		QVERIFY(DropTarget({DropTarget::Kind::FunctionArg, &length, 0, length.slotDropKeys(0, mode), false, true}).accepts(&truth, mode));
	}
}

void TestAll::testScriptConstructorDragKeepsFormulaRows()
{
	QStandardItemModel		columns;	// outlives the view, which listens to it
	ScriptConstructorView	view;
	view.setSize(QSizeF(800, 400));
	view.ensureChromeBuilt();
	view.setColumnsModel(&columns);

	auto * number = new ScriptNodeLiteral(ScriptNode::Type::Number);
	view.model()->insertNode(number, DropTarget::root());
	view.refresh();

	// The formula row showing `number` (a direct child of the script column), if any.
	auto rowItem = [&]() -> ScriptNodeItem *
	{
		for(QQuickItem * scriptColumn : view.scriptArea()->childItems())
			for(QQuickItem * child : scriptColumn->childItems())
				if(auto * item = qobject_cast<ScriptNodeItem*>(child); item && item->node() == number)
					return item;
		return nullptr;
	};

	ScriptNodeItem * item = rowItem();
	QVERIFY(item);

	// A relayout during the drag (a resize, the hint growing a line, ...) leaves the formula at the cursor.
	const QPointF grab = item->mapToScene(QPointF(1, 1));
	view.startDragExisting(item, grab);
	view.dragMove(grab + QPointF(150, 100));
	const QPointF dragged = item->position();
	view.setWidth(view.width() + 20);
	QCOMPARE(item->position(), dragged);

	// A columns-model change rebuilds the palette, which cancels the drag: the formula gets a row again,
	// and relayouts after the dragged item is deleted must not touch it.
	columns.appendRow(new QStandardItem("x"));
	QTRY_VERIFY(rowItem() != nullptr);
	QCoreApplication::sendPostedEvents(nullptr, QEvent::DeferredDelete);
	view.setWidth(view.width() + 20);
	QVERIFY(rowItem() != nullptr);
}


QTEST_MAIN(TestAll)
