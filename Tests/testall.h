#include <QTest>
#include <QTemporaryDir>

class DataSetPackage;
class Importer;
class DataSet;
class DataSetSyncer;
class QSignalSpy;
class MainWindow;
class QQuickItem;

class TestAll: public QObject
{
    Q_OBJECT
	
private slots:
    void    initTestCase();
    void    init();
	void	cleanup();
	void    testDataImport();
	void	testDataImport_data();
	void	testJaspDataImport();
	void	testJaspDataImport_data();
	void	testJaspRoundRobin_data();
	void	testJaspRoundRobin();
	void	testSavLabels();
	void	testFilterLabels();

	// DataSetSyncer tests
	void	testSyncerStartStopFileSyncing();
	void	testSyncerFileChangeEmitsSignal();
	void	testSyncerStartStopDatabaseSyncing();
	void	testSyncerSyncNowWithoutDataSource();
	void	testSyncerMultipleStartStop();
	void	testSyncerReleasesSyncGuardOnCompletion();
	void	testSyncerRetriesFileChangeMissedDuringSync();

	// DataExporter tests
	void	testDataExporterShownDataSetOnly();

	// DatabaseInterface regressions
	void	testFilterRevisionInvalidatedRoundTrip();

	// Filter cache-length regression: the engine result must be authoritative for the whole dataset.
	void	testFilterSetFilterVectorResizesToResult();

	// Computed-dataset cycle prevention: a computed dataset must not depend on a dataset that
	// (transitively) depends on it, or the recompute cascade would livelock.
	void	testComputedDataSetCycleDetection();

	// Undo regression: the drop-levels command stores its old value as the enum name so undo/redo
	// (which restore via dropLevelsTypeFromQString) do not throw missingEnumVal.
	void	testUndoColumnDropLevels();

	// Encoder regression: each dataset's encoder prefix must carry the dataset id (not -1), so
	// colliding column names across datasets cannot encode to the same name.
	void	testEncoderPrefixPerDataset();

	// Filter ownership: removeFilter must unregister (no dangling pointer in _filters) and
	// runFilters() must stay safe afterwards.
	void	testFilterRemoveFilter();

	// Sync + export integration tests
	void	testSyncerExportModifyReimport();
	void	testSyncerExportModifyReimportChangesDetected();

	// keepMissingColsWhenSyncing: pins the current semantics, including the fact that the kept columns
	// accumulate over the syncs of one session (see the --keepMissingColsWhenSyncing help text).
	void	testSyncKeepMissingColumns();

	// AsyncLoader FileEvent sync flow test
	void	testFileSyncerFullAsyncFlow();

	// SQLite database sync test
	void	testSyncerDatabaseSyncFromSQLite();

	// Closing/removing datasets and workspaces must never crash (regression for the dataset-close crash
	// and the workspace teardown paths).
	void	testCloseWorkspaceAndDataSets();

	// The PRO command-line chain: open a .jasp file and synchronize it with a data file afterwards.
	// Nothing is loaded yet when that chain is set up, which is exactly the condition
	// MainWindow::_open has to handle.
	void	testCliSyncExportChainFromFreshWorkspace();
	void	testCliSyncExportChainFailsOnBadDataFile();
	void	testCliSyncExportWaitsForAnalysesToSettle();

	// ScriptConstructor (drag-and-drop filter / computed column model) regression tests.
	// These replace the old QML FilterConstructor and must stay byte/behaviour compatible with the
	// JSON stored in .jasp files and the R code that gets sent to the engine.
	void	testScriptConstructorRoundTrip();
	void	testScriptConstructorGoldenR();
	void	testScriptConstructorCompleteness();
	void	testScriptConstructorUndo();
	void	testScriptConstructorGobble();
	void	testScriptConstructorDefaultFilterJson();
	void	testScriptConstructorAllowedColumnTypes();
	void	testScriptConstructorRowFunctionFreeSlot();

	// Malformed constructorJson (corrupt .jasp files) must never throw or crash: garbage is
	// rejected, mistyped/corrupt formulas are skipped.
	void	testScriptConstructorRobustJson();

	// "Best spot" drop resolution: a drop without an explicit target fills the leftmost
	// empty accepting slot, working left-to-right / top-to-bottom through the formulas.
	void	testScriptConstructorLeftMostEmpty();

	// The function palette of each mode matches the old QML constructors: the filter never offered
	// the computed-column transforms, cut/replaceNA or the random-data generators.
	void	testScriptConstructorFunctionPalette();

	// A number literal's inline editor shows the value in full, and committing it unchanged (a
	// focus loss) keeps the exact value.
	void	testScriptConstructorNumberLiteralText();

	// Operator slots take their drop keys for the constructor's mode: the left side of %|% takes
	// the condition (boolean) in a filter, but the values to split (number) in a computed column.
	void	testScriptConstructorModeDropKeys();

	// ==/!= compare like with like: once one side is filled, the other only accepts what it offers.
	void	testScriptConstructorMirroredKeys();

	// length() takes booleans too, as the old computed-column palette's "string:number:boolean" did.
	void	testScriptConstructorLengthAcceptsBooleans();

	// Boots the real QML MainWindow headlessly, loads a dataset and shows the filter window
	// (which instantiates the C++ ScriptConstructorView). Serves as a profiling harness for
	// the ScriptConstructor initialization path (use with JASP_TIMER_USED=ON) and as a
	// regression test that the full UI bootstrap + filter window opening works headlessly.
	void	testMainWindowShowsFilterWindow();

private:
	DataSetPackage		*	_pkg		= nullptr;
	Importer			*	_importer	= nullptr;
	bool					_newPkgWithDataSet();
	bool					_checkDoSyncFake();
	static bool				_writeTextFile(const QString & path, const QByteArray & contents);
	QSignalSpy			*	_newMainWindowWithExitSpy(MainWindow *& mw);
	QQuickItem			*	_findQuickItemByName(const QString & objectName);
};
