#include <QTest>
#include <QTemporaryDir>

class DataSetPackage;
class Importer;
class DataSet;
class DataSetSyncer;
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

	// AsyncLoader FileEvent sync flow test
	void	testFileSyncerFullAsyncFlow();

	// SQLite database sync test
	void	testSyncerDatabaseSyncFromSQLite();

	// Closing/removing datasets and workspaces must never crash (regression for the dataset-close crash
	// and the workspace teardown paths).
	void	testCloseWorkspaceAndDataSets();

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

	// "Best spot" drop resolution: a drop without an explicit target fills the leftmost
	// empty accepting slot, working left-to-right / top-to-bottom through the formulas.
	void	testScriptConstructorLeftMostEmpty();

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
	QQuickItem			*	_findQuickItemByName(const QString & objectName);
};
