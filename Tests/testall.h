#include <QTest>
#include <QTemporaryDir>

class DataSetPackage;
class Importer;
class DataSet;
class DataSetSyncer;
class QSignalSpy;
class MainWindow;

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
	void	testCancelledImportLeavesTheModelUsable();
	void	testDatabaseImportNulls();
	void	testCsvImportLocale();
	void	testCsvImportLocaleColumnWidth();
	void	testNumbersGroupedWithSpaces();
	void	testCsvImportNumbers_data();
	void	testCsvImportNumbers();
	void	testCsvSyncNumbers_data();
	void	testCsvSyncNumbers();
	void	testCsvSyncTextAndNumbersStayApart();
	void	testFailedLoadOrSyncForgetsCsvChoices();
	void	testOdsImportLocale();
	void	testMinitabImportLocale();
	void	testDataSetsTableUpgrade();
	void	testSavLabels();
	void	testFilterLabels();

	// JASP reads the whitespace around a csv field as padding, but whatever sits inside quotes is data.
	void	testCsvParserTrimsOnlyUnquotedPadding();

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

	// Multi-dataset selection: the workspace must offer every filter of every dataset (value =
	// globally unique filterId, label = "DataSet - Filter") for VariablesForm::dataSetSelection.
	void	testDataSetFilterDropDownList();

	// Multi-dataset encoding: the same column in two different datasets must be encoded (and
	// attributed) against the encoder of its own dataset, per the .meta dataSetId provenance.
	void	testPerDataSetEncodingUsesOwnDatasetEncoder();

	// Option provenance: AnalysisBase must gather the dataSetId -> filterId pairs from the .meta
	// of its bound values (with -1 for options that carry no filterId).
	void	testAnalysisBaseReferencedDataSets();

	// Engine read-queue: the per-dataset slices are handed out exactly once, in order, and an empty
	// queue restores the legacy single-dataset read-path.
	void	testMultiDataSetQueueHandout();

	// A multi-dataset aware analysis has no single dataset: usesDataSet() must answer from the
	// referenced datasets of the .meta, not from (the absence of) its own filter.
	void	testAnalysisBaseUsesDataSetWhenAware();

	// File round-trip 1/2: rebinding an aware analysis' options restamps meta from the current
	// filter, so the loaded provenance must be restored - but only for unchanged values.
	void	testRestoreProvenanceFromBoundValues();

	// File round-trip 2/2: dataSetId/filterId provenance saved by another session is re-resolved
	// through the name-based side table; unknown datasets keep the stale id.
	void	testRemapSavedProvenance();

	// Pin for the removal of that remapping: dataset/filter ids come straight from the storage
	// db and a .jasp restores that db verbatim, so reopening (fresh Workspace over the same
	// database) must hand back the very same ids - saved provenance is simply still valid.
	void	testDataSetFilterIdsSurviveStorageReload();

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

private:
	DataSetPackage		*	_pkg		= nullptr;
	Importer			*	_importer	= nullptr;
	bool					_newPkgWithDataSet();
	bool					_checkDoSyncFake();
	static bool				_writeTextFile(const QString & path, const QByteArray & contents);
	QSignalSpy			*	_newMainWindowWithExitSpy(MainWindow *& mw);
	static bool				_batchErrorsAreOnlyMissingModules(MainWindow * mw);
};
