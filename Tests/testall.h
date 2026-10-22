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

	// Editing the data by hand must stop the external synching *and* say so, so the Synchronisation
	// ribbon button can switch to its off-state instead of claiming the data file is still leading.
	void	testManualEditStopsExternalSynching();

	// ...and "Reload Data File" has to really reload: re-importing the (unchanged) data file must throw
	// the hand-made edits away instead of leaving the edited data on screen.
	void	testReloadDataFileDiscardsManualEdits();

	// The model behind the data view must keep following the current undo stack across the workspace
	// teardown+recreation that loading data does, or the ribbon's Undo/Redo buttons never light up.
	void	testUndoChangedSurvivesWorkspaceRecreation();

	// Undoing the hand-made edits puts the data back to what the data file holds, so the external
	// synching has to come back on by itself.
	void	testUndoingManualEditRestoresSynching();

	// Editing a label is not a change of the data, so it must not switch the external synching off.
	void	testLabelEditDoesNotStopSynching();

	// Hand edits in one dataset must never turn the synching of another one on: that bookkeeping belongs
	// to the dataset, not to the package that happens to show it.
	void	testSynchRestoreIsPerDataSet();

	// ...and so are the hand edits themselves: editing a second dataset must switch off *its* synching,
	// even though another dataset was already edited by hand.
	void	testManualEditsAreTrackedPerDataSet();

	// The synching can also be switched back on straight on the syncer (FileMenu::setCurrentDataFile when
	// a generated data file lands). That must clear the manual-edits flag too, or the *next* hand edit
	// leaves the synching on and the watcher silently reverts it.
	void	testStartFileSyncingDirectlyAlsoClearsManualEdits();

	// The dataFileSynch flag outlives the session (it is saved in the workspace), the file watcher does
	// not. synchingExternally() must not report a synch that nobody is actually doing.
	void	testSynchingExternallyRequiresWatcher();

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

	// Encoder regression: each dataset owns a canonical mint (no full-string
	// encode/decode) and each Filter a view prefix "JASPColumn_D<dsId>_F<filterId>_", so colliding
	// column names cannot encode to the same token; decoding is context-free via the global table.
	void	testEncoderPrefixPerDataset();

	// Mutation funnel: after every mutation class {rename, add, remove, type-only,
	// filter create, filter destroy, dataset unload} the ONE decode table maps exactly the current
	// names of the live namespaces; stale/foreign tokens decode raw.
	void	testEncoderMutationFunnel();

	// Decode-superset semantics (C4/C16): every results-decode site is the static
	// entry now, so ONE sweep resolves tokens from ANY live namespace - other datasets, other
	// filters, the extras block - while unknown, foreign-never-live and deleted-filter tokens
	// stay raw everywhere.
	void	testDecodeSupersetSemanticsAcrossLiveNamespaces();

	// The computed-column results site in EngineRepresentation (enginerepresentation.cpp) must
	// decode with the one global table too - extras tokens included (known gap,
	// closed and pinned at D1.5).
	void	testComputedColumnSiteDecodesExtrasTokens();

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

	// C1/M1 request-boundary discipline: filling the queue through a real prepareMultiDataSetRun
	// and clearing it through the very call the syntax bridge entry and Engine::receiveAnalysisMessage
	// make (setMultiDataSetQueue({})) must leave nothing for the next request - a half-consumed
	// aware queue can never leak into a later (non-aware) read.
	void	testMultiDataSetQueueClearedAtRequestBoundary();

	// Encode-boundary per stamp: prepareMultiDataSetRun encodes each option against
	// the VIEW of the filter its .meta filterId stamp names, not the shown encoder; un-stamped
	// options dual-read back to the shown-filter fallback. The landmine case - a form whose HOST
	// filter is neither the shown dataset nor its shown filter - must still encode in the host's
	// namespace and decode through the one global table.
	void	testPrepareMultiDataSetRunEncodesPerStampView();
	void	testGeneratedEngineJsonSidecarDrivesRunAndIsStrippedForR();
	void	testPerTermMixedProvenanceEncodesViaOwnViews();
	void	testContainerSidecarWalkAndClassicEncoderStaysCurrentEncoder();
	void	testPlaceOptionFilterIdsCollisionDemotion();
	void	testPreAttachOptionsParkAndAreNeverReadForBehaviour();
	void	testProviderUpdateParticipationKeepsForeignStamps();

	// (the FactorsForm deferral closed): per-factor provenance transport. The
	// factors option's sidecar is an array-of-arrays (element i = factor i's per-term array,
	// FactorsFormBase::createFilterIds); the engine walk resolves it factor-positionally then
	// term-positionally via the documented array-of-objects discriminator, the collector walks
	// the nested arrays, and the flat families keep byte-identical shapes (no-misfire pins).
	void	testPerFactorProvenanceEncodesViaOwnViews();
	void	testPerFactorSidecarCollectorAndDiscriminatorNoMisfire();

	// the final Storage format - per-term {name, filterId?, types} objects for the
	// term families (simple, interaction, row-bearing; optionKey kept, row children kept), the
	// single JSON->Terms applier accepting BOTH the new Storage shape and the legacy Engine-shape
	// input with byte-identical semantics there, and lossless Storage-json -> parse -> regenerate
	// round trips - including the flagship same-name-different-filter twins (x@11 + x@12 must not
	// dedupe through a save/reload cycle; strict Term equality since D2.1 makes this real).
	void	testStorageTermShapeAndRoundTrip();
	void	testApplierAcceptsEngineAndStorageShapes();
	void	testRelatedValuesPerTermRowKeying();

	// NO stamp migration - old released options (plain names, no filterIds
	// anywhere) must parse to all-`-1` terms and encode end-to-end through the C13 fallbacks
	// exactly like a non-aware run today (engine walk hits the shown-filter view / current
	// encoder, queue holds only the analysis' own slice), and a sidecar id that no longer
	// resolves (a stale/foreign filterId LOADED from the D5.1 Storage format - those survive
	// restamp by design, landmine D4.x) must degrade through the very same fallback, never
	// crash and never queue a phantom slice.
	void	testReleasedOptionsEncodeViaLegacyFallbackAndDeadStampsDegrade();

	// the legacy bare-prefix result decoder - best-effort read-side shim for
	// PRE-EXISTING persisted results (released .jasp files can hold JaspColumn_<i>_Encoded
	// tokens). Unchanged columns decode, churned/removed ones stay raw, current D-tokens are
	// invisible to it, and NOTHING it materializes registers into the global decode table
	// (single-writer rule, live decode stays strict-table).
	void	testLegacyResultTokenDecoder();

	// released-format .jasp fixture end to end - loads (legacy result tokens
	// decode on the unchanged columns, the dead one stays raw, missing module cleanly skipped,
	// no crash), re-save writes our own envelope, reload is lossless and the 7.x gate skips
	// the legacy sweep. testJaspArchiveVersionBlocksOlderBuilds stays green next to it.
	void	testReleasedFormatFileUpgradesOnLoad();

	// C1/M6 loadFilteredData provenance: the stamped filterId is globally unique and implies its
	// dataset, so it anchors the column lookup; a mismatching dataSetId is verification-only and a
	// dead dataSetId cannot pull the read to the wrong dataset. Without a filterId the legacy
	// name-based path must behave exactly as before.
	void	testLoadFilteredDataFilterFirstProvenance();

	// C1/e4 primarySliceKey: a real primary filter yields its id as the "primary" member, and when
	// no primary filter exists the member is ABSENT (jaspBase keys on !is.null - "" would read as
	// a bogus key).
	void	testPrepareMultiDataSetRunPrimarySliceKey();

	// A multi-dataset aware analysis has no single dataset: usesDataSet() must answer from the
	// referenced datasets of the .meta, not from (the absence of) its own filter.
	void	testAnalysisBaseUsesDataSetWhenAware();

	// C3/M7: and neither has dataSpec() - the results-header spec must be built from the
	// referenced datasets (workspace titles, deduped, own dataset first, " + " joined) instead
	// of early-returning "" on the single-dataset accessor that aware analyses refuse (whose
	// desktop implementation also logged a warning on every call).
	void	testAnalysisBaseDataSpecWhenAware();

	// File round-trip: dataset/filter ids come straight from the storage db and a .jasp restores
	// that db verbatim, so reopening (fresh Workspace over the same database) must hand back the
	// very same ids - saved provenance is simply still valid, and rebinding needs no repair pass:
	// the derived selection dependency (see tst_dataSetSelectionVariablesList.qml) stamps values
	// with the loaded selection from the very first bind. (This invariant is also why
	// Analyses::remapSavedProvenance and its side table were removed.)
	void	testDataSetFilterIdsSurviveStorageReload();

	// the syntax surface of the dataset selection options is human strings
	// only - bare dataset title for the default filter, dropdown label "title - filterName" for a
	// named one, exact-match lookup against the live workspace (never parsed, ids not enterable) -
	// including the reverse direction used by the wrapper render and the label-collision
	// precedence (bare titles win).
	void	testDataSetSelectionSurfaceMap();

	// the user-qualified column-reference grammar - pure
	// parse/render: the three grammar levels + the bare production, roundtrip
	// parse(render(x)) == x for names with dots/quotes/backslashes/unicode, exactly two
	// escapes (\" and \\, anything else rejected), case-insensitive type segment normalized
	// to the canonical columnType spelling, and every malformed shape rejected with a message.
	void	testQualifiedColumnRefParseRenderRoundTrip();

	// resolution against the live Workspace - full form, "DS"."Col" (default
	// filter), unique-filter "F"."Col", the flagship same-name column addressed through two
	// filters of one dataset (distinct filterIds), duplicate columns across datasets, the
	// type segment winning over lookup, dataset-title precedence over filter names, and every
	// typed rejection (ambiguous filter name listing candidates, unknown dataset/filter/
	// column, bare refusal) distinguishable.
	void	testQualifiedColumnRefResolveAgainstWorkspace();

	// the context-aware minimal render - plain-name passthrough inside the
	// context's default namespace, shortest still-resolvable form outside it ("DS"."Col" /
	// unique "F"."Col" / full form), type segment only when asked for, dead/absent filter ids
	// render empty (loud-failure doctrine), and every emitted qualified form roundtrips
	// through parse+resolve to the exact (dataSetId, filterId, column) it was built from.
	void	testQualifiedColumnRefRenderMinimal();

	// the in-code scanner for qualified references (QualifiedColumnRef::scanText)
	// - finds every >=2-segment surface form in R code text with byte positions, skips R
	// comments and string literals wholesale (a surface form inside them is user text), never
	// counts a bare name (quoted or not) as a reference, tolerates whitespace, and only ever
	// returns candidates the strict parse accepts.
	void	testQualifiedColumnRefScanInCode();

	// Workspace::referencedFilterIdsInCode - the desktop-side scan that builds
	// the transient `referencedFilterIds` request sidecar: distinct resolved filter ids (self-
	// references included), bare names excluded, unresolvable refs skipped with the typed
	// first-error reported (the engine's run-time scan is the loud surface, D7.5).
	void	testWorkspaceReferencedFilterIdsInCode();

	// rbridge_prepareSliceRunScript - the engine-side consumption of the sidecar:
	// surface forms rewritten into the referenced filter view's tokens, the queue = foreign
	// slices first (sidecar order, per-filter columns) + the own slice last, non-aware code
	// byte-identical to the legacy encode, and loud typed errors for stale/unresolvable refs.
	// Pure C++ (no R calls), hence unit-tested here; the out-of-process behaviour is pinned in
	// TestEngine's slice tests.
	void	testPrepareSliceRunScript();

	// the editor-side catalog + validation surface of the qualified
	// grammar - Workspace::qualifiedColumnCandidates (every column of every filter of every
	// loaded dataset rendered through renderQualifiedColumnRef against the editor's context
	// filter: own context columns bare, everything else the minimal qualified form; the
	// ambiguous-name case forces the full 3-segment form; dead slices never offered; every
	// emitted code round-trips through resolveQualifiedColumnRef to the same filterId) and
	// Workspace::filterIdByName (the compute editors' context resolution).
	void	testQualifiedColumnCandidates();

	// the visual constructors' outward emission - ScriptNodeColumn::toR emits
	// the FULL qualified surface form ("Dataset"."Filter"."Column"("Type")) for outward nodes
	// (dataSetName set), composed from the resolved slice (Workspace::fullQualifiedColumnRef):
	// dataset-level and named-filter forms, the source column's type segment (+ wrapping)
	// without consulting the own-dataset type provider, serialization/clone round-trips, and
	// the loud stale-slice degradation (stored names kept, typed unknownDataSet at scan).
	// Own-dataset nodes stay byte-identical (the known-red golden payloads must not move).
	void	testScriptConstructorQualifiedOutwardToR();

	// cross-dataset dependency invalidation - a qualified reference that was
	// valid when typed can be invalidated WITHOUT any edit on the editor side, and every kind
	// invalidates LOUDLY and TYPED (no tombstoning, C16): a renamed referenced filter (the
	// Filter::nameChanged -> Workspace::filterNameChanged relay), an unloaded dataset
	// (deleteShownDataSet), and column churn. Per invalidation kind, the scanner's first
	// typed error names the missing slice (the same message the engine fails the run with).
	void	testQualifiedRefInvalidatedByFilterRename();
	void	testQualifiedRefInvalidatedByDataSetUnload();
	void	testQualifiedRefInvalidatedByColumnChurn();

	// (wiring 1 + 3, the D3.4 single JSON->Terms parse chokepoint): a STRING term
	// component that *is* a qualified surface form resolves into a TYPED term - the
	// plain column name plus its filterId (ids only, D1.2) - so same-name terms from different
	// filters stay distinct through parse/bind; bare components and non-references pass through
	// untouched (§4), the ("Type") segment wins over the positional type, uniform-qualified
	// interaction rows stamp as one term and anything mixed/unresolvable fails the parse LOUDLY
	// with the typed message. Storage roundtrip keeps the ids.
	void	testQualifiedTermComponentsParseTyped();

	// RSyntax term emission - terms stamped outside the emission context (the
	// analysis' own filter) render as the D7.0a qualified MINIMAL forms (backtick-wrapped in
	// formula text, quote-escaped in the addQuotes list() path, byte-identical bare for -1 and
	// own-context terms); the formula text parser keeps qualified forms verbatim (quote-aware
	// separator split) so the re-parse resolves through the D8.1 applier into typed terms
	// again - the full emission->re-parse roundtrip.
	void	testQualifiedTermEmissionMinimalFormsAndReparse();

	// the bridge leaves the raw options JSON of its parse on the form; a
	// selection host registering after that parse (async instantiation, the M9 case) resolves
	// its own option out of the pending JSON and writes the filter id into the bound values
	// immediately - dead hosts are pruned from the registry at read.
	void	testSelectionRegistryLateHostAppliesPendingValue();

	// (chunk-4 finding #1): a control registered on the form and destroyed later
	// must not stay in the form's control caches - the destroyed signal prunes _controls,
	// _dependsOrderedCtrls and the error-message references synchronously, so the passes that
	// walk them (clearFormErrors, filterByNameDone, usedVariables, the setAnalysisUp/bind pass)
	// never dereference freed memory, and the freed option name is reusable.
	void	testDeadControlsArePrunedFromFormCaches();

	// a Term carries the filterId it was selected through (-1 = unqualified) and
	// it is part of identity: same-name terms from different filters never compare equal, Terms
	// lookups distinguish stamped terms but keep the C13 fallback (an unqualified lookup matches
	// on name), moves/copies carry the id and combination terms inherit it when uniform. No JSON
	// wire field: options stay byte-identical until Phase 3's generators.
	void	testTermFilterIdIsPartOfIdentity();

	// the selection-host registry on AnalysisForm is introspectable (count/list in
	// registration order, per-option names kept) and unregisters eagerly on destruction - the
	// foundation D2.4 (AnalysisBase as VariableInfoProvider) aggregates over.
	void	testSelectionHostRegistryIntrospection();

	// AnalysisBase implements VariableInfoProvider over that registry -
	// 0 hosts fall back to the analysis' own filter, 1 host (deduped) delegates to that filter's
	// live namespace (handle resolved per use, never cached), >1 distinct filters give the merged
	// union view (same-name columns from different filters coexist; columnEncoder/providerModel
	// return nullptr for the merged case) - and selectionNamespaceChanged() tracks the aggregate.
	void	testAnalysisProviderAggregatesSelectionHosts();

	// a saved .jasp carries the 7.0.0 archive version (major bump: older
	// builds must reject aware files via JASPImporter::isCompatible's major-version gate, not try
	// to misread them), and a same-build round trip opens.
	void	testJaspArchiveVersionBlocksOlderBuilds();

	// Filter ownership: removeFilter must unregister (no dangling pointer in _filters) and
	// runFilters() must stay safe afterwards.
	void	testFilterRemoveFilter();

	// Column-aware runFilters: editing a column a filter does not use must NOT invalidate it,
	// while used columns (R filter) and label-filtered columns (default filter) must.
	// Guards against under-invalidation in DataSet::runFilters(editedColumn)/Filter::columnUsed.
	void	testRunFiltersColumnAware();

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

	// A dragged formula leaves the formula rows: a relayout during the drag does not snap it back,
	// and a cancelled drag (the columns model changed) gives it its row back.
	void	testScriptConstructorDragKeepsFormulaRows();

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
	static bool				_batchErrorsAreOnlyMissingModules(MainWindow * mw);
	QQuickItem			*	_findQuickItemByName(const QString & objectName);
};
