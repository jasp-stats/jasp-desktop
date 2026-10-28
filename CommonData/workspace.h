#ifndef WORKSPACE_H
#define WORKSPACE_H

#include <set>
#include "dataset.h"
#include "datasetbasenode.h"
#include "databaseinterface.h"

// --- user-qualified column references -------------------------------
// The surface grammar of an outward column reference in filter/compute/analysis code:
//
//   "Dataset"."Filter"."Column"("Type")   full form, always valid
//   "Dataset"."Column"                    that dataset's DEFAULT filter
//   "Filter"."Column"("Type")             only when the filter name is UNIQUE across all loaded
//                                         datasets (ambiguity => rejection listing candidates)
//   "Column"                              bare - NOT handled by these helpers (boundary below)
//
// Every segment is a double-quoted string and '.' separates segments only OUTSIDE quotes, so
// names containing dots need no escaping at all. Inside a segment exactly two escapes exist,
// R-style: \" for a quote and \\ for a backslash; a backslash before anything else is a parse
// error. That keeps the rule honest and roundtrip-safe: parse(render(x)) == x for every name,
// and a quote inside a name can never be confused with a segment boundary. The optional type
// segment is a quoted columnType spelling (scale/ordinal/nominal/nominalText, case-insensitive,
// canonical spelling on render) and WINS over the column's current type when present.
//
// BOUNDARY: these helpers parse/render/resolve QUALIFIED references only. A bare
// column name is never resolved here - which namespace it falls back to is per-context (the
// filter's own dataset, the selected input filter, the analysis host, the legacy current
// encoder) and is the CALLER's concern. parse() still recognizes the bare production (so
// callers can tell it apart from qualified input), resolve() refuses it with
// QualifiedRefError::unqualifiedReference, and the context-aware render returns the plain
// unquoted name when the reference lies inside the caller's default namespace.

enum class QualifiedRefError
{
    none = 0,
    unqualifiedReference,	///< bare name: no context in these helpers (the callers' fallback)
    unknownDataSet,			///< the dataset segment names no loaded dataset
    unknownFilterInDataSet,	///< the filter segment names no filter of the named dataset
    ambiguousFilterName,	///< 2-segment form: the filter name exists in several datasets (error lists them)
    filterNameMatchesNothing,///< 2-segment form: first segment is neither a dataset title nor a filter name
    unknownColumn,			///< the column segment names no column of the resolved dataset
    unknownTypeName			///< a hand-built ref whose type segment is no columnType spelling
};

/// The concrete resolution of a parsed reference against the live Workspace. Ids only on
/// purpose: consumers resolve the live objects per use (Workspace::filterById/dataSetById)
/// and never cache Filter*/Column* pointers (the lazy-attach landmine).
struct ResolvedQualifiedColumn
{
    int			dataSetId	= -1;
    int			filterId	= -1;
    std::string	columnName;				///< bare original name, never an encoded token
    columnType	type		= columnType::unknown;	///< the type segment when present (it wins), else the column's current type
};

struct QualifiedRefOccurrence;	///< defined below; one scanText() hit (byte range + parsed segments)

struct QualifiedColumnRef
{
    QString dataSetName;	///< dataset TITLE (the user-facing handle, like the selection surface); "" when absent
    QString filterName;		///< filter NAME (Filter::name - never the translatable "Default filter" title); "" when absent
    QString columnName;		///< always present
    QString typeName;		///< canonical columnType spelling when a type segment was parsed/built; "" when absent

    bool	isBare() const { return dataSetName.isEmpty() && filterName.isEmpty(); }
    bool	isFullyQualified() const { return !dataSetName.isEmpty() && !filterName.isEmpty(); }

    /// The surface form of exactly the segments set: quoted+escaped segments joined by '.',
    /// the type appended as ("Type") when typeName is non-empty. A bare ref renders as the
    /// quoted column alone; the UNQUOTED plain-name form (today's own-dataset code) is only
    /// ever produced by Workspace::renderQualifiedColumnRef's default-namespace passthrough.
    QString		render() const;
    /// `"` and `\` escaped R-style (\" / \\), everything else verbatim - dots need no escaping
    /// because the separator only acts outside quotes.
    static QString	escapeSegment(const QString & raw);
    /// Whole-string parse of one reference (whitespace around separators allowed). False +
    /// ready-to-show `error` on any violation; a bare input parses with only columnName set.
    static bool		parse(const QString & text, QualifiedColumnRef & out, QString & error);

    /// Scan R code text for quoted qualified references: every occurrence of
    /// the surface form with at least TWO segments ("DS"."Col", "F"."Col", the full
    /// form, each with its optional ("Type") tail). R comments and single- or double-quoted R
    /// string literals are skipped wholesale - a surface form inside them is user text, not a
    /// reference - and the bare production (one quoted segment) IS an R string literal, so a
    /// bare name is never a reference (the bare-name boundary above). The candidate shapes are
    /// validated by the strict parse() before they count, so the scanner cannot produce an
    /// occurrence parse() would reject. Positions are byte offsets into `code` (the grammar
    /// is ASCII-structural, so they are stable); feed them to callers that rewrite the text.
    static std::vector<QualifiedRefOccurrence>	scanText(const std::string & code);
};

/// One qualified reference found in code text by QualifiedColumnRef::scanText:
/// the byte range [begin, end) it occupies and the parsed segments. Never hand-built.
struct QualifiedRefOccurrence
{
    size_t				begin	= 0;
    size_t				end		= 0;
    QualifiedColumnRef	ref;
};

class Workspace : public DataSetBaseNode
{
	Q_OBJECT
	
	Q_PROPERTY(bool				dataMode			READ dataMode			WRITE setDataMode			NOTIFY dataModeChanged		)
	Q_PROPERTY(bool				showRSyntax			READ showRSyntax		WRITE setShowRSyntax		NOTIFY showRSyntaxChanged	)
	Q_PROPERTY(DataSet		*	shownDataSet		READ shownDataSet									NOTIFY shownDataSetChanged	)
	Q_PROPERTY(Column		*	shownColumn			READ shownColumn		WRITE setShownColumn		NOTIFY shownColumnChanged	)
	Q_PROPERTY(Filter		*	shownFilter			READ shownFilter		WRITE setShownFilter		NOTIFY shownFilterChanged	)
	Q_PROPERTY(VariableInfo *	varInfo				READ varInfo										CONSTANT					)
	Q_PROPERTY(QVariantList	inputFilterDropDownList READ inputFilterDropDownList						NOTIFY inputFilterDropDownListChanged	)
	///Every filter of every dataset in the workspace as {value: filterId, label: "DataSet - Filter"} entries,
	///for the multi-dataset aware dataset/filter selection (see VariablesForm::dataSetSelection). Selecting a
	///dataset always means selecting one of its filters, hence filters and not just datasets here.
	Q_PROPERTY(QVariantList	dataSetFilterDropDownList READ dataSetFilterDropDownList					NOTIFY dataSetFilterDropDownListChanged	)
	
	// Emit signals also in refresh
	
public:
	explicit Workspace(QObject *parent = nullptr);
	~Workspace();
	
			DatabaseInterface	 &	db();
	const	DatabaseInterface	 &	db() const;
	
			bool					dataMode()				const	{ return _dataMode;		}
			bool					showRSyntax()			const	{ return _showRSyntax;	}
			
			int						rowCount(		const QModelIndex &parent = QModelIndex())										const	override { return _dataSets.size(); }
			int						columnCount(	const QModelIndex &parent = QModelIndex())										const	override { return 1; }
			QVariant				data(			const QModelIndex &index, int role = Qt::DisplayRole)							const	override;
			
			VariableInfo		*	varInfo() const { return _varInfo; }
			
			void					setDataMode(		bool mode);
			void					setShowRSyntax(		bool showRSyntax);
			
			void					dbLoad(std::function<void(float)> progressCallback = [](float){}, Version doUpgradeFrom = Version());
			void					dbUpdate();
			void					dbDelete();
			
			bool					checkForUpdates(std::function<void(float)> progressCallback = [](float){});
			
				
			DataSet				*	shownDataSet()	const;
			DataSets				dataSets()		const;
			DataSet				*	dataSetById(int id) const;
			DataSet				*	dataSetByName(const std::string & name) const;
			///< Exact match on the user facing title (unique per Workspace::setTitle); syntax-mode
			///< dataset selection refers to datasets by name.
			DataSet				*	dataSetByTitle(const QString & title) const;
	///< Resolves live objects first, then lazily materialises named filters from the storage db
	///< (a reopened .jasp only loads default filters; the row id stays the invariant).
	Filter				*	filterById(int id);
			///Returns title if no other dataset already has that title, otherwise appends " (n)" with an incrementing n until it is unique. exclude lets a dataset check against the others without matching against its own current title.
			QString					makeDataSetTitleUnique(const QString & title, DataSet * exclude = nullptr) const;
			
			Column				*	shownColumn() const;
			Filter				*	shownFilter() const;
			void					setShownColumn(Column *newShownColumn);
			void					setShownFilter(Filter * newShownFilter);
			void					initializeComputedColumns();
	///True if making 'me' depend on 'target' (as defaultInputFilterId) would create a cycle among
	///the computed datasets. Used by DataSet::setDefaultInputFilterId to refuse loops.
	bool							wouldCreateComputedDataSetLoop(DataSet * me, DataSet * target) const;
	///True if any cycle exists among computed datasets; fills errorMessage. Used as an anti-livelock
	///sweep before running the recompute cascade.
	bool							computedDataSetsHaveLoop(std::string & errorMessage) const;
	static	Workspace			*	singleton() { return _singleton; }
	
	
public slots:
			void					refresh();
			DataSet				*	createDataSet();
			Column				*	createComputedColumn(const std::string & name, int dataSetId, int analysisId = -1, columnType type = columnType::unknown, computedColumnType desiredType = computedColumnType::analysis);
			DataSet				*	createComputedDataSet(const std::string & name, int defaultInputFilterId, computedColumnType desiredType = computedColumnType::rCode);
			Q_INVOKABLE int						shownDataSetId() const	{ return shownDataSet() ? shownDataSet()->id() : -1; }
			Q_INVOKABLE int						dataSetIdByName(const QString & name) const		{ DataSet * ds = dataSetByName(fq(name)); return ds ? ds->id() : -1; }
			Q_INVOKABLE QString					dataSetNameById(int id) const				{ DataSet * ds = dataSetById(id); return ds ? ds->name() : QString(); }
			Q_INVOKABLE QStringList				dataSetNames() const;
			QVariantList						inputFilterDropDownList() const;
			QVariantList						dataSetFilterDropDownList() const;

			// --- dataset selection on the syntax surface ------------------------------
			// Selection options carry FILTER ids everywhere internally (options json, .jasp, engine,
			// .meta) - but in syntax text (generated wrapper defaults, the call shown in the form,
			// hand-written R) they are human strings only: a dropdown label "title - filterName" for
			// a named filter, or the BARE dataset title for its default filter (the default filter's
			// own label part is tr()-translatable and must never reach syntax text; user filter names
			// are user strings and safe). These strings are never parsed or split - titles may contain
			// " - " or ":" - they are resolved by exact lookup against the maps below, built on demand
			// from the live workspace so they always self-sync. Filter ids are not enterable syntax.

			///< Syntax surface string -> filter id: every dropdown label, plus every bare dataset
			///< title pointing at that dataset's default filter. Bare titles win label collisions
			///  (a dataset may be titled like another pair's label - the title is the more direct
			///  handle), and the win is logged.
			QMap<QString, int>					datasetSelectionSurfaceMap() const;
			///< Reverse of that, for rendering: bound filter id -> bare title (a dataset's default
			///< filter) or "title - filterName" (named), "" for -1 or an id that resolves to nothing.
			QString								datasetSelectionSurface(int filterId) const;
			///< Resolve one raw selection-option value the way the syntax bridge does: a bare
			///< string (or int - which is only ever a title that happens to read as a number, never
			///< a dataset or filter id), or that same bare thing inside the desktop-shaped
			///< {types,value}. On failure returns false and fills a ready-to-show message, naming
			///  `optionName` and listing what IS selectable (syntax-facing English by design: the
			///  bridge hands it to R users untouched by the UI language).
			bool								resolveDataSetSelectionValue(const QString &optionName, const Json::Value &value, int &filterId, QString &error) const;

			// --- user-qualified column references -------------------
			// Parse/render are pure (QualifiedColumnRef); resolution runs against the live
			// workspace: the dataset segment is matched by TITLE (exact, like the selection
			// surface), the filter segment by NAME. The 2-segment form "X"."Column" resolves X
			// as a dataset title first (its default filter); only when no dataset is titled X
			// is X treated as a filter name, and then only when UNIQUE across all loaded
			// datasets - ambiguity is a rejection listing the candidate datasets. Every failure
			// kind is a distinct QualifiedRefError plus a ready-to-show message (syntax-facing
			// English, like resolveDataSetSelectionValue). Bare names are NOT resolved here (the
			// context fallback is the callers' concern - see the grammar block at the top).
			///< Parse a surface-form reference and resolve it to the concrete (dataSetId,
			///< filterId, column, type); `ref` must come from QualifiedColumnRef::parse or be
			///  built to the same rules. Returns the rejection kind (none on success).
			QualifiedRefError					resolveQualifiedColumnRef(const QualifiedColumnRef & ref, ResolvedQualifiedColumn & out, QString & error) const;
			///< Context-aware minimal render (qualify only what lies outside the
			///< context's default namespace): the plain unqualified column name when `filterId`
			///< IS the caller's context filter `contextFilterId` (a filter editor's own
			///< dataset+filter, the compute panel's selected input filter, an analysis host);
			///< otherwise the shortest form that still resolves - "DS"."Col" for a default
			///< filter, "F"."Col" when the filter name is unique across loaded datasets, the
			///< full form otherwise. The type segment is emitted only when `type` is not
			///  unknown. A dead/unresolvable `filterId` renders "" (the loud-failure doctrine
			///  of datasetSelectionSurface) and so does `filterId` < 0 (logged): these helpers
			///< never invent a bare form, because bare means the caller's context and this
			///< call just established the reference is NOT in it. Non-const on purpose: like
			///< filterById it may materialize a named filter of a reopened file.
			QString								renderQualifiedColumnRef(int contextFilterId, int filterId, const std::string & columnName, columnType type);

			///< the visual constructors' outward emission (ScriptNodeColumn::toR):
			///< the FULL qualified surface form ("Dataset"."Filter"."Column"("Type")) composed
			///< from the RESOLVED pieces - the slice is resolved live from `dataSetTitle` (+
			///< `filterName`, "" meaning the dataset's default filter), so a resolvable
			///< reference emits its current title/name and the default filter emits its
			///< internal DEFAULT_FILTER spelling (the full form always names the filter; the
			///< qualified-reference grammar resolves that spelling). This is the compose-from-resolved-ids
			///< helper: the emission is built from the resolved segments, never by parsing
			///< rendered text back. An unresolvable dataset title emits the STORED names
			///< verbatim - the run then fails loudly with the typed unknownDataSet error
			///< instead of degrading quietly. `type` unknown means no type segment
			/// (nothing meaningful to stamp; the type segment is optional).
			QString								fullQualifiedColumnRef(const QString & dataSetTitle, const QString & filterName, const std::string & columnName, columnType type) const;

			///< scan R code text (a filter expression, computed-column code,
			///< computed-dataset code) for qualified references, resolve each against this
			///< workspace and collect the referenced FILTER ids - the transient
			///< `referencedFilterIds` request sidecar the filter/compute request builders stamp.
			///< Bare names are NOT references here (the context filter's
			///< own namespace is the callers' host-bound fallback). A reference that does
			///< not resolve is skipped (it contributes no slice) and reported through
			///< `firstError`/`firstErrorKind` when given: the engine re-scans the very same code
			///< at run time and fails the run LOUDLY with the same kind + message, so the
			///< request side stays passive by design (no crash, no silent fallback - the run
			///  error surface is the engine's, which is what the rename invalidation triggers on).
			std::set<int>						referencedFilterIdsInCode(const std::string & code, QString * firstError = nullptr, QualifiedRefError * firstErrorKind = nullptr) const;

			// --- the editor-side surface of the same grammar ----------------
			// The three R editors (FilterWindow's filterEdit, ComputeColumnWindow's
			// computeColumnEdit, ComputeDataSetPanel's codeEdit) share two needs beyond the
			// request-side scan: a CATALOG to pick outward references from, and a cheap
			// VALIDATION of the code as typed. Both are read-only views over the same helpers:
			// insertion is always the verbatim output of renderQualifiedColumnRef (the minimal
			// surface form - the context's own columns come out bare, everything
			// else qualified), and validation is the scanner's typed first error. Neither
			// writes anything: bare names are never validated here (their fallback is the
			// editor's context, codified in the editors) and unresolvable references are a
			// non-blocking warning, the run itself still fails loudly at the engine.
			///< The slice catalog for the editors' pickers/autocomplete: every
			///< column of every filter of every loaded dataset, rendered through
			///< renderQualifiedColumnRef against `contextFilterId` (the edited filter / selected
			///< input filter; -1 = no context, everything renders qualified). Each entry is
			///< {code, label, dataSetTitle, filterName, filterTitle, columnName, filterId,
			///< isBare}; `code` is what gets inserted, `label` is display only. Entries whose
			///< slice does not resolve are skipped (never offer an unresolvable reference).
			///< The context's own dataset comes first, then the rest by id. Non-const on
			///< purpose: like renderQualifiedColumnRef it may materialize named filters of a
			///  reopened file.
			Q_INVOKABLE QVariantList			qualifiedColumnCandidates(int contextFilterId);
			///< The scanner as editor-side live validation: "" when every qualified
			///< reference in `code` resolves against this workspace, otherwise the first typed
			///< error message (ready to show; same message the engine fails the run with).
			Q_INVOKABLE QString					qualifiedRefErrorInCode(const QString & code) const;
			///< Filter-id lookup by NAME within one dataset (filter names are unique per
			///< dataset): how the compute editors resolve the selected compute/input filter's
			///< id for the render context. "DEFAULT_FILTER" resolves to the dataset's default
			///< filter (Column::computeFilter's empty-name spelling). -1 when unknown.
			Q_INVOKABLE int						filterIdByName(int dataSetId, const QString & filterName);

			Q_INVOKABLE void					setDataSetComputed(const QString & name, bool computed);
			void					setShownDataSet(QString	  name);
			void					setShownDataSet(DataSet * dataSet);
			void					setShownDataSet(int		  dataSetId);
			void					deleteShownDataSet();
			void					showFilter(int id);
			void					onShownFilterChanged(DataSet * data);
			void					refreshAllCompCols(Filter * f);
			void					updateComputedColumnDependenciesForAnalysis(int analysisId, const stringset & usedVariables);
			void					computedColumnSucceeded(int dataSetId, QString columnName, QString warning, bool dataChanged);
			void					computedDataSetSucceeded(int dataSetId, QString warning, bool dataChanged);
			void					initializeComputedDatasets();
			
signals:
			void					dataSetCreated(int dataSetId);
			void					dataSetRemoved(int dataSetId);
			void					dataSetTitleChanged(int dataSetId);
			///< relay of Filter::nameChanged for every named filter of every
			///  loaded dataset (DataSet::workspace() is the hub) - the invalidation hook for
			///  editors holding qualified references to that filter BY NAME: a renamed
			///  referenced filter invalidates them (loudly) without any edit on their side.
			void					filterNameChanged(int dataSetId);
			void					filterByNameDone(int dataSetId, const QString & name, const QString & error);
			void					dataModeChanged(bool dataMode);
			void					showRSyntaxChanged(bool showIt);
			void					shownDataSetChanged(DataSet * dataSet);
			void					shownFilterChanged();
			void					datasetChanged(				int						dataSetId,
																QStringList				changedColumns,
																QStringList				missingColumns,
																QMap<QString, QString>	changeNameColumns,
																bool					rowCountChanged,
																bool					hasNewColumns); 
			void					labelsReordered(			QString columnName);
			void					labelFilterChanged();
			QString					askPassword(	QString title, QString message);
			bool					showYesNo(		QString title, QString message);
			void					allFiltersReset();
			void					showWarning(						QString title, QString msg);
			void					descriptionChanged();
			//dataFileChanged, dataFileSynchChanged and manualEditsChanged are relayed from the *shown*
			//dataset only (see setShownDataSet), so consumers can watch "the data" without having to
			//re-hook on every dataset switch themselves.
			void					dataFileChanged();
			void					databaseJsonChanged();
			void					dataFileSynchChanged();
			void					manualEditsChanged();
			void					dataTimestampChanged();
			void					columnsLabelFilteredCountChanged();
			void					refreshAllAnalyses(Filter * f);
			void					runComputedColumn(int dataSetid, QString columnName, QString code, enum columnType columnType);
			void					runComputedDataSet(int dataSetid, QString code, int defaultInputFilterId);
			void					sendFilter(			int dataSetID, const QString & generatedFilter, const QString & filter);
			void					sendFilterByName(	int dataSetID, const QString & name, const QString & module = "*");
			void					filtersCountChanged();
void					enableModified();
			void					shownColumnChanged();
			void					checkForDependentAnalyses(Column * column);
			void					showAnalysis(			int			analysisId);
			void					emptyValuesChanged();
			void					inputFilterDropDownListChanged();
			void					dataSetFilterDropDownListChanged();
			
	
			
private:
	std::map<int,DataSet*>			_dataSets;
	DataSet						*	_shownDataSet			= nullptr;
	VariableInfo				*	_varInfo				= nullptr;
	///Shared builder for inputFilterDropDownList()/dataSetFilterDropDownList(); the filter of the dataset
	///with excludeDataSetId (-1 for none) is left out.
	QVariantList					_filterDropDownList(int excludeDataSetId) const;
	bool							_showRSyntax			= false,
									_dataMode				= false,
									_inRefresh				= false; //instance flag (not static): works across Workspace instances
	static Workspace			*	_singleton;

};

#endif // WORKSPACE_H
