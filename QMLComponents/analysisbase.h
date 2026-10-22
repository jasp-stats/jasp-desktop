#ifndef ANALYSISBASE_H
#define ANALYSISBASE_H

#include <QObject>
#include <QPointer>
#include <json/json.h>
#include "controls/jaspcontrol.h"
#include "boundcontrols/boundcontrol.h"
#include "variableinfo.h"
#include "appinfo.h"


class AnalysisForm;
class DataSet;
class Filter;

// the analysis IS a VariableInfoProvider - the AnalysisBase analogue of
// Filter::columnEncoder() (CommonData/filter.cpp) - over the form's selection-host registry
// (AnalysisForm::selectionHostControls):
//
//   - every registered host is asked for its currently selected filter through the virtual
//     JASPControl::selectedSelectionFilter() PER USE. Nothing is ever cached (not a ColumnEncoder*,
//     not even a Filter*): a Filter's encode handle lazy-attaches to the filter's final rowid and
// is dropped when the filter dies, and hosts come and go with the QML tree.
//   - exactly ONE selected filter       ⟹ the provider IS that filter's namespace: provideInfo/
//     absorbInfo/providerModel/columnEncoder delegate straight to it (columnEncoder() = the live
//     Filter's own view handle, resolved at call time).
//   - NO host holds a selection          ⟹ fallback to the analysis' own filter (_filter, the
//     FilterMenuButton one), and to nothing at all when the analysis has none either.
//   - MORE THAN ONE distinct filter      ⟹ MERGED namespace (union view over the selected filters):
//       * VariableNames is the union of the hosts' names, one entry per distinct selected filter
//         that holds the column - same-name columns coming through different filters COEXIST
// (they are different namespaces; the terms' filterId disambiguates them
// downstream),
//       * every other request scans the hosts in registration order and answers with the first
//         hit (per-name) or the first host (dataset-wide),
//       * columnEncoder() and providerModel() return nullptr: there is no single namespace, and
//         pretending otherwise is exactly the phantom-slice disease this plan kills.
//     Consumers that need a SINGLE namespace - a legacy run, an encoder handle, one model - must
//     not use the merged view: they use the analysis' own filter (filter()/its columnEncoder()) or
//     one specific host (AnalysisForm::selectionHostControls() → selectedSelectionFilter()), and
//     they can detect the merged case with selectionNamespaceIsMerged() / selectionNamespaceFilterCount().
//
// selectionNamespaceChanged() is emitted whenever the aggregate could have changed: a host
// registers or unregisters (AnalysisForm::selectionHostsChanged) or a host's selected filter
// moves (AnalysisForm::selectionHostFilterChanged). The per-control resolution walk was deleted
// with controls resolve locally through their own selection host
// or the analysis-level VariableInfo (JASPControl::resolvedVarInfo); this provider is the
// analysis-wide endpoint.
class AnalysisBase : public QObject, public VariableInfoProvider
{
	friend class TestAll; ///< simulate an attached+initialized form (no QML controls in the C++ harness) to pin the pre-attach deferral vs the attached read
	Q_OBJECT
	QML_ELEMENT

	Q_PROPERTY(QQuickItem		*	formItem				READ formItem										NOTIFY formItemChanged			)
	Q_PROPERTY(QString				qmlError				READ qmlError			WRITE setQmlError			NOTIFY qmlErrorChanged			)
	Q_PROPERTY(Filter * filter		READ filter								NOTIFY filterChanged) //Select filter by changing filterName
	Q_PROPERTY(QString	filterName	READ filterName							NOTIFY filterChanged)
	Q_PROPERTY(int		filterId	READ filterId		WRITE setFilterId	NOTIFY filterChanged)
	Q_PROPERTY(QString	dataSpec	READ dataSpec							NOTIFY dataSpecChanged)
	///< Multi-dataset aware analyses let the user pick another dataset/filter per selection (see
	///< VariablesForm::dataSetSelection), and their variable options carry dataSetId/filterId provenance
	///< in their .meta. Desktop::Analysis derives this from the module entry; the base stores what is
	///< set here (used by the dummy analysis of R-syntax / unit-test mode).
	Q_PROPERTY(bool		multiDataSetAware READ multiDataSetAware WRITE setMultiDataSetAware NOTIFY multiDataSetAwareChanged)
	

public:
	explicit AnalysisBase(QObject *parent = nullptr);
	AnalysisBase(QObject *parent, AnalysisBase* duplicateMe);

	virtual				bool				isOwnComputedColumn(const std::string &col)					const	{ return false; }
	virtual				void				refresh()															{}
	virtual				void				run()																{}
	virtual				void				reloadForm()														{}
	virtual				void				exportResults()														{}
	virtual				bool				isDuplicate()												const	{ return false;				}
	virtual				bool				wasUpgraded()												const	{ return false;				}
	virtual				bool				needsRefresh()												const	{ return false;				}
	virtual				const std::string   module()													const	{ return emptyString;		}
	virtual				const std::string & name()														const	{ return emptyString;		}
	virtual				const std::string & title()														const	{ return emptyString;		}
	virtual				const std::string & titleDefault()												const	{ return emptyString;		}
	virtual				void				setTitle(const std::string& titel)									{}
	virtual				void				preprocessMarkdownHelp(const QString& md)					const	{}
	virtual				QString				helpFile()															{ return "";				}
	virtual				const stringvec   & upgradeMsgsForOption(const std::string& name)				const	{ return emptyStringVec;	}
	virtual				const Json::Value & resultsMeta()												const 	{ return Json::Value::null;	}
	virtual				const Json::Value & getRSource(const std::string& name)							const 	{ return Json::Value::null;	}
	virtual				void				initialized(AnalysisForm* form, bool isNewAnalysis)					{}
	virtual				std::string			qmlFormPath(bool addFileProtocol = true,
											bool ignoreReadyForUse = false)								const;
	virtual Q_INVOKABLE	QString				helpFile()													const	{ return ""; }
	virtual Q_INVOKABLE void				createForm(QQuickItem* parentItem=nullptr);
	virtual				void				destroyForm();
	virtual				bool				isColumnFreeOrMine(const QString & columnName)				const	{ return false; }
	virtual				DataSet *			dataSet()													const	{ return nullptr; }

	// ---- VariableInfoProvider over the selection-host registry -------------
	// See the class doc above for the one/many/none semantics; every access re-resolves the live
	// filters through the form's registry, nothing is cached.
	QVariant								provideInfo(varInfoType info, const QString &name = "", int row = 0)	const override;
	bool									absorbInfo(	varInfoType info, const QString &name, int row, QVariant value)			override;
	QAbstractItemModel					*	providerModel()																							override;
	ColumnEncoder							*	columnEncoder()																							override;

	///< The live, deduplicated (registration-order) filters the form's selection hosts hold right
	///  now, read through JASPControl::selectedSelectionFilter(). Empty when no host holds one -
	///  the provider's fallback (analysis' own filter) takes over then.
	QList<Filter *>							selectedSelectionFilters() const;
	///< True when more than one distinct filter is selected across the hosts: the merged case (see
	///  the class doc - consumers needing a single namespace must not use the merged view).
	Q_INVOKABLE bool						selectionNamespaceIsMerged() const;
	///< The distinct selected filter ids in registration order ([] = fallback to the analysis'
	///  own filter). Introspection/QML surface of selectedSelectionFilters().
	Q_INVOKABLE QVariantList				selectionNamespaceFilterIds() const;
	///< The provider's name universe: single namespace = that filter's names; merged = the union
	/// with one entry per distinct source filter (collisions COEXIST).
	Q_INVOKABLE QStringList					selectionNamespaceVariableNames() const;

	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue)													const	{ return defaultValue;		}
	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue, const QString& module, const QString& analysis)	const	{ return defaultValue;		}
	virtual bool				optionLocked(const QString& name)																				const	{ return false; };
	virtual	const Version	  &	moduleVersion()																									const	{ return AppInfo::version;	}

	// ---- the components own the state, JSON is generated on demand --------
	// The mirror and its whole reader/writer API are DELETED here:
	// the run request, save, RPC report, QML options() and script rendering all GENERATE from the
	// controls (createOptionsJson below), and the one value home is BoundControlBase._boundValue.
	// What survives of "push a tree in" is the explicit pending-options parking below.

	// ---- PENDING pre-attach options (setAnalysisUp's bootstrap, as an
	// explicit small member - the mirror's one sanctioned reader, never read for behaviour) -----
	///< Park options that arrive before a form has attached (file load, RPC create, QML
	///  setOptions, the downstream-widget fallback). setAnalysisUp consumes the parking lot ONCE
	///  through bindTo and it is then discarded; no run/save/dataSpec path ever reads pending
	///  JSON (pinned by testPreAttachOptionsParkAndAreNeverReadForBehaviour).
	///  createdWithOptions marks the parking as what the analysis was CONSTRUCTED with (file
	///  load / RPC create / duplication) - see createdWithOptions().
	void				setPendingOptions(const Json::Value & options, bool wasCreatedByOptions = false);
	///< Take and clear the parked tree: the single sanctioned consumption point
	///  (AnalysisForm::setAnalysisUp).
	Json::Value			takePendingOptions();
	///< Peek accessors (tests/introspection only - behaviour must go through takePendingOptions
	///  at attach or through the components; neither of these may feed a run/save/dataSpec path).
	Q_INVOKABLE bool	hasPendingOptions()	const	{ return _hasPendingOptions;	}
	const Json::Value &	pendingOptions()		const	{ return _pendingOptions;		}
	///< True when this analysis was CREATED with options (file load, RPC create with options,
	///  duplication): the explicit successor of the old mirror-sized new-analysis heuristic.
	bool				createdWithOptions()	const	{ return _createdWithOptions;	}

	///< strip the transient `.filterIds` sidecar from a generated Engine tree for
	///  consumers outside the run pipeline (RPC response["options"], QML options(), script
	///  rendering): provenance never leaves the desktop; `.meta` stays as generated
	///  (send-time keys only - dataSetId/filterId never enter it).
	static void			stripTransientSidecar(Json::Value & options);

	// ---- JSON generated on demand from the components ---------------------
	// The mirror era is over: these generators are the ONLY options tree. Engine = today's
	// R contract + send-time `.meta` (never dataSetId/filterId) + the transient `.filterIds`
	// sidecar; Storage = clean typed state (the preview shape).
	///< Generate the whole options tree from component state (delegates to the AnalysisForm; an
	///< analysis without a form has no state to generate from and returns an empty object).
	Json::Value				createOptionsJson(BoundControl::JsonFormat format);
	///< QML/test view of createOptionsJson, canonical (key-sorted) JSON string:
	///< format 0 = Engine, 1 = Storage.
	Q_INVOKABLE QString		createOptionsJsonAsJson(int format);

	///< the ONE path implementation for a nested (row-control) option location
	///< (the generators' own placement walk, sharing the row-chain semantics the mirror's lookup
	///< used to provide). parentKeys describe the row chain; parentNames collects the enclosing option names on
	///< the found path (what send-time `.meta` / `.filterIds` are nested under), found says the
	///< chain resolved, createAnyway materialises the row objects for a control whose parent option
	///< is generated earlier in the same pass.
	static Json::Value	&	resolveOptionParent(Json::Value & root,
												 const QVector<JASPControl::ParentKey> & parentKeys,
												 QVector<std::string> & parentNames, bool & found, bool createAnyway = false);
	///< place one option's meta node in a meta tree at the same parent path its value
	///< sits at (the mirror's setBoundValue `.meta` nesting, shared with the generators' `.meta` and
	///< `.filterIds` trees so all three agree on placement).
	static void				placeOptionMeta(Json::Value & metaRoot, const QVector<std::string> & parentNames,
											  const std::string & name, const Json::Value & meta);
	///< like placeOptionMeta, but for the transient `.filterIds` sidecar: a bare
	///< int/array own-node colliding with named row-child nodes is demoted into the container's
	///< reserved "$" member (grammar: ColumnEncoder::sidecarOwnFilterId) instead of tripping
	///< jsoncpp's objectValue assert. Non-colliding placements stay byte-identical to
	///< placeOptionMeta's.
	static void				placeOptionFilterIds(Json::Value & filterIdsRoot, const QVector<std::string> & parentNames,
												 const std::string & name, const Json::Value & filterIds);



						QQuickItem		  *	formItem()													const;

						const QString	  &	qmlError()													const;
						void				setQmlError(const QString &newQmlError);
						void				sendRScript(const QString & script, const QString & controlName, bool whiteListedVersion)		{ emit sendRScriptSignal(script, controlName, whiteListedVersion, tq(module())); }
						void				sendFilter(	const QString & name)																{ emit sendFilterSignal(name, tq(module())); }
							
						Filter			*	filter() const;
						
						///Whether this analysis operates on the given dataset. An analysis without an explicit dataset
						///binding (e.g. reports, or before a dataset is selected) is treated as using any dataset.
						bool				usesDataSet(int dataSetId)						const;
	virtual				bool				multiDataSetAware()								const	{ return _multiDataSetAware;		}
	void					setMultiDataSetAware(bool multiDataSetAware);
	///< What this analysis runs on, computed ON DEMAND from component state: the
	///< typed filterIds the bound controls generate (their `.filterIds` sidecar state), the
	///< registered selection hosts' filters, and the analysis' own filter - mapped to the
	///< dataSetId -> filterId pairs they live in. The primary dataset/filter of the analysis
	///< itself is included; every consumer dedups against it anyway. Defers (answers {}) until
	///< the form has attached - pending JSON is never read for behaviour.
	std::map<int, int>	referencedDataSets()												const;
	///< Let consumers re-read dataSpec()/referencedDataSets() (emits dataSpecChanged): used by
	///< AnalysisForm::setAnalysisUp once attach has produced real component state.
	void				notifyDataSpecChanged();
	///< the pure change-notification tail of the deleted mirror write - called by
	///< BoundControlBase::writeCurrentValue for every committed option value: emits
	///< boundValuesChanged (rSyntax text, results-JS) and, for aware analyses, dataSpecChanged
	///  (a changed value can change what the analysis runs on - the generated sidecar moved).
	///  Gated on the form being initialized, exactly like the mirror's setBoundValue was.
	void				notifyOptionsChanged();

						QString				filterName()	const;
						int					filterId()		const;
						void				setFilterId(int filterId);

						///The dataset and/or filter this analysis runs on, but only insofar as they actually tell it
						///apart from the others: empty when there is but a single dataset holding a single filter.
						///This is a read-only decoration of the title and never becomes part of the title itself.
						QString				dataSpec()		const;

						bool				isAnnotated()		const	{ return _isAnnotated; }
						void				setIsAnnotated(bool isAnnotated);
	


public slots:
	virtual void	boundValueChangedHandler()																	{}
	virtual void	requestColumnCreationHandler(			const std::string & columnName, columnType colType)	{}
	virtual void	requestComputedColumnCreationHandler(	const std::string & columnName)						{}
	virtual void	requestComputedColumnDestructionHandler(const std::string & columnName)						{}
	virtual void	onUsedVariablesChanged()																	{}
	///< The form's selection-host registry changed (host registered/unregistered) or a host moved
	///  to another filter: re-emits selectionNamespaceChanged() (the merged-namespace signal of the
	/// VariableInfoProvider) and, for aware analyses, dataSpecChanged() - the
	///  analysis-wide view of "what this analysis runs on" may have moved even when no option
	///  value changed (a same-dataset host switch rewrites no value, but it restamps provenance and
	/// can change the referenced datasets). Wired by AnalysisForm at pairing.
	void			handleSelectionNamespaceMaybeChanged();


signals:
	void			multiDataSetAwareChanged();
	void			sendRScriptSignal(QString script, QString controlName, bool whiteListedVersion, QString module);
	void			sendFilterSignal( QString  name,  QString module);
	void			formItemChanged();
	void			qmlErrorChanged();
	void			boundValuesChanged();
	void			filterChanged(Filter * f);
	void			dataSpecChanged();
	///< the aggregate over the selection hosts (this class' provider namespace)
	///< may have changed - consumers re-read it through selectedSelectionFilters()/provideInfo(),
	///< never cache it.
	void			selectionNamespaceChanged();


protected:
	void			connectDataSpecChanges();


	AnalysisForm*	_analysisForm		= nullptr;
	QQuickItem	*	_parentItem			= nullptr;
	QString			_qmlError;
	bool			_isAnnotated		= false;
	bool			_multiDataSetAware	= false;	///< See the multiDataSetAware Q_PROPERTY above; Desktop::Analysis pushes its module entry's value in via syncMultiDataSetAwareness()
	//Guarded pointers: a Filter/DataSet is owned by a DataSet/Workspace that may be destroyed (e.g.
	//multi-dataset teardown) while the analysis lives on; the guard auto-nulls on destruction so the
	//analysis never dereferences freed memory. _filterDataSet also derives from the (possibly null)
	//_filter instead of being tracked separately on dataset teardown.
	QPointer<Filter>				_filter				;
	QPointer<DataSet>				_filterDataSet		;

private:
	// the PENDING pre-attach parking lot (setAnalysisUp consumes it once through
	//bindTo; nothing else reads it) and the explicit created-with-options flag that replaces the
	//old mirror-sized new-analysis heuristic. The mirror itself is
	//DELETED: components own the state, JSON is generated on demand.
	Json::Value		_pendingOptions		= Json::objectValue;
	bool			_hasPendingOptions	= false,
					_createdWithOptions	= false;
	
protected:
	static const std::string	emptyString; ///< Otherwise we return references to a temporary object (std::string(""))
	static const stringvec		emptyStringVec;
};

#endif // ANALYSISBASE_H
