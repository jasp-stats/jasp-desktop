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

// mds-plan C10/D2.4: the analysis IS a VariableInfoProvider - the AnalysisBase analogue of
// Filter::columnEncoder() (CommonData/filter.cpp) - over the form's selection-host registry
// (AnalysisForm::selectionHostControls, mds-plan D2.3):
//
//   - every registered host is asked for its currently selected filter through the virtual
//     JASPControl::selectedSelectionFilter() PER USE. Nothing is ever cached (not a ColumnEncoder*,
//     not even a Filter*): a Filter's encode handle lazy-attaches to the filter's final rowid and
//     is dropped when the filter dies (mds-plan D1.2), and hosts come and go with the QML tree.
//   - exactly ONE selected filter       ⟹ the provider IS that filter's namespace: provideInfo/
//     absorbInfo/providerModel/columnEncoder delegate straight to it (columnEncoder() = the live
//     Filter's own view handle, resolved at call time).
//   - NO host holds a selection          ⟹ fallback to the analysis' own filter (_filter, the
//     FilterMenuButton one), and to nothing at all when the analysis has none either.
//   - MORE THAN ONE distinct filter      ⟹ MERGED namespace (union view over the selected filters):
//       * VariableNames is the union of the hosts' names, one entry per distinct selected filter
//         that holds the column - same-name columns coming through different filters COEXIST
//         (they are different namespaces, mds-plan C3; the terms' filterId disambiguates them
//         downstream, mds-plan C9/C13),
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
// moves (AnalysisForm::selectionHostFilterChanged). The resolution walk (JASPControl::
// effectiveVarInfo) stays the per-control mechanism until Phase 4 replaces it with term-carried
// provenance; this provider is the analysis-wide endpoint Phase 3/4 consumers switch to.
class AnalysisBase : public QObject, public VariableInfoProvider
{
	friend class TestAll; ///< mds-plan D3.5: simulate an attached+initialized form (no QML controls in the C++ harness) to pin the pre-attach deferral vs the attached read
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

	// ---- VariableInfoProvider over the selection-host registry (mds-plan C10/D2.4) -------------
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
	///  with one entry per distinct source filter (collisions COEXIST, mds-plan C3/C9).
	Q_INVOKABLE QStringList					selectionNamespaceVariableNames() const;

	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue)													const	{ return defaultValue;		}
	virtual QVariant			getConstant(const QString& key, const QVariant& defaultValue, const QString& module, const QString& analysis)	const	{ return defaultValue;		}
	virtual bool				optionLocked(const QString& name)																				const	{ return false; };
	virtual	const Version	  &	moduleVersion()																									const	{ return AppInfo::version;	}

						const Json::Value &	boundValues()												const	{ return _boundValues;		}
	///< The whole bound-value tree as a JSON string: a QML/inspectable view of exactly what the
	///< controls write (used by the dataset-selection tests to assert option payloads).
	Q_INVOKABLE QString		boundValuesAsJson()																const;
						const Json::Value &	boundValue(const std::string& name,
														 const QVector<JASPControl::ParentKey>& parentKeys = {});

						void				setBoundValue(const std::string& name, const Json::Value& value, const Json::Value& meta, const QVector<JASPControl::ParentKey>& parentKeys = {});
						void				setBoundValues(const Json::Value& boundValues);
						const Json::Value	optionsMeta()												const	{ return _boundValues.get(".meta", Json::nullValue);	}
						void				clearBoundValues()													{ _boundValues.clear();		}

	// ---- mds-plan C11/D3.1: JSON generated on demand from the components ---------------------
	// Until the consumer flip (D3.3/D3.5) the _boundValues mirror above stays authoritative for
	// the run/save/dataSpec paths; these generators are the Phase-3 replacement being proved
	// equal to it. They walk the form's registered controls per their bindTo paths (no stored
	// tree): Engine = today's R contract + send-time `.meta` (C8, minus dataSetId/filterId) + the
	// transient `.filterIds` sidecar (D3.2); Storage = clean typed state (D5.1 preview).
	///< Generate the whole options tree from component state (delegates to the AnalysisForm; an
	///< analysis without a form has no state to generate from and returns an empty object).
	Json::Value				createOptionsJson(BoundControl::JsonFormat format);
	///< QML/test view of createOptionsJson, same canonical writer as boundValuesAsJson:
	///< format 0 = Engine, 1 = Storage.
	Q_INVOKABLE QString		createOptionsJsonAsJson(int format);

	///< mds-plan D3.1: the ONE path implementation for a nested (row-control) option location,
	///< extracted from _getParentBoundValue so the _boundValues mirror and the generators place
	///< values at byte-identical paths. parentKeys describe the row chain (see _getParentBoundValue);
	///< parentNames collects the enclosing option names on the found path (what send-time `.meta` /
	///< `.filterIds` are nested under), found says the chain resolved, createAnyway materialises the
	///< row objects for a control whose parent option is generated earlier in the same pass.
	static Json::Value	&	resolveOptionParent(Json::Value & root,
												 const QVector<JASPControl::ParentKey> & parentKeys,
												 QVector<std::string> & parentNames, bool & found, bool createAnyway = false);
	///< mds-plan D3.1: place one option's meta node in a meta tree at the same parent path its value
	///< sits at (the mirror's setBoundValue `.meta` nesting, shared with the generators' `.meta` and
	///< `.filterIds` trees so all three agree on placement).
	static void				placeOptionMeta(Json::Value & metaRoot, const QVector<std::string> & parentNames,
											  const std::string & name, const Json::Value & meta);
	///< mds-plan D3.3: like placeOptionMeta, but for the transient `.filterIds` sidecar: a bare
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
	///< The dataSetId -> filterId pairs referenced through the .meta of the bound values (see
	///< ColumnEncoder::collectDataSetIdsFromMeta); the primary dataset/filter of the analysis itself is
	///< not necessarily part of it, that comes from filter()/dataSet(). Defers (answers {}) until
	///< the form has attached - pending JSON is never read for behaviour (mds-plan D3.5/C12).
	std::map<int, int>	referencedDataSets()												const;
	///< Let consumers re-read dataSpec()/referencedDataSets() (emits dataSpecChanged): used by
	///< AnalysisForm::setAnalysisUp once attach has produced real component state (mds-plan D3.5).
	void				notifyDataSpecChanged();

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
	///  VariableInfoProvider, mds-plan C10/D2.4) and, for aware analyses, dataSpecChanged() - the
	///  analysis-wide view of "what this analysis runs on" may have moved even when no option
	///  value changed (a same-dataset host switch rewrites no value, but it restamps provenance and
	///  can change the referenced datasets, mds-plan C3/M5). Wired by AnalysisForm at pairing.
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
	///< mds-plan C10/D2.4: the aggregate over the selection hosts (this class' provider namespace)
	///< may have changed - consumers re-read it through selectedSelectionFilters()/provideInfo(),
	///< never cache it.
	void			selectionNamespaceChanged();


protected:
	Json::Value&	_getParentBoundValue(const QVector<JASPControl::ParentKey> & parentKeys, QVector<std::string>& parentNames, bool & found, bool createAnyway = false);
	std::string		_displayParentKeys(const QVector<JASPControl::ParentKey> & parentKeys) const;
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
	Json::Value		_boundValues		= Json::objectValue,
					_orgBoundValues		= Json::objectValue;
	
protected:
	static const std::string	emptyString; ///< Otherwise we return references to a temporary object (std::string(""))
	static const stringvec		emptyStringVec;
};

#endif // ANALYSISBASE_H
